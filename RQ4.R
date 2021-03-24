# RQ4: What is the causal association between the explanatory variable **cover** and the outcome of interest?

library(tidyverse)
library(rethinking)
library(ggridges)
library(plyr)

theme_set(theme_classic())

d <- read_csv("data.csv")

# convert **prj** column to factor
d <- d%>%mutate_if(is.character, as.factor)

str(d)
summary(d)
levels(d$prj)

# scaled the explanatory variables in order to have mean zero and variance one.
d$exec <- log(d$exec + 1)
d$cover <- log(d$cover + 1)
d = ddply(d,c("prj"), transform, exec=scale(exec))
d = ddply(d,c("prj"), transform, cover=scale(cover))

S = d

# Prepare multilvel GLM (cover --> outcome)
dat_list <- list(
  killed = S$killed,
  cover = S$cover,
  prj = S$prj
)

m <- ulam(
  alist(
    killed ~ dbinom(1, p),
    logit(p) <- a[prj] +  bCover[prj] * cover,
    a[prj] ~ dnorm(0, 1),
    bCover[prj] ~ dlnorm(0, 1)
  ), data=dat_list, cmdstan = TRUE, chains=4, cores = 8, iter = 2000
)

saveRDS(m, file = "RQ4_m.rds")
m <- readRDS(file = "RQ4_m.rds")

# posterior check
precis(m, depth = 2,prob = 0.975)

means = aggregate(d[,2], list(d$prj), mean) # true mutation score

post <- extract.samples(m)
prob_a <- inv_logit(post$a)
labs <- levels(S$prj)

plot(precis(as.data.frame(prob_a), prob=0.975), xlim=c(0, 1), labels=labs, xlab="Mutation score")
points(means$x, y=seq(12,1), col=rangi2,lwd=2)

estimated_mean = apply(prob_a,2, mean)     
err = round(abs(estimated_mean - means$x),3)
as.data.frame(err)

resid_var <- var2(err)
outcome_var <- var2(means$x)
R2 = 1 - resid_var/outcome_var

# associations between **cover** and outcome
post <- extract.samples(m, n=10000)
prob_cover <- inv_logit(post$bCover)
df_prob_cover = as.data.frame(prob_cover)
colnames(df_prob_cover) <- levels(d$prj)
prob_cover_long = pivot_longer(
  data=df_prob_cover,
  cols=1:12
)

ggplot(prob_cover_long, aes(x=value,y=name, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.025, 0.975),
  ) +
  scale_fill_manual(
    name = "Probability", values = c("grey", "lightblue", "grey"),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")
  ) +
  xlab("Association") +
  ylab("") +
  theme(text = element_text(size=24))

precis(df_prob_cover, prob=0.95)

df_cover = as.data.frame(post$bCover)
colnames(df_cover) <- levels(d$prj)
precis(df_cover, prob = 0.95)

