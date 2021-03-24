# RQ1: What is the association between the explanatory variable **exec** and the outcome of interest?

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


# Prepare multilvel GLM (exec --> outcome)
dat_list <- list(
  killed = S$killed,
  exec = S$exec,
  prj = S$prj
)


m <- ulam(
  alist(
    killed ~ dbinom(1, p),
    logit(p) <- a[prj] +  bExec[prj] * exec,
    a[prj] ~ dnorm(0, 1),
    bExec[prj] ~ dlnorm(0, 1)
  ), data=dat_list, cmdstan = TRUE, chains=4, cores = 8, iter = 2000
)

saveRDS(m, file = "RQ1_m.rds")
m <- readRDS(file = "RQ1_m.rds")

# prior check
prior <- extract.prior(m,n=10000)
xseq = seq(-1, 1, length.out = 100)
theta <- inv_logit(prior$a + prior$bExec * xseq)
df = as.data.frame(theta[,1])
ggplot(df, aes(x=theta[,1])) + 
  geom_histogram(color="darkblue", fill="lightblue", binwidth = 0.1, alpha=0.9) +
  xlab("Mutation Score") +
  ylab("Count") +
  theme(text = element_text(size=16))

# posterior check
precis(m, depth = 2,prob = 0.975)

means = aggregate(d[,2], list(d$prj), mean) # true mutation score

post <- extract.samples(m)
prob_a <- inv_logit(post$a)
labs <- levels(S$prj)

plot(precis(as.data.frame(prob_a), prob=0.975), xlim=c(0, 1), labels=labs, main="Mutation score", cex=1.5, xlab="")
points(means$x, y=seq(12,1), col=rangi2,lwd=2)

estimated_mean = apply(prob_a,2, mean)     
err = round(abs(estimated_mean - means$x),3)
as.data.frame(err)

resid_var <- var2(err)
outcome_var <- var2(means$x)
R2 = 1 - resid_var/outcome_var

# associations between **exec** and outcome
post <- extract.samples(m)
prob_exec <- inv_logit(post$bExec)
df_prob_exec = as.data.frame(prob_exec)
colnames(df_prob_exec) <- levels(d$prj)
prob_exec_long = pivot_longer(
  data=df_prob_exec,
  cols=1:12
)

ggplot(prob_exec_long, aes(x=value,y=name, fill = factor(stat(quantile)))) +
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


df_exec = as.data.frame(post$bExec)
colnames(df_exec) <- levels(d$prj)
precis(df_exec, prob = 0.95)

