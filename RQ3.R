# RQ3: What additional information **exec**  does provide after knowing **cover** variable?

library(tidyverse)
library(rethinking)
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

S = filter(d, prj=="wire")
#S = sample_n(d, 50000)

dat_list <- list(
  killed = S$killed,
  exec = S$exec,
  cover = S$cover
)

cfm <- ulam(
  alist(
    killed ~ dbinom(1, p),
    logit(p) <- a + bExec * exec + bCover * cover,
    a ~ dnorm(0, 1),
    bExec ~ dlnorm(0, 1),
    bCover ~ dlnorm(0, 1),
    exec ~ dnorm(mu_exec, sigma_exec),
    mu_exec <- a_exec + b_exec_cover * cover,
    a_exec ~ dnorm(0, 1),
    b_exec_cover ~ dnorm(0, 1),
    sigma_exec ~ dexp(1)
  ), data=dat_list, cmdstan = TRUE, threads = 1, chains = 4, cores=8, iter = 2000
)

precis(cfm)

dat_list <- list(
  killed = S$killed,
  exec = S$exec
)

m <- ulam(
  alist(
    killed ~ dbinom(1, p),
    logit(p) <- a +  bExec * exec,
    a ~ dnorm(0, 1),
    bExec ~ dlnorm(0, 1)
  ), data=dat_list, cmdstan = TRUE, chains=4, cores=8, iter = 2000
)

precis(m)

sim_dat <- data.frame(exec=seq(from=-2, to=2, length.out=10), cover=0)
s <- sim(cfm, data=sim_dat, vars="killed")

sim_dat2 <- data.frame(exec=seq(from=-2, to=2, length.out=10))
s2 <- sim(m, data=sim_dat2, vars="killed")

df <- data.frame(x=sim_dat$exec,y1=colMeans(s),y2=colMeans(s2))

ggplot(df, aes(x)) +                    
  geom_line(aes(y=y1, colour="Controlled")) +  
  geom_line(aes(y=y2, colour="Uncontrolled")) + 
  scale_color_manual(values = c("lightblue", "black")) +
  ylim(0,1) +
  xlim(-2,2) +
  labs(x="Exec (standardized)", y="Mutation Score", color = "")


