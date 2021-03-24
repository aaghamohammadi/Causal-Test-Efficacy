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
labs <- levels(d$prj)

# scaled the explanatory variables in order to have mean zero and variance one.
d$exec <- log(d$exec + 1)
d$cover <- log(d$cover + 1)
d = ddply(d,c("prj"), transform, exec=scale(exec))
d = ddply(d,c("prj"), transform, cover=scale(cover))

S = d
m2 <- readRDS(file = "RQ2_m.rds")
m4 <- readRDS(file = "RQ4_m.rds")
post_m2 <- extract.samples(m2)
post_m4 <- extract.samples(m4)

df_post_m2 <- as.data.frame(post_m2$bExec)
colnames(df_post_m2) <- levels(d$prj)

df_post_m4 <- as.data.frame(post_m4$bCover)
colnames(df_post_m4) <- levels(d$prj)

precis(df_post_m2)
precis(df_post_m4)

precis(sort(df_post_m4$argparse4j) - sort(df_post_m2$argparse4j), prob=0.95)
precis(sort(df_post_m4$`assertj-core`) - sort(df_post_m2$`assertj-core`), prob=0.95)
precis(sort(df_post_m4$fess) - sort(df_post_m2$fess), prob=0.95)
precis(sort(df_post_m4$`joda-time`) - sort(df_post_m2$`joda-time`), prob=0.95)
precis(sort(df_post_m4$la4j) - sort(df_post_m2$la4j), prob=0.95)
precis(sort(df_post_m4$lang) - sort(df_post_m2$lang), prob=0.95)
precis(sort(df_post_m4$msg) - sort(df_post_m2$msg), prob=0.95)
precis(sort(df_post_m4$nodebox) - sort(df_post_m2$nodebox), prob=0.95)
precis(sort(df_post_m4$opennlp) - sort(df_post_m2$opennlp), prob=0.95)
precis(sort(df_post_m4$recast4j) - sort(df_post_m2$recast4j), prob=0.95)
precis(sort(df_post_m4$uaa) - sort(df_post_m2$uaa), prob=0.95)
precis(sort(df_post_m4$wire) - sort(df_post_m2$wire), prob=0.95)










