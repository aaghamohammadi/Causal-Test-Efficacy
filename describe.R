library(tidyverse)
library(rethinking)
library(psych)
library(plyr)

theme_set(theme_classic())

d <- read_csv("data.csv")

# convert **prj** column to factor
d <- d%>%mutate_if(is.character, as.factor)

str(d)
summary(d)
levels(d$prj)

S = filter(d, prj=="argparse4j")

summary(S)
skew(S$exec)
skew(S$cover)

S = filter(d, prj=="assertj−core")

summary(S)
skew(S$exec)
skew(S$cover)


S = filter(d, prj=="fess")

summary(S)
skew(S$exec)
skew(S$cover)

S = filter(d, prj=="joda-time")

summary(S)
skew(S$exec)
skew(S$cover)

S = filter(d, prj=="la4j")

summary(S)
skew(S$exec)
skew(S$cover)


S = filter(d, prj=="lang")

summary(S)
skew(S$exec)
skew(S$cover)


S = filter(d, prj=="msg")

summary(S)
skew(S$exec)
skew(S$cover)


S = filter(d, prj=="nodebox")

summary(S)
skew(S$exec)
skew(S$cover)


S = filter(d, prj=="opennlp")

summary(S)
skew(S$exec)
skew(S$cover)


S = filter(d, prj=="recast4j")

summary(S)
skew(S$exec)
skew(S$cover)


S = filter(d, prj=="uaa")

summary(S)
skew(S$exec)
skew(S$cover)


S = filter(d, prj=="wire")

summary(S)
skew(S$exec)
skew(S$cover)


summary(d)
skew(d$exec)
skew(d$cover)
