# setup
library(dplyr)
library(tidyr)
library(stats)
library(psych)
library(ggplot2)
library(tibble)


# read in data - switch this out with whatever directory structure you have!
d <- read.csv("us_run-02_2016-07-19_anonymized.csv")

# examine response times
qplot(d$rt, bins = 100) + 
  scale_x_log10(breaks = seq(0, 100, 1000)) +
  geom_vline(xintercept = 250, color = "red")

# filter out response times > 250ms
d0 <- d %>%
  filter(rt >= 250)

# make a dataframe appropriate for factor analysis:
# each row is a participant
# each column is an item
d1 <- d0 %>%
  select(capacity, responseNum, subid) %>%
  spread(capacity, responseNum)

# change subid into rowname instead of variable
d2 <- data.frame(d1[,-1], row.names = d1[,1])

# examine factor structure: how many factors to extract?
fa.parallel(d2, cor = "cor") # using Pearson correlations
# fa.parallel(d2, cor = "poly") # using polychoric correlations, takes forever but can be more appropriate depending on your response scale

# do factor analysis with maximal number of factors
fa(r = d2, # what is your dataset?
   nfactors = 13, # how many factors to extract?
   rotate = "none", # do you want to rotate the solution?
   fm = "minres", # what method of factor analysis do you want to use?
   cor = "cor") # what kind of correlations do you want to use?

# check out the rotated version
fa(r = d2, nfactors = 13, rotate = "varimax", fm = "minres", cor = "cor")

# extract fewer factors
fa(r = d2, nfactors = 3, rotate = "varimax", fm = "minres", cor = "cor")

# useful way to look at factor loadings
fa.sort(fa(d2, nfactors = 3, rotate = "varimax")$loadings[]) %>% View()
