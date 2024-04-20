library(ggplot2)
library(dplyr)
library(ggmosaic)
library(tidyverse)
theme_set(theme_minimal(20))

# Read CSV into DataFrame
house = read.csv('./Houses.csv')
head (house)

bootstrap <- replicate (1000, sample (house$Area..Meter., replace=T))
mean <-mean(bootstrap)
SE <- sd(bootstrap)/sqrt (length (mean))

B = 1000
n = nrow(house)
boot.samples = matrix(sample(house$Area..Meter., size = B * n, replace = TRUE),B, n)
boot.statistics = apply(boot.samples, 1, mean)

require(ggplot2)
ggplot(data.frame(meanprice = boot.statistics),aes(x=meanprice)) +
  geom_histogram(binwidth=2,aes(y=..density..)) +
  geom_density(color="red")