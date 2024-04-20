library(ggplot2)
library(dplyr)
library(ggmosaic)
library(tidyverse)
theme_set(theme_minimal(20))

#read data
diet = read.csv('./Diet.csv')
head (diet)
#-------------------------------------------------------------------------------
#a
#preprocessing
diet <- diet %>%
  mutate(Diet = recode_factor(Diet, `1` = "A", `2` = "B", `3` = "C")) %>%
  mutate_at(c("gender"),
            ~ recode_factor(.x, `0` = "Female", `1` = "Male"))


diet$weight.loss <- diet$pre.weight - diet$weight6weeks
diet$Diet  <- factor(diet$Diet,levels=c("A","B","C"))
diet$gender      <- factor(diet$gender,levels=c("Male","Female"))



#draw bpx plot
boxplot(diet$weight.loss  ~ diet$Diet, ylab = "Weight loss (kg)", xlab = "Diet type",
        main = "side-by-side boxplots") 
#-------------------------------------------------------------------------------
#b
fisher  = aov(weight.loss~Diet,data=diet)
#-------------------------------------------------------------------------------
#c
summary(fisher)
#-------------------------------------------------------------------------------
#d
gp <- unique(diet$Diet)

A <- diet$weight.loss[which(diet$Diet==gp[1])]
B <- diet$weight.loss[which(diet$Diet==gp[2])]

diet.t.test <- t.test(A , B)
diet.t.test
diet.t.test$conf.int

#-------------------------------------------------------------------------------
#END

