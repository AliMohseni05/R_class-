#### library ####
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

####  call data  ####
#-------------------#
#     call data     #
#-------------------#
getwd()
setwd("G:/R CLASS practice")

crop.data <- read.csv("crop.data.csv", header = TRUE, 
                     colClasses = c("factor", "factor", "factor", "numeric"))
summary(crop.data)

RBD<- aov(yield ~ npk + block, data = crop.data)

summary(RBD)

####Plot the raw data ####
two.way.plot <- ggplot(crop.data, aes(x = leve, y = yield, group=npk)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))

two.way.plot

two.way.plot <- two.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.yield.data, aes(x=leve, y=yield))

two.way.plot
#
#Add the means and standard errors to the graph
#
two.way.plot <- two.way.plot +
  geom_text(data=mean.yield.data, label=mean.yield.data$group, vjust = -8, size = 5) +
  facet_wrap(~ npk)
two.way.plot
#
#Split up the data
#
