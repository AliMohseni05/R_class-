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

#if you want to skip some row you can use 'skip' and "" will be "NA" by 
#using na.strings = c("", "NA"))
data <- read.csv("path/to/your/file.csv", skip = 3, na.strings = c("", "NA"))

####  anova  ####
#-------------------#
#      anova        #
#-------------------#
#aov and lm 
CRD<- aov(yield ~ npk, data = crop.data)

summary(CRD)
model.tables(CRD,"mean")

#### homoscedasticity ####
#-------------------#
# homoscedasticity  #
#-------------------#
par(mfrow=c(2,2))
plot(CRD)
par(mfrow=c(1,1))
#### Tukey’s HSD ####
#-------------------#
#   Tukey’s HSD     #
#-------------------#
tukey.CRD<-TukeyHSD(CRD)
#TukeyHSD(x, which, ordered = FALSE, conf.level = 0.95, …)
#X:CRD
#which: npk

tukey.CRD

tukey.CRD.fact<-TukeyHSD(CRD.fact)

tukey.CRD.fact


#### results in a graph ####
#-------------------------#
#   results in a graph    #
#-------------------------#
tukey.plot.aov<-aov(yield ~ npk:leve, data=crop.data)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

tukey.plot.CRD.fact<-aov(Y~A*B,data= CRDfact.data)
tukey.plot.CRD.test<-TukeyHSD(tukey.plot.CRD.fact)
plot(tukey.plot.CRD.test, las = 1)

#### group labels ####
mean.yield.data <- crop.data %>%
  group_by(npk,leve) %>%
  summarise(
    yield = mean(yield)
  )
mean.yield.data$group <- c("a","b","b","b","b","c")

mean.yield.data

