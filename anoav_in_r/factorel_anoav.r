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

factorel.data<-read.csv("factoril.csv",header=TRUE,
                   colClasses = c("factor", "factor", "factor", "numeric","numeric"))

CRDfact.data<-read.csv("CRDfact.csv",header=TRUE,
                   colClasses = c("factor", "factor", "numeric"))


summary(factorel.data)
summary(CRDfact.data)

####  anova  ####
#-------------------#
#      anova        #
#-------------------#
#aov and lm 
fatoril.rb<- aov(Y~ A + B + AB + R, data= factorel.data)

summary(fatoril.rb)

CRD.fact<- aov(Y~ A+ B + A*B, data= CRDfact.data)

model.tables(CRD.fact,"effects")
summary(CRD.fact)

###  best model  ####
#-------------------#
#    best model     #
#-------------------#
#library(AICcmodavg)
#we should make all this model after that you can test them
model.set <- list(one.way, two.way, interaction, blocking)
model.names <- c("one.way", "two.way", "interaction", "blocking")

aictab(model.set, modnames = model.names)

#-------------------#
#   Tukey’s HSD     #
#-------------------#
tukey.CRD.fact<-TukeyHSD(CRD.fact)

tukey.CRD.fact

#### results in a graph ####
#-------------------------#
#   results in a graph    #
#-------------------------#
tukey.plot.CRD.fact<-aov(Y~A*B,data= CRDfact.data)
tukey.plot.CRD.test<-TukeyHSD(tukey.plot.CRD.fact)
plot(tukey.plot.CRD.test, las = 1)
