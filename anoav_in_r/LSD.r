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

LSD.data<-read.csv("LSD.csv",header=TRUE,
                   colClasses = c("factor", "factor", "factor", "numeric"))
summary(LSD.data)

LSD<- aov(Y~ T + R + C, data= LSD.data)

summary(LSD)
