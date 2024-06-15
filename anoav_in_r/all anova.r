
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

LSD.data<-read.csv("LSD.csv",header=TRUE,
                   colClasses = c("factor", "factor", "factor", "numeric"))

factorel.data<-read.csv("factoril.csv",header=TRUE,
                   colClasses = c("factor", "factor", "factor", "numeric","numeric"))

CRDfact.data<-read.csv("CRDfact.csv",header=TRUE,
                   colClasses = c("factor", "factor", "numeric"))

summary(crop.data)
summary(LSD.data)
summary(factorel.data)
summary(CRDfact.data)

####  colClasses  ####
system.time(largeData <- read.csv("huge-file.csv",
                                  header = TRUE,
                                  colClasses = c("character", "character", "complex", 
                                                 "factor", "factor", "character", "integer", 
                                                 "integer", "numeric", "character", "character",
                                                 "Date", "integer", "logical")))
#................
smallData <- read.csv("small-file.csv", 
                      header = TRUE,
                      colClasses=c("variableName"="character"))


> class(smallData$variableName)
#"character"
#................

#if you want skip some row you can use 'skip' and "" will be "NA" by 
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

RBD<- aov(yield ~ npk + block, data = crop.data)

summary(RBD)

LSD<- aov(Y~ T + R + C, data= LSD.data)

summary(LSD)

fatoril.rb<- aov(Y~ A + B + AB + R, data= factorel.data)

summary(fatoril.rb)

CRD.fact<- aov(Y~ A+ B + A*B, data= CRDfact.data)

model.tables(CRD.fact,"effects")

summary(CRD.fact)
#effects
#For type = "effects" give tables of the coefficients
#for each term, optionally with standard errors.
#means
#For type = "means" give tables of the mean response 
#for each combinations of levels of the factors in a term.
###  best model  ####
#-------------------#
#    best model     #
#-------------------#
#library(AICcmodavg)
#we should make all this model after that you can test them
model.set <- list(one.way, two.way, interaction, blocking)
model.names <- c("one.way", "two.way", "interaction", "blocking")

aictab(model.set, modnames = model.names)
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


two.way.plot <- two.way.plot +
  theme_classic2() +
  labs(title = "Crop yield in response to fertilizer mix and planting density",
      x = "Planting density (1=low density, 2=high density)",
      y = "Yield (bushels per acre)")

two.way.plot
#### other ####
#-------------------#
#      other        #
#-------------------#
y<-c(12,13,13,34)
h<-c(123,134,124,155)
length(y)
length(h)
g<-c("A","A","c","C")
yh<-cbind(y,h)
manova.yh<-manova(yh~g)
manova.yh
#x2 
o<-c(40,60)
e<-c(0.5,0.5)
chisq.test(o,p=e)
#p: p-valiue o== numeber e==persent 

# t.test
#two.sided
t<-c(12,13,13,34)
t2<-c(123,134,124,155)
t.test(t,t2,alternative = "two.sided",var.equal = F)
# var.equal = F for not joined 

#t.test
#one.way
A<-c(12,13,13,34)
t.test(A,y=NULL,alternative = "greater")
#greater or less 
#y=sd 
