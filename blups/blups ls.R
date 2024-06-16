library(lme4)
library(dplyr)
#----------------------#
#       call data      #
#----------------------#
getwd()
setwd("G:/R CLASS practice/LS")
datak<-read.csv("blup4.csv",header =T)

str(datak)
head(datak)
tail(datak)

#-------------------------------------------------
rep=as.factor(datak$rep)
Year=as.factor(datak$Year)
Location=as.factor(datak$Location)
block=as.factor(datak$block)
plot=as.factor(datak$plot)
env=as.factor(datak$env)
G=as.factor(datak$G)
Y2 = as.numeric(datak$Y)
ph = as.numeric(datak$ph)
ssT = as.numeric(datak$ssT)
LOC=as.factor(datak$Location)
YEAR=as.factor(datak$Year)
REP=as.factor(datak$rep)
LINE=as.factor(datak$G)
#-----------------------------------------------
hist(Y2, col="gold")
boxplot(Brix~Loc, xlab="Location", ylab="Degrees Brix", main="Degrees Brix by Location", col="pink")

## BLUPS
# fit the model
brixmodel = lmer(Y2~ (1|LINE) + (1|LOC) + (1|YEAR) + (1|LINE:LOC) + (1|LINE:YEAR))


# estimate BLUPS
brixblup = ranef(brixmodel)
# look at output structure
str(brixblup)
# extract blup for line
brixlineblup = brixblup$LINE
# see the structure of the blup for each line
str(brixlineblup)
# save the brixlineblup output to a separate .csv file
write.csv(brixlineblup, file="BrixLineBLUPS.csv")

## Creating plots with the BLUPs
# Create a numeric vector with the BLUP for each line
LINEBLUP = brixlineblup[,1]
# Create a histogram with the BLUP for each line
hist(LINEBLUP, col="green")

## Compare BLUP to line averages on a scatterplot
lmean = tapply(Y2, LINE, na.rm=T, mean)
plot(LINEBLUP, lmean, col="black")

















