# functions
aggregate.expand = function(dv, f, fun = mean){
    res = aggregate(dv ~ f, FUN = fun)
    rownames(res) = as.character(res[,1])
    return(res[as.character(f), 2])
}

# configure R environment

packages.needed <- c("lme4", "ggplot2", "lattice", "rio", "lmtest")
new.packages <- packages.needed[!(packages.needed %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://r.meteo.uni.wroc.pl/')


library(zoo)
library(Matrix)
library(lme4)
library(ggplot2)
library(lattice)
library(rio)
library(lmtest)


options(max.print = 1000)

# read data

setwd("..\\OneDrive\\Repos\\Change Detection")
data <- read.csv("CD_ex3_RAWdata - BetweenSubject.csv", header = TRUE)

# validate

#str(data)

TrialsByCondition = aggregate(ID ~ TypeOfChange * Memory, data, length)
TrialsByCondition

IDbyCorr = aggregate(Corr ~ ID, data, mean)
dotplot(ID ~ Corr, IDbyCorr)

PASbyID = aggregate(PAS ~ ID, data, mean)
dotplot(ID ~ PAS, PASbyID)

# Outliers

potentialOutliers = aggregate(Corr ~ ID * Memory * TypeOfChange, data, mean)


ggplot(potentialOutliers, aes(Corr, Memory, TypeOfChange, colour=ID)) + 
  geom_line() + 
  geom_point()
# ANOVA

summary(aov(Corr ~ Error(ID) + TypeOfChange*Memory * PAS, data))

# GLMM

mf = glmer(Corr ~ (1|ID) * (TypeOfChange * Memory * PAS, 
                    data, 
                    family = binomial, 
                    control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(mf)

