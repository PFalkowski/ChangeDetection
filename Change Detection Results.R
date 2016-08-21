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


options(max.print = 100)

# read data

setwd("..\\OneDrive\\Repos\\Change Detection")
data <- read.csv("CD_ex3_RAWdata - BetweenSubject.csv", header = TRUE)

# validate

nrow(data)
ncol(data)
str(data)
head(data)
tail(data)

BetweenSubjectTrials = aggregate(ID ~ Memory, data, length)
BetweenSubjectTrials 

WithinSubjectTrials = aggregate(ID ~ TypeOfChange, data, length)
WithinSubjectTrials 

res = aggregate(Corr ~ ID, data, length)
plot(sort(res$Corr))

res = aggregate(Corr ~ ID, data, mean)
dotplot(ID ~ Corr, res)

res = aggregate(PAS ~ ID, data, mean)
dotplot(ID ~ PAS, res)

# handle outliers

data$macc = aggregate.expand(data$Corr, data$ID)
potentialOutliers = data[data$macc < .5,]
nrow(potentialOutliers)

# ANOVA

summary(aov(Corr ~ Error(ID) + (TypeOfChange*Memory) * PAS, data))

# GLMM

mf = glmer(Corr ~ (1|ID) * TypeOfChange * Memory * PAS, 
                    data, 
                    family = binomial, 
                    control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(mf)

