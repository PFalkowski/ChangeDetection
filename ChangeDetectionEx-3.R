# functions
aggregate.expand = function(dv, f, fun = mean){
    res = aggregate(dv ~ f, FUN = fun)
    rownames(res) = as.character(res[,1])
    return(res[as.character(f), 2])
}

# configure R environment

packages.needed <- c("lme4", "ggplot2", "lattice", "rio", "lmtest", "rms")
new.packages <- packages.needed[!(packages.needed %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://r.meteo.uni.wroc.pl/')

library(scales)
library(zoo)
library(Matrix)
library(lme4)
library(ggplot2)
library(lattice)
library(rio)
library(lmtest)
library(rms)


options(max.print = 1000)

# read data

setwd("..\\OneDrive\\Repos\\Change Detection\\Data")
data <- read.csv("CD_ex3_RAWdata - Long.csv", header = TRUE)

# add variables
ScaleMin = 0
ScaleMax = 1
data$ScaledPAS = rescale(data$PAS, to=c(ScaleMin, ScaleMax))
data$ScaledSetsize = rescale(data$Setsize, to=c(ScaleMin, ScaleMax))
data$PositionRatio <-data$TargetPos / data$Setsize

# validate

#str(data)

TrialsByCondition = aggregate(ID ~ TypeOfChange * Memory, data, length)
TrialsByCondition

IDbyCorr = aggregate(Corr ~ ID, data, mean)
dotplot(ID ~ Corr, IDbyCorr)

PASbyID = aggregate(PAS ~ ID, data, mean)
dotplot(ID ~ PAS, PASbyID)

# Get Outliers
lowerBoundOutlier = .5
upperBoundOutlier = 0.95
CorrByConditionID = aggregate(Corr ~ ID + Memory + TypeOfChange, data, mean)
ggplot(CorrByConditionID, aes(Corr, TypeOfChange, Memory, colour=ID)) + 
  geom_line() + 
  geom_point() +
  geom_text(aes(label=ifelse(Corr<=lowerBoundOutlier | Corr>= upperBoundOutlier,as.character(ID),'')),hjust=0,vjust=1.5)


# Remove Outliers
outliersIDs = CorrByConditionID[CorrByConditionID$Corr <= lowerBoundOutlier | CorrByConditionID$Corr >= upperBoundOutlier, ]
data = data[!(is.element(data$ID, outliersIDs$ID)),]

# ANOVA

summary(aov(ScaledSetsize ~ TypeOfChange * Memory * PAS + Error(ID), data))

# GLMM

mf = glmer(Corr ~ TypeOfChange * Memory * PAS * (1|TrialsOrder) * (1|ID) , 
                    data, 
                    family = binomial, nAGQ = 0)
summary(mf)

# Ordered logit
attach(data)
X <- cbind(Setsize)
Y <- cbind(TypeOfChange, Memory, PAS, ID)
Xvar <- c("Type of change", "Memory", "PAS")


summary(X)
summary(Y)

table(Y)
ddist <- datadist(Xvar)
options(datadist = 'ddist')

ologit <- lrm(X ~ Y)
print(ologit)

fitted <- predict(ologit, newdata = data, type = "fitted.ind")
colMeans(fitted)