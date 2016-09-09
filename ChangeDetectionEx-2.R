# configure R environment
source("../OneDrive/Repos/Change Detection/Helper.R")
#source("../Helper.R")
Packages = c("lme4", "ggplot2", "lattice", "rio", "lmtest", "rms")
WorkingDirectory = "../OneDrive/Repos/Change Detection/Data"
SetupEnvironment(workingDirectory = WorkingDirectory, requiredPackages = Packages)

# read data

data2 <- read.csv("CD_ex2_data_140t_25Ps.xlsx - CD_ex2_RAW.csv", header = TRUE)

# add variables

ScaleMin = 0
ScaleMax = 1
data2$ScaledPAS = rescale(data2$PAS, to=c(ScaleMin, ScaleMax))
data2$PositionRadians <-data2$TargetPos / 8



# Get Outliers By response Bias

responseBiasByCondition <- aggregate(Response ~  ConditionRecoded * ID, data2, mean)
responseBiasByID <- aggregate(Response ~ ID, data2, mean)
toRemove <- responseBiasByCondition[responseBiasByCondition$Response < .15, ]
data2 <- data2[!(is.element(data2$ID, toRemove$ID)), ]

responseSD <- sd(responseBiasHealthy$Response, na.rm = TRUE) 
responseMean <- mean(responseBiasHealthy$Response, na.rm = TRUE)

# Get Outliers By RT

averageResponseByID = aggregate(ResponseRT ~  ID * ConditionRecoded, data2, mean)
RTSD <- sd(averageResponseByID$ResponseRT, na.rm = TRUE) 
RTMean <- mean(averageResponseByID$ResponseRT, na.rm = TRUE)

summary(aov(ResponseRT ~  ID * ConditionRecoded, data2))

# Get Outliers By Corr
lowerBoundOutlier = .25
upperBoundOutlier = 1


CorrByConditionID = aggregate(Corr ~ ID * TypeOfChange, data2, mean)
outliersIDs = CorrByConditionID[CorrByConditionID$Corr < lowerBoundOutlier | CorrByConditionID$Corr > upperBoundOutlier, ]
outliersData = data2[(is.element(data2$ID, outliersIDs$ID)),]
outliersAnalysis = aggregate(Corr ~ ConditionRecoded * ID, outliersData, mean)

ggplot(CorrByConditionID, aes(Corr, TypeOfChange, Memory, colour=ID)) + 
  geom_line() + 
  geom_point() +
  geom_text(aes(label=ifelse(Corr<=lowerBoundOutlier | Corr>= upperBoundOutlier,as.character(ID),'')),hjust=0,vjust=1.5)

# Remove Outliers
outliersIDs = CorrByConditionID[CorrByConditionID$Corr <= lowerBoundOutlier | CorrByConditionID$Corr >= upperBoundOutlier, ]
data2 = data2[!(is.element(data2$ID, outliersIDs$ID)),]

# ANOVA

summary(aov(Corr ~ TypeOfChange * PAS + Error(ID), data2))

# GLMM

mf = glmer(Corr ~ TypeOfChange * PAS * (1|ID) * (1|TargetPos) * (1|Trial), 
                    data2, 
                    family = binomial, nAGQ = 0)
summary(mf)


# prepare the data with only conditions of interest

onlyChange2 = data2[data2$TypeOfChange != "NoChange",]


mf = glmer(Corr ~ TypeOfChange * PAS * (1|ID) * (1|TargetPos) * (1|Trial), 
           onlyChange2, 
           family = binomial, nAGQ = 0)
summary(mf)
