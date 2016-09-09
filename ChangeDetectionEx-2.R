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

# Get Outliers
CorrByConditionID = aggregate(Corr ~ ID + TypeOfChange, data2, mean)
#outliers = RemoveOutliers(CorrByConditionID$Corr)
lowerBoundOutlier = .25
upperBoundOutlier = 1
CorrByConditionID = aggregate(Corr ~ ID + TypeOfChange, data2, mean)
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

