# configure R environment

source("../OneDrive/Repos/Change Detection/Helper.R")
Packages = c("lme4", "ggplot2", "lattice", "rio", "lmtest", "rms", "stargazer")
WorkingDirectory = "../OneDrive/Repos/Change Detection/Data"
SetupEnvironment(workingDirectory = WorkingDirectory, requiredPackages = Packages)
# read data

data <- read.csv("CD_ex3_RAWdata - Long.csv", header = TRUE)

# add variables
ScaleMin = 0
ScaleMax = 1
data2$ScaledPAS = rescale(data2$PAS, to=c(ScaleMin, ScaleMax))
data2$PositionRadians <-data2$TargetPos / 8

# Get Outliers

responseBias <- aggregate(Response ~ ID, data, mean)

lowerBoundOutlier = .25
upperBoundOutlier = 1

CorrByConditionID = aggregate(Corr ~ ID * ConditionRecoded * Memory, data, mean)
outliersIDs = CorrByConditionID[CorrByConditionID$Corr < lowerBoundOutlier | CorrByConditionID$Corr > upperBoundOutlier, ]
outliersData = data[(is.element(data$ID, outliersIDs$ID)),]
outliersAnalysis = aggregate(Corr ~ ConditionRecoded * Memory * ID, outliersData, mean)


ggplot(CorrByConditionID, aes(Corr,  Memory, colour=ID)) + 
  geom_line() + 
  geom_point() +
  geom_text(aes(label=ifelse(Corr<lowerBoundOutlier | Corr> upperBoundOutlier,as.character(ID),'')),hjust=0,vjust=1.5)

# Remove Outliers

data = data[!(is.element(data$ID, outliersIDs$ID)),]


# GLMM

TwoByTwo = glmer(Corr ~  Memory * BlockType *  PAS  * (1|ID) , #* (1|TargetRadians) * (1|TrialsOrder)
                 data, 
                   family = binomial,
                   control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

TwoByTwoPlusChangeOccured = glmer(Corr ~ Memory * BlockType * ChangeOccured *  PAS  * (1|ID), 
                 data, 
                 family = binomial,
                 control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

TwoByTwoPlusTargetRadians = glmer(Corr ~ Memory * BlockType * ChangeOccured *  PAS  * (1|ID) * (1|TargetRadians), 
                                  data, 
                                  family = binomial,
                                  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

#TwoByTwoPlusTrialsOrder = glmer(Corr ~ Memory * BlockType * ChangeOccured *  PAS  * (1|ID) * (1|TargetRadians) * (1|TrialsOrder), 
#                                  data, 
#                                  family = binomial,
#                                  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

#TwoByTwoNoMemory = glmer(Corr ~  BlockType * ChangeOccured *  PAS  * (1|ID) * (1|TargetRadians), 
#                                data, 
#                                family = binomial,
#                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

summary(TwoByTwo)
summary(TwoByTwoPlusTrialsOrder)

anova(TwoByTwo, TwoByTwoPlusChangeOccured)
anova(TwoByTwoPlusChangeOccured, TwoByTwoPlusTargetRadians)
#anova(TwoByTwoPlusTargetRadians, TwoByTwoPlusTrialsOrder)
#anova(TwoByTwoPlusTargetRadians, TwoByTwoNoMemory)
# ANOVA

summary(aov(Setsize ~ Memory * BlockType * ChangeOccured *  PAS + Error(ID), data))

stargazer(TwoByTwo, TwoByTwoPlusChangeOccured, TwoByTwoPlusTargetRadians,
          align=TRUE, type="html")


