# configure R environment

source("../OneDrive/Repos/Change Detection/Helper.R")
Packages = c("lme4", "ggplot2", "lattice", "rio", "lmtest", "rms", "stargazer", "reshape")
WorkingDirectory = "../OneDrive/Repos/Change Detection/Data"
SetupEnvironment(workingDirectory = WorkingDirectory, requiredPackages = Packages)
# read data

data <- read.csv("CD_ex3_RAWdata - Long.csv", header = TRUE)

# organize variables
ScaleMin = 0
ScaleMax = 1
data$ScaledPAS = rescale(data$PAS, to=c(ScaleMin, ScaleMax))
data$PositionRadians <-data$TargetPos / 8

data <- rename(data, c(BlockType="ChangeType"))
# Get Outliers

responseBias <- aggregate(Response ~ ID, data, mean)

lowerBoundOutlier = .25
upperBoundOutlier = 1

CorrByConditionID = aggregate(Corr ~ ID * ConditionRecoded * Memory, data, mean)
outliersIDs = CorrByConditionID[CorrByConditionID$Corr < lowerBoundOutlier | CorrByConditionID$Corr > upperBoundOutlier, ]
outliersData = data[(is.element(data$ID, outliersIDs$ID)),]
outliersAnalysis = aggregate(Corr ~ ConditionRecoded * Memory * ID, outliersData, mean)


# Plot Outliers
ggplot(CorrByConditionID, aes(Corr,  Memory, colour=ID)) + 
  geom_line() + 
  geom_point() +
  geom_text(aes(label=ifelse(Corr<lowerBoundOutlier | Corr> upperBoundOutlier,as.character(ID),'')),hjust=0,vjust=1.5)

# Remove Outliers

data = data[!(is.element(data$ID, outliersIDs$ID)),]


# GLMM
basicModel = glmer(Corr ~  Memory * (1|ID) , #* (1|TargetRadians) * (1|TrialsOrder)
                 data, 
                 family = binomial,
                 control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(basicModel)

basicModel2 = glmer(Corr ~  ChangeType * (1|ID) , #* (1|TargetRadians) * (1|TrialsOrder)
                   data, 
                   family = binomial,
                   control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(basicModel2)

basicModel3 = glmer(Corr ~  Memory * ChangeType * (1|ID) , #* (1|TargetRadians) * (1|TrialsOrder)
                    data, 
                    family = binomial,
                    control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(basicModel3)
anova(basicModel2, basicModel3)
TwoByTwo = glmer(Corr ~  Memory * ChangeType *  PAS  * (1|ID) , #* (1|TargetRadians) * (1|TrialsOrder)
                 data, 
                   family = binomial,
                   control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

TwoByTwoPlusChangeOccured = glmer(Corr ~ Memory * ChangeType * ChangeOccured *  PAS  * (1|ID), 
                 data, 
                 family = binomial,
                 control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

TwoByTwoPlusTargetRadians = glmer(Corr ~ Memory * ChangeType * ChangeOccured * PAS * (1|Memory/ID), 
                                  data, 
                                  family = binomial,
                                  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

myModel = glmer(Corr ~ Memory * ChangeType * ChangeOccured * PAS +
                  PAS:Memory:ChangeType +
                  (1|Memory/ID),
                #(1|ID) + (1|ID:Memory), 
                                  data, 
                                  family = binomial,
                                  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(myModel)
#TwoByTwoPlusTrialsOrder = glmer(Corr ~ Memory * ChangeType * ChangeOccured *  PAS  * (1|ID) * (1|TargetRadians) * (1|TrialsOrder), 
#                                  data, 
#                                  family = binomial,
#                                  control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

#TwoByTwoNoMemory = glmer(Corr ~  ChangeType * ChangeOccured *  PAS  * (1|ID) * (1|TargetRadians), 
#                                data, 
#                                family = binomial,
#                                control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

summary(TwoByTwo)
summary(TwoByTwoPlusTargetRadians)

anova(TwoByTwo, TwoByTwoPlusChangeOccured)
anova(TwoByTwoPlusChangeOccured, TwoByTwoPlusTargetRadians)
#anova(TwoByTwoPlusTargetRadians, TwoByTwoPlusTrialsOrder)
#anova(TwoByTwoPlusTargetRadians, TwoByTwoNoMemory)
# ANOVA

summary(aov(Setsize ~ Memory * ChangeType * ChangeOccured *  PAS + Error(ID), data))

stargazer(TwoByTwo, TwoByTwoPlusChangeOccured, TwoByTwoPlusTargetRadians,
          align=TRUE, type="html")


