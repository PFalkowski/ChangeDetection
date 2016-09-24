# configure R environment
source("../OneDrive/Repos/Change Detection/Helper.R")
#source("../Helper.R")
Packages = c("lme4", "ggplot2", "lattice", "rio", "lmtest", "rms")
WorkingDirectory = "../OneDrive/Repos/Change Detection/Data"
SetupEnvironment(workingDirectory = WorkingDirectory, requiredPackages = Packages)

# read data

data2 <- read.csv("CD_ex2_data_140t_25Ps.xlsx - CD_ex2_RAW.csv", header = TRUE)
aggregate(Corr ~ ChangeType, data2, mean)
describeBy(data2,data2$ChangeType)
# add variables

ScaleMin = 0
ScaleMax = 1
data2$PositionRadians <-data2$TargetPos / 8
data2$ChangeOccured <- ifelse(data2$ChangeOccured > 0, c("yes"), c("no"))
data2$ChangeType <- ifelse(data2$Condition > 2, c("Mirror"), c("Category"))
data2$MirrorChange <- ifelse(data2$Condition > 2, 1, 0)


# Get outliers by response bias

LowerBoundStrategy = .2
UpperBoundStrategy = 1 - LowerBoundStrategy

ResponseByID = aggregate(Response ~  ID, data2, mean)
strategizersIDs = ResponseByID[ResponseByID$Response < LowerBoundStrategy | ResponseByID$Response > UpperBoundStrategy, ]


aggregate(Response ~   ConditionRecoded * ID, data2, mean)

# Remove Outliers

data2 = data2[!(is.element(data2$ID, strategizersIDs$ID)),]

# ANOVA

summary(aov(Corr ~ ChangeType * ChangeOccured * PAS + Error(ID), data2))

# GLMM

mf1 = glmer(Corr ~ (1|ID) , 
           data2, 
           family = binomial)
summary(mf1)

mf2 = glmer(Corr ~ ChangeType + (1|ID) , 
            data2, 
            family = binomial)
summary(mf2)
anova(mf1,mf2)


mf3 = glmer(Corr ~  ChangeType + PAS +
              (1|ID) , 
                    data2, 
                    family = binomial)
summary(mf3)
anova(mf2,mf3)

mf4 = glmer(Corr ~ ChangeType  * PAS +
              (1|ID) , 
            data2, 
            family = binomial)
summary(mf4)
anova(mf3,mf4)


mf5 = glmer(Corr ~ ChangeType  * PAS + ChangeOccured +
              (1|ID) , 
            data2, 
            family = binomial)
summary(mf5)
anova(mf4,mf5)

mf6 = glmer(Corr ~ ChangeType * PAS + ChangeOccured + ChangeType:ChangeOccured +
              (1|ID) , 
            data2, 
            family = binomial,
            control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(mf6)
anova(mf5,mf6)

mf7 = glmer(Corr ~ ChangeType * PAS + ChangeOccured + ChangeType:ChangeOccured +
              ChangeOccured:PAS +
              (1|ID) , 
            data2, 
            family = binomial,
            control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(mf7)
anova(mf6,mf7)

mf8 = glmer(Corr ~ ChangeType * PAS * ChangeOccured +
              (1|ID) , 
            data2, 
            family = binomial,
            control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(mf8)
anova(mf7,mf8)
