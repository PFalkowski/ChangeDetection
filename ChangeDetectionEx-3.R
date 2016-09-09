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

lowerBoundOutlier = .3
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

# prepare the data with only conditions of interest

onlyChange = data[data$TypeOfChange != "NoChange",]

# GLMM
x <- sample(12)
set.seed(77777777)
TwoByTwo = glmer(Corr ~ Memory * TypeOfChange *  PAS  * (1|ID)+ (1|TargetRadians), 
                 onlyChange, 
                   family = binomial,
                   control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

summary(TwoByTwo)

TwoByTwoPlus = glmer(Corr ~ Memory * TypeOfChange *  PAS  * (1|ID) + (1|TargetRadians), 
                 onlyChange, 
                 family = binomial,
                 control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

TwoByTwoPlusOrder = glmer(Corr ~ Memory * TypeOfChange *  PAS  * (1|ID) + (1|TrialsOrder), 
                          onlyChange, 
                          family = binomial,
                          control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

TwoByTwoPlusTrials = glmer(Corr ~ Memory * TypeOfChange *  PAS  * (1|ID) + (1|Trial), 
                          onlyChange, 
                          family = binomial,
                          control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))



anova(FourByTwo, ThreeByTwo)
# ANOVA

summary(aov(Setsize ~ TypeOfChange * Memory * PAS + Error(ID), data))

# GLMM

ThreeByTwo = glmer(Corr ~ Memory * TypeOfChange  * PAS  * (1|ID)  , 
           data, 
           family = binomial,
           control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

FourByTwo = glmer(Corr ~ Memory * ConditionRecoded  * PAS  * (1|ID)  , 
                    data, 
                    family = binomial,
                    control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

TwoByTwo = glmer(Corr ~ Memory * ChangeOccured * BlockType * PAS  * (1|ID) , 
                 data, 
                 family = binomial,
                 control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))



summary(FourByTwo)
summary(ThreeByTwo)
summary(TwoByTwo)

stargazer(TwoByTwoPlusOrder, TwoByTwoPlus, TwoByTwo, ThreeByTwo, FourByTwo, btitle="Regression Results", align=TRUE, type="html")


