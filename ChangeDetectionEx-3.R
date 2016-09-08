# configure R environment
source("../OneDrive/Repos/Change Detection/Helper.R")
Packages = c("lme4", "ggplot2", "lattice", "rio", "lmtest", "rms")
WorkingDirectory = "../OneDrive/Repos/Change Detection/Data"
SetupEnvironment(workingDirectory = WorkingDirectory, requiredPackages = Packages)
library("stargazer")
# read data

data <- read.csv("CD_ex3_RAWdata - Long.csv", header = TRUE)

# add variables
ScaleMin = 0
ScaleMax = 1
data$ScaledPAS = rescale(data$PAS, to=c(ScaleMin, ScaleMax))
data$ScaledSetsize = rescale(data$Setsize, to=c(ScaleMin, ScaleMax))


# validate
#str(data)
TrialsPerConditionPerID = aggregate(Response ~ ID * ConditionRecoded * Memory, data, length)
ResponseBias = aggregate(Response ~ ID * ConditionRecoded * Memory, data, mean)
ResponseBias = aggregate(Corr ~ ID * ConditionRecoded * Memory, data, mean)
  
  
  
TrialsByCondition = aggregate(Corr ~ TypeOfChange * Memory, data, mean)
TrialsByCondition
dotplot(Corr ~ TypeOfChange + Memory, TrialsByCondition)


IDbyCorr = aggregate(Corr ~ ID, data, mean)
dotplot(ID ~ Corr, IDbyCorr)

PASbyID = aggregate(PAS ~ ID, data, mean)
dotplot(ID ~ PAS, PASbyID)


RecodedConditionbyCorr = aggregate(Corr ~ ConditionRecoded, data, mean)
dotplot(Corr ~ ConditionRecoded, RecodedConditionbyCorr)


RecodedConditionbySetsize = aggregate(Setsize ~ RecodedCondition, data, mean)
dotplot(Setsize ~ RecodedCondition, RecodedConditionbySetsize)

# Analize erroneus responses

lowerBoundOutlier = .25
upperBoundOutlier = 1

CorrByConditionID = aggregate(Corr ~ ID * ConditionRecoded * Memory, data, mean)
outliersIDs = CorrByConditionID[CorrByConditionID$Corr < lowerBoundOutlier | CorrByConditionID$Corr > upperBoundOutlier, ]
outliersData = data[(is.element(data$ID, outliersIDs$ID)),]
outliersAnalysis = aggregate(Corr ~ ConditionRecoded * Memory * ID, outliersData, mean)

dotplot(Corr ~ ConditionRecoded , outliersAnalysis)
# Get Outliers

ggplot(CorrByConditionID, aes(Corr,  Memory, colour=ID)) + 
  geom_line() + 
  geom_point() +
  geom_text(aes(label=ifelse(Corr<lowerBoundOutlier | Corr> upperBoundOutlier,as.character(ID),'')),hjust=0,vjust=1.5)

# Remove Outliers

data = data[!(is.element(data$ID, outliersIDs$ID)),]

# prepare the data with only conditions of interest

onlyChange = data[data$TypeOfChange != "NoChange",]

# GLMM

TwoByTwo = glmer(Corr ~ Memory * TypeOfChange *  PAS  * (1|ID), 
                 onlyChange, 
                   family = binomial,
                   control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
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

summary(TwoByTwo)

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
# Results charting

CorrByTrial = aggregate(Corr ~ Trial * TypeOfChange * Memory, data, mean)
ggplot(CorrByTrial, aes(x=Trial, y=Corr, colour=TypeOfChange, group = TypeOfChange)) + geom_line()


sp(Corr ~ PAS * Memory, 
   data = data,
   xlab = "PAS",
   ylab = "Correctnes",
   main = "Data",
   labels = row.names(data))

scatterplotMatrix( ~ TypeOfChange * Memory | Corr * PAS ,
                  data = data,
                  main = paste("Scatterplot Matrix",
                               "Using the \"car\" Package"))
barplotData = 
barplot(data,
        beside = TRUE,
        col = c("steelblue3", "thistle3"),
        main = "Mean Number of Warp Breaks\nby Tension and Wool",
        xlab = "Tension",
        ylab = "Mean Number of Breaks")

fit = fitted(mf)
res = aggregate(fit ~ ID + Memory * TypeOfChange * PAS, data, mean)
ggplot(res, aes(x = Corr, y = fit, group = TypeOfChange, 
                color = TypeOfChange)) + geom_line() 


