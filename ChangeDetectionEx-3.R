# configure R environment
source("../OneDrive/Repos/Change Detection/Helper.R")
Packages = c("lme4", "ggplot2", "lattice", "rio", "lmtest", "rms")
WorkingDirectory = "../OneDrive/Repos/Change Detection/Data"
SetupEnvironment(workingDirectory = WorkingDirectory, requiredPackages = Packages)

# read data

data <- read.csv("CD_ex3_RAWdata - Long.csv", header = TRUE)

# add variables
ScaleMin = 0
ScaleMax = 1
data$ScaledPAS = rescale(data$PAS, to=c(ScaleMin, ScaleMax))
data$ScaledSetsize = rescale(data$Setsize, to=c(ScaleMin, ScaleMax))

#str(data)

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

summary(aov(Setsize ~ TypeOfChange * Memory * PAS + Error(ID), data))

# GLMM

ThreeByTwo = glmer(Corr ~ Memory * TypeOfChange  * PAS  * (1|ID) * (1|TargetRadians) * (1|Trial) , 
           data, 
           family = binomial)

FourByTwo = glmer(Corr ~ Memory * ConditionRecoded  * PAS  * (1|ID) * (1|TargetRadians) * (1|Trial) , 
                    data, 
                    family = binomial,
                    control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

TwoByTwo = glmer(Corr ~ Memory * ChangeOccured * BlockType * PAS  * (1|ID) * (1|TargetRadians) * (1|Trial) , 
                 data, 
                 family = binomial,
                 control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

summary(FourByTwo)
summary(TwoByTwo)

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


