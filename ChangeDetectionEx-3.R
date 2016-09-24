center_scale <- function(x) {
  scale(x, scale = FALSE)
}

# configure R environment

source("../OneDrive/Repos/Change Detection/Helper.R")
Wants = c("lme4", "ggplot2", "lattice", "rio", "lmtest", "rms", "stargazer", "reshape", "psych",
          "MASS", "ordinal", "rms", "VGAM")
WorkingDirectory = "../OneDrive/Repos/Change Detection/Data"
SetupEnvironment(workingDirectory = WorkingDirectory, requiredPackages = Wants)
# read data

data <- read.csv("CD_ex3_RAWdata - Long.csv", header = TRUE)
describeBy(data,data$ChangeType)
# organize variables
ScaleMin = 0
ScaleMax = 1
data$ScaledPAS = rescale(data$PAS, to=c(ScaleMin, ScaleMax))
data$PositionRadians <-data$TargetPos / 8
data <- rename(data, c(BlockType="ChangeType"))

data$ID = as.factor(tolower(as.character(data$ID)))
## Centrujemy na najni¿szym ratingu
data$PAS = data$PAS - min(data$PAS)

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
mf = glmer(Corr ~ Memory * ChangeType + ChangeOccured + (1|ID) , #* (1|TargetRadians) * (1|TrialsOrder)
                 data, 
                 family = binomial,
                 control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(mf)

mf2 = glmer(Corr ~ Memory * ChangeType +
            
             (ChangeType|ID), #* (1|TargetRadians) * (1|TrialsOrder)
           data, 
           family = binomial,
           control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(mf2)
mf3 = glmer(Corr ~ Memory * ChangeType * ChangeOccured +
              
              (ChangeType|ID), #* (1|TargetRadians) * (1|TrialsOrder)
            data, 
            family = binomial,
            control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
summary(mf3)
anova(mf2, mf3)

summary((m = glmer(Corr ~  ChangeType *  Memory + (PAS |ID), data, family = binomial,
                   control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))))

summary(aov(PAS ~ Memory * ChangeType * ID, data))
lf = (lmer(PAS ~ Memory + ChangeType + Setsize + Memory:ChangeType + (1|ID),  REML=FALSE, data = data))
summary(lf)
summary(vglm(formula = PAS ~ Memory + ChangeType + Setsize + Memory:ChangeType, family = propodds, data = data))
