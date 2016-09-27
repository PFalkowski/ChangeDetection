source("../OneDrive/Repos/Change Detection/Helper.R")
Wants = c("lme4", "ggplot2", "lattice", "rio", "lmtest", "rms", "stargazer", "reshape", "psych",
          "MASS", "ordinal", "rms", "VGAM", "stringr")
WorkingDirectory = "../OneDrive/Repos/Change Detection/Data"
SetupEnvironment(workingDirectory = WorkingDirectory, requiredPackages = Wants)



df <- read.csv("CD_ex3_RAWdata - Long.csv", header = TRUE)


df <- rename(df, c(BlockType="ChangeType"))

df$ID = as.factor(tolower(as.character(df$ID)))
## Centrujemy na najni¿szym ratingu
df$PAS = df$PAS - min(df$PAS)
df$PASF = as.factor(df$PAS)
res = aggregate(Corr ~ ID, df, mean)
res$n = aggregate(Corr ~ ID, df, length)$Corr
rownames(res) = as.character(res$ID)
df$macc = res[as.character(df$ID), 'Corr']
res$f = res$n / lns[as.character(res$gr), 'fit']
## Wywalamy osoby
df = df[df$macc > 0.6,]
print("Srednia poprawnosc")
mean(df$Corr)
print("SD")
sd(df$macc, na.rm = FALSE)
pdf(file = 'pl.pdf')
plot(sort(aggregate(PAS ~ ID, df, sd)$PAS))
dev.off()
df$PAS.sd = aggregate.expand(df$PAS, df$ID, sd)



print("Opisowka tabeleczki")


aggregate(ResponseRT ~ Corr + Memory + ChangeType, df, function(x)paste(round(mean(x),2), '(', round(sd(x),2), ')'))

aggregate(PAS ~ Corr + Memory + ChangeType, df, function(x)paste(round(mean(x),2), '(', round(sd(x),2), ')'))

aggregate(Corr ~ Memory + ChangeOccured + ChangeType, df, function(x)paste(round(mean(x),3), '(', round(length(x),2), ')'))


dotplot(ID ~ Corr, aggregate(Corr ~ ID, df, mean), horizontal = T)

summary((m1 <- glmer(Corr ~ PAS * ChangeType + Memory +
                       (ChangeType + PAS|ID),
                     df, family = 'binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 10000000)))))

summary((m0 <- glmer(Corr ~ PAS * ChangeType * Memory +
                       (ChangeType + PAS|ID),
                     df, family = 'binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 10000000)))))
anova(m0,m3)
summary((m3 <- glmer(Corr ~ PAS * ChangeType * ChangeOccured +
                       (ChangeType+ PAS|ID),
                     df, family = 'binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 10000000)))))
#save(m3, file = "CdEx3glmer")

summary((m4 <- glmer(Corr ~  PAS * ChangeType * Memory +
                       (Memory + PAS|ID),
                     df, family = 'binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 10000000)))))

anova(m3,m4)
summary((m4 <- glmer(Corr ~ PAS * ChangeType * ChangeOccured + Memory + (ChangeType + PAS |ID), df, family = 'binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 10000000)))))

summary((m7 <- glmer(Corr ~ PASF * ChangeType * Memory + (ChangeType |ID), df, family = 'binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 10000000)))))

anova(m0,m3)
df$fit = (model.matrix(m1) %*% fixef(m1))[,1]
(p1 <- ggplot(df, aes(x = (PAS + 1), y = fit, group = ChangeType, lty = ChangeType)) + geom_line())

res = aggregate(fit ~ ChangeType + Memory + PAS, df, mean)
res$n = aggregate(fit ~ ChangeType + Memory + PAS, df, length)$fit
lns = aggregate(fit ~ ChangeType + Memory, df, length)


res$f = res$n / lns[as.character(res$gr), 'fit']
p2 <- ggplot(res, aes(x = (PAS + 1), y = fit, group = ChangeType, lty = ChangeType)) + geom_line() + geom_point(aes(size = f)) +
  labs(x = 'Confidence', y = 'Accuracy', lty = 'Order', size = 'Freq')

#PAS
lm = lmer(PAS ~ ChangeType * Memory + (1 | ID), df)
summary(lm)
lmer.sig()
