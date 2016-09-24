source("../OneDrive/Repos/Change Detection/Helper.R")
Wants = c("lme4", "ggplot2", "lattice", "rio", "lmtest", "rms", "stargazer", "reshape", "psych",
          "MASS", "ordinal", "rms", "VGAM")
WorkingDirectory = "../OneDrive/Repos/Change Detection/Data"
SetupEnvironment(workingDirectory = WorkingDirectory, requiredPackages = Wants)



df <- read.csv("CD_ex3_RAWdata - Long.csv", header = TRUE)


df <- rename(df, c(BlockType="ChangeType"))

df$ID = as.factor(tolower(as.character(df$ID)))
## Centrujemy na najni�szym ratingu
df$PAS = df$PAS - min(df$PAS)

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

aggregate(Corr ~ ChangeOccured + ChangeType + Memory, df, function(x)paste(round(mean(x),2), '(', round(sd(x),2), ')'))


dotplot(ID ~ Corr, aggregate(Corr ~ ID, df, mean), horizontal = T)

summary((m1 <- glmer(Corr ~ PAS * ChangeType * ChangeOccured + (PAS + ChangeType |ID),
                     df, family = 'binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))))
summary((m2 <- glmer(Corr ~ PAS + ChangeType + ChangeOccured + Memory +
                       ChangeType:Memory +
                       PAS:ChangeType +
                       PAS:Memory +
                       PAS:ChangeType:Memory
                       (PAS + ChangeType |ID),
                     df, family = 'binomial', control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))))

summary((m3 <- glmer(Corr ~ PAS * ChangeType * ChangeOccured + (1|ID), df, family = 'binomial')))
summary((m4 <- glmer(Corr ~ PAS * ChangeType + Memory + (PAS + ChangeType |ID), df, family = 'binomial')))
anova(m1,m2)
df$fit = (model.matrix(m1) %*% fixef(m1))[,1]
(p1 <- ggplot(df, aes(x = (PAS + 1), y = fit, group = ChangeType, lty = ChangeType)) + geom_line())

res = aggregate(fit ~ ChangeType + Memory + PAS, df, mean)
res$n = aggregate(fit ~ ChangeType + Memory + PAS, df, length)$fit
lns = aggregate(fit ~ ChangeType + Memory, df, length)


res$f = res$n / lns[as.character(res$gr), 'fit']
p2 <- ggplot(res, aes(x = (PAS + 1), y = fit, group = ChangeType, lty = ChangeType)) + geom_line() + geom_point(aes(size = f)) +
  labs(x = 'Confidence', y = 'Accuracy', lty = 'Order', size = 'Freq')

