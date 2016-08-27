
subjects <- read.csv("subjects.csv", header = TRUE)
subjectsCD2 <- subjects[subjects$Experiment == "CD_2",]
subjectsCD3 <- subjects[subjects$Experiment == "CD_3",]

summary(subjectsCD2)
summary(subjectsCD3)
