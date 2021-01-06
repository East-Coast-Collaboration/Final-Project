FinalProject <- Final_Project_Data
  #Changed name of data set to something more convenient

plotNormalHistogram(FinalProject$Sales)
plotNormalHistogram(FinalProject$NormDT)
plotNormalHistogram(FinalProject$MobDT)

bartlett.test(Sales ~ Time, data = FinalProject) #Does not meet the assumption
fligner.test(Sales ~ Time, data = FinalProject) #Does meet the assumption

bartlett.test(NormDT ~ Time, data = FinalProject) #Does meet the assumption
fligner.test(NormDT ~ Time, data = FinalProject) #Does meet the assumption

bartlett.test(MobDT ~ Time, data = FinalProject) #Does not meet the assumption
fligner.test(MobDT ~ Time, data = FinalProject) #Does not meet the assumption

ANOVA <- lm(Sales ~ Time, data=FinalProject)
Anova(ANOVA, type = "II", white.adjust = TRUE)

pairwise.t.test(FinalProject$Sales, FinalProject$Time, p.adjust="none")