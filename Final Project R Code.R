#Load Libraries
library("dplyr")
library("rcompanion")
library("car")
library("mvnormtest")
library("IDPmisc")

CFA <- CFA_data
  #Changed name of data set to something more convenient

plotNormalHistogram(CFA$Sales)
plotNormalHistogram(CFA$NormDT)
plotNormalHistogram(CFA$MobDT)

CFA$Outside <- CFA$'3rdParty'
plotNormalHistogram(CFA$Outside)

#OBJECTIVE 1: To reflect on how successful CFA has evolved to fit the new norm (drive-thru only)
bartlett.test(Sales ~ Time, data = CFA) #Does meet the assumption
fligner.test(Sales ~ Time, data = CFA) #Does meet the assumption

bartlett.test(NormDT ~ Time, data = CFA) #Does meet the assumption
fligner.test(NormDT ~ Time, data = CFA) #Does meet the assumption

bartlett.test(MobDT ~ Time, data = CFA) #Does not meet the assumption
fligner.test(MobDT ~ Time, data = CFA) #Does not meet the assumption

bartlett.test(Outside ~ Time, data = CFA) #Does meet the assumption
fligner.test(Outside ~ Time, data = CFA) #Does meet the assumption

#ANALYZE Sales before during and after.
  
  #Going to try to run the ANOVA without the bonferroni adjustment, because it met the assumption with the bartlett test.
CFA_ANOVA <- aov(CFA$Sales ~ CFA$Time)
summary(CFA_ANOVA)
pairwise.t.test(CFA$Sales, CFA$Time, p.adjust="none")

  #Determine the means in order to draw conclusions:
SALESmeans <- CFA %>% group_by(Time) %>% summarize(Mean = mean(Sales))
SALESmeans
  #CONCLUSION: We see that before and after stayed fairly the same, meaning we haven't increased overall sales by too much.
    # We can also see that there was a pretty significant decrease in overall sales during the 6 week lockdown period. 

#ANALYZE Traditional Drive-Thru(NormDT) sales for before during and after.

CFA_ANOVA <- aov(CFA$NormDT ~ CFA$Time)
summary(CFA_ANOVA)
pairwise.t.test(CFA$NormDT, CFA$Time, p.adjust="none")

DTmeans <- CFA %>% group_by(Time) %>% summarize(Mean = mean(NormDT))
DTmeans
  #CONCLUSION: The average increased by ~$20,000 a week in Traditional Drive-Thru sales from before the lockdown to present. 

#ANALYZE amount of sales done through the mobile app and picked up in drive thru before during and after.

  #Going to try to run the ANOVA with the bonferroni adjustment, because it did not meet the assumption with the bartlett test.
ANOVA <- lm(MobDT ~ Time, data=CFA)
Anova(ANOVA, type = "II", white.adjust = TRUE)
pairwise.t.test(CFA$MobDT, CFA$Time, p.adjust="bonferroni")

MOBmeans <- CFA %>% group_by(Time) %>% summarize(Mean = mean(MobDT))
MOBmeans
  #CONCLUSION: The average increased from $7,300/day before Covid to $25,500/day after the lockdown.

#ANALYZE 3rd party delivery sales growth from before COVID to after.

CFA_ANOVA <- aov(CFA$Outside ~ CFA$Time)
summary(CFA_ANOVA)
pairwise.t.test(CFA$Outside, CFA$Time, p.adjust="none")

OUTmeans <- CFA %>% group_by(Time) %>% summarize(Mean = mean(Outside))
OUTmeans
  #CONCLUSION: Went from less than $1,000/day in sales before to over $5,000/day after!

#OBJECTIVE 2: To analyze how labor costs have been affected by Covid? 
  #This will be calculated by looking at the percent of sales were spent on labor and by seeing how productivity for each month changes over time.


#OBJECTIVE 3: To show the difference in how people are ordering.
  