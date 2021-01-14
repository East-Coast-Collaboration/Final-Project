library("mvnormtest")
library("car")
library("IDPmisc")

# load in data

library(readxl)
CFA_data <- read_excel("Bethel/11_FinalProject/Datasets/CFA_data.xlsx")
View(CFA_data)

# a little data wrangling

names(CFA_data)[names(CFA_data)=="3rdParty"]="ThirdParty"
head(CFA_data)

# make sure DV's are numeric

str(CFA_data$MobCO)
str(CFA_data$MobDI)
str(CFA_data$MobDT)
str(CFA_data$NormDT)
str(CFA_data$CarryO)
str(CFA_data$ThirdParty)
str(CFA_data$DineIn)
str(CFA_data$Catering)

# recode IV to be numeric

CFA_data$TimeR=NA
CFA_data$TimeR[CFA_data$Time=="Before"]=1
CFA_data$TimeR[CFA_data$Time=="During"]=2
CFA_data$TimeR[CFA_data$Time=="After"]=3

# subset and make a matrix

keeps=c("MobCO","MobDI","MobDT",'NormDT',"CarryO","ThirdParty","DineIn","Catering")
CFA_data1=CFA_data[keeps]
CFA_data1mx=as.matrix(CFA_data1)

# Test assumptions

## Sample Size

## Multivariate Normality

mshapiro.test(t(CFA_data1mx))

# did not pass - p was significant - proceed to Levenes Test

leveneTest(MobCO~Time,data=CFA_data)
leveneTest(MobDI~Time,data=CFA_data)
leveneTest(MobDT~Time,data=CFA_data)
leveneTest(NormDT~Time,data=CFA_data)
leveneTest(CarryO~Time,data=CFA_data)
leveneTest(ThirdParty~Time,data=CFA_data)
leveneTest(DineIn~Time,data=CFA_data)
leveneTest(Catering~Time,data=CFA_data)

# leveneTest(MobCO~Time,data=CFA_data) - did not pass
# leveneTest(MobDI~Time,data=CFA_data) - did not pass
# leveneTest(MobDT~Time,data=CFA_data) - did not pass
# leveneTest(NormDT~Time,data=CFA_data) - passed 
# leveneTest(CarryO~Time,data=CFA_data) - did not pass
# leveneTest(ThirdParty~Time,data=CFA_data) - passed
# leveneTest(DineIn~Time,data=CFA_data) - did not pass
# leveneTest(Catering~Time,data=CFA_data) - passed

## absense of Multicolinearity

cor.test(CFA_data$MobCO,CFA_data$MobDI,method="pearson",use="complete.obs")# did not pass
cor.test(CFA_data$MobCO,CFA_data$MobDT,method="pearson",use="complete.obs")# did not pass
cor.test(CFA_data$MobCO,CFA_data$ThirdParty,method="pearson",use="complete.obs") # did not pass
cor.test(CFA_data$MobCO,CFA_data$NormDT,method="pearson",use="complete.obs") # did not pass
cor.test(CFA_data$MobCO,CFA_data$CarryO,method="pearson",use="complete.obs") # did not pass
cor.test(CFA_data$MobCO,CFA_data$DineIn,method="pearson",use="complete.obs") # did not pass
cor.test(CFA_data$MobCO,CFA_data$Catering,method="pearson",use="complete.obs") 
cor.test(CFA_data$MobDI,CFA_data$MobDT,method="pearson",use="complete.obs") # did not pass
cor.test(CFA_data$MobDI,CFA_data$ThirdParty,method="pearson",use="complete.obs") # did not pass
cor.test(CFA_data$MobDI,CFA_data$NormDT,method="pearson",use="complete.obs") # did not pass
cor.test(CFA_data$MobDI,CFA_data$CarryO,method="pearson",use="complete.obs") # did not pass
cor.test(CFA_data$MobDI,CFA_data$DineIn,method="pearson",use="complete.obs") # did not pass
cor.test(CFA_data$MobDI,CFA_data$Catering,method="pearson",use="complete.obs")
cor.test(CFA_data$MobDT,CFA_data$ThirdParty,method="pearson",use="complete.obs") # did not pass
cor.test(CFA_data$MobDT,CFA_data$NormDT,method="pearson",use="complete.obs") # did not pass
cor.test(CFA_data$MobDT,CFA_data$CarryO,method="pearson",use="complete.obs") # did not pass
cor.test(CFA_data$MobDT,CFA_data$DineIn,method="pearson",use="complete.obs") # did not pass
cor.test(CFA_data$MobDT,CFA_data$Catering,method="pearson",use="complete.obs")
cor.test(CFA_data$ThirdParty,CFA_data$NormDT,method="pearson",use="complete.obs") ## did not pass
cor.test(CFA_data$ThirdParty,CFA_data$CarryO,method="pearson",use="complete.obs") # did not pass
cor.test(CFA_data$ThirdParty,CFA_data$DineIn,method="pearson",use="complete.obs") # did not pass
cor.test(CFA_data$ThirdParty,CFA_data$Catering,method="pearson",use="complete.obs")
cor.test(CFA_data$NormDT,CFA_data$CarryO,method="pearson",use="complete.obs") # did not pass
cor.test(CFA_data$NormDT,CFA_data$DineIn,method="pearson",use="complete.obs") # did not pass
cor.test(CFA_data$NormDT,CFA_data$Catering,method="pearson",use="complete.obs")
cor.test(CFA_data$CarryO,CFA_data$DineIn,method="pearson",use="complete.obs") # did not pass
cor.test(CFA_data$CarryO,CFA_data$Catering,method="pearson",use="complete.obs")
cor.test(CFA_data$DineIn,CFA_data$Catering,method="pearson",use="complete.obs")

corr_matrix <- cor(CFA_data)

## do analysis anyway

MANOVA = manova(cbind(MobCO,MobDI,MobDT,NormDT,CarryO,ThirdParty,DineIn,Catering)~Time, data = CFA_data)
summary(MANOVA)

summary.aov(MANOVA, test = "wilks")
  