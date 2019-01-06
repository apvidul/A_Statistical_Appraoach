
setwd("C:/Users/Vidul/Desktop/website")
patient_data = read.table("indian_liver_patient.csv", header = TRUE, sep = ',')
names(patient_data)[11] = "Diagnosis" #Renaming the Variable
summary(patient_data)
patient_data <- na.omit(patient_data) #Dropping rows conatinaing NA 
summary(patient_data)

attach(patient_data)

missing_values = 0 #checking for missing values

for (feature in names(patient_data)){
missing_values = missing_values + sum(is.na(names(patient_data)))
}
print (missing_values)

par(mfrow=c(2,4)) # Generating Figure:1
hist(Total_Bilirubin, main="Hist of Total_Bilirubin", col ="Red")
hist(Direct_Bilirubin, main="Hist of Direct_Bilirubin", col ="Red")
hist(Alkaline_Phosphotase, main="Hist of Alkaline_Phosphotase", col ="Red")
hist(Alamine_Aminotransferase, main="Hist of Alamine_Aminotransferase", col ="Red")
hist(Aspartate_Aminotransferase, main="Hist of Aspartate_Aminotransferase", col ="Red")
hist(Total_Protiens, main="Hist of Total_Protiens", col ="Blue")
hist(Albumin , main="Hist of Albumin ", col ="Blue")
hist(Albumin_and_Globulin_Ratio, main="Hist of Albumin_and_Globulin_Ratio", col ="Blue")


par(mfrow=c(1,3)) #Generating Figure:2
boxplot(Albumin_and_Globulin_Ratio ~ Diagnosis,main="Albumin_and_Globulin_Ratio",col=(c("gold","darkgreen")), notch =TRUE)
title( sub = "1=Patient has liver disease ")
boxplot(Albumin ~ Diagnosis,main="Albumin",col=(c("gold","darkgreen")),, notch =TRUE)
boxplot(Total_Protiens ~ Diagnosis,main="Total_Protiens",col=(c("gold","darkgreen")),, notch =TRUE)
title( sub = "2=Patient does not have liver disease")


#--------------------------------------------Testing for Total_Proteins-------------------------------------------------
has_dis= subset(Total_Protiens, Diagnosis==1) #Dviding the Total_Protein based on diagnosis
no_dis = subset(Total_Protiens, Diagnosis==2)

set.seed(2) # just to make it reproducible
has_dis = sample(has_dis)


has_dis = has_dis[1:100] #Extracting 100 random samples
no_dis = no_dis[1:100]


var.test(has_dis, no_dis, alternative = "two.sided") #F-test for comparing variances

t.test(has_dis,no_dis,var.equal = TRUE) #t-test with equal variance

t.test(has_dis) #One sample t-test to estimate the mean
t.test(no_dis)

#----------------------------------------------------Testing for Albumin-----------------------------------------------

has_dis= subset(Albumin, Diagnosis==1)
no_dis = subset(Albumin, Diagnosis==2)

set.seed(2)
has_dis = sample(has_dis)

has_dis = has_dis[1:100]
no_dis = no_dis[1:100]

var.test(has_dis, no_dis, alternative = "two.sided")

t.test(has_dis,no_dis,var.equal = TRUE)

t.test(has_dis)
t.test(no_dis)

#--------------------------Testing for Albumin_and_Globulin_Ratio-----------------------------------------------------

has_dis= subset(Albumin_and_Globulin_Ratio, Diagnosis==1)
no_dis = subset(Albumin_and_Globulin_Ratio, Diagnosis==2)

set.seed(2) # just to make it reproducible
has_dis = sample(has_dis)


has_dis = has_dis[1:100]
no_dis = no_dis[1:100]

var.test(has_dis, no_dis, alternative = "two.sided")

t.test(has_dis,no_dis, var.equal = TRUE)
t.test (has_dis)
t.test(no_dis)


new_patient_data = patient_data[,c(3,4,5,6,7,8,9,10,11)] #Generating Correlating Matrix
install.packages("corrplot")
pd = cor(new_patient_data)
library(corrplot)
corrplot(pd, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
#if the above snippet doesn't run, try uncommenting and execting the below command and re run
#dev.off()

cor(new_patient_data)

fit =  lm(Albumin ~ Total_Protiens, data = patient_data)
summary(fit)
#rm(list = ls())

estimates = predict(fit, data.frame(Total_Protiens))


patient_data$Estimates = estimates

has_dis= subset(Estimates, Diagnosis==1)
no_dis = subset(Estimates, Diagnosis==2)

set.seed(2) # just to make it reproducible
has_dis = sample(has_dis)

has_dis=has_dis[1:100]
no_dis=no_dis[1:100]

var.test(has_dis, no_dis, alternative = "two.sided")
t.test(has_dis,no_dis)


########################################

fit2 =  lm(Albumin ~ Total_Bilirubin, data = patient_data)
summary(fit2)

estimates2 = predict(fit2, data.frame(Total_Bilirubin))
print (estimates2)

plot(Total_Protiens, Albumin, main="Regresssion Model", 
     xlab="Total Proteins ", ylab="Albumin ")

library(ggplot2)
ggplot(patient_data, aes(x=Total_Protiens, y=Albumin, color="Red")) +
  geom_point(shape=1) +
  scale_colour_hue(l=10) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE) 
library(ggplot2)
ggplot(patient_data, aes(x=Direct_Bilirubin, y=Albumin, color="Blue")) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE) 

ggplot(patient_data, aes(x=Direct_Bilirubin, y=Albumin, color="56B4E9")) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
ggplot(patient_data, aes(x=Total_Protiens, y=Albumin, color="Reg. Line")) +
  geom_point(colour="#56B4E9") + 
#  scale_colour_hue(l=25)+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE,colour="#56B4E9")

fit3 =  lm(Albumin ~ Total_Bilirubin + Total_Protiens + Alkaline_Phosphotase, data = patient_data)
summary(fit3)
