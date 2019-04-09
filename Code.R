# Uncomment the below line and set the appropriate 
#setwd("F:/Data Analysis")
Admission_Predict_Ver1.1 <- read.csv("Admission_Predict_Ver1.1.csv")
View(Admission_Predict_Ver1.1)
admissions=Admission_Predict_Ver1.1
str(admissions)
dim(admissions)
summary(admissions)

#checking for missing values#
is.na(admissions)
length(unique(admissions$Serial.No.))==nrow(admissions)

#removing serial no from data frame#
admissions$Serial.No.= NULL
View(admissions)

#checking correlations#
cor(admissions, use="everything", method="pearson")
install.packages(corrplot)
library(corrplot)
A<-cor(admissions)
head(round(A, 4))
corrplot(A, method="circle")
corrplot(A, method="number")
corrplot(A, method="color")
corrplot(A, type="upper", order="hclust")
#strongest correlation with admission chances appear to be GRE score, TOEFL score and CGPA.

#Let's explore these relationships further#
library(ggplot2)
ggplot(admissions, aes(x=GRE.Score, y=Chance.of.Admit))+geom_point(shape=14, color="blue")
ggplot(admissions, aes(x=GRE.Score, y=Chance.of.Admit))+geom_point(shape=14, color="blue")+geom_abline()
ggplot(admissions, aes(x=GRE.Score, y=Chance.of.Admit))+geom_point(shape=14, color="blue")+geom_smooth(method=lm)
ggplot(admissions, aes(x=GRE.Score, y=Chance.of.Admit))+geom_point(shape=14, color="blue")+geom_smooth(method=lm, color="red")
ggplot(admissions, aes(x=TOEFL.Score, y=Chance.of.Admit))+geom_point(shape=14, color="blue")
ggplot(admissions, aes(x=TOEFL.Score, y=Chance.of.Admit))+geom_point(shape=14, color="blue")+geom_smooth()
ggplot(admissions, aes(x=CGPA, y=Chance.of.Admit))+geom_point(shape=14, color="blue")+geom_smooth()
ggplot(admissions, aes(x=CGPA, y=Chance.of.Admit))+geom_point(shape=14, color="blue")+geom_smooth(method=lm, color="red")


G1<-ggplot(admissions, aes(x=CGPA, y=Chance.of.Admit))+geom_point(shape=14, color="blue")
G1+ggtitle("Admission Chances with CGPA")+xlab("CGPA")+ylab("Admission Chances")
G2<-ggplot(admissions, aes(x=GRE.Score, y=Chance.of.Admit))+geom_point(shape=14, color="red")
G2+ ggtitle("Admission Chances with GRE Score") +xlab("GRE Score")+ylab("Admission Chances")
G3<-ggplot(admissions, aes(x=TOEFL.Score, y=Chance.of.Admit))+geom_point(shape=14, color="green")
G3+ggtitle("Admission Chances with TOEFL Score")+xlab("TOEFL Score")+ ylab("Admission Chances")

#explore the relationship with admission chances with remaining variables, SOP, LOR, University
# rating and research
#converting to factors
names<-c("LOR", "SOP", "University.Rating", "Research")
admissions[,names]<-lapply(admissions[,names], factor)
str(admissions)

#data split for training and testing
library(dplyr)
admissions_train<-sample_frac(admissions, 0.75)
admissions_test<-sample_frac(admissions, 0.25)
str(admissions_train)

#start off with multiple regression (maybe stepwise backwawrd regression)
model1<-lm(Chance.of.Admit ~ GRE.Score+TOEFL.Score+CGPA + Research + LOR + SOP, data=admissions_train)
summary(model1)
#removing all insignificatnt variables
model2<-lm (Chance.of.Admit ~ GRE.Score + TOEFL.Score + CGPA + Research, data=admissions_train)
summary(model2)
residuals(model2)
plot(resid(model2))

anova(model2)

#compare with test data
test_result= predict(model2,newdata=admissions_test)
mse=mean((admissions_test$Chance.of.Admit - predict.lm(model2, admissions_test))^2)
mse
