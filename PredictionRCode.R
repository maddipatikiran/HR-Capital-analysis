#Source 
setwd('C:/Users/kiran.m/Desktop/hack/Hack/File')
HR_DataSet<-read.csv("HRDataSet.csv",header = TRUE,na.strings=c(""))



# Packages Used 
library(caret)
library(ggplot2)
library(plotly)
library(sqldf)

s<-sqldf('select * from HR_DataSet where JobRole="Manager"')
#DataPartition 
set.seed(3456)
intrain<-createDataPartition(HR_DataSet$Attrition,p=0.7,list=FALSE)
TrainingDataSet<-HR_DataSet[intrain,]
TestingDataSet<-HR_DataSet[-intrain,]

#Logistic regressionModel
Logistic_Regression_Model<-glm(Attrition ~ BusinessTravel+EnvironmentSatisfaction+Gender+MaritalStatus+OverTime+StockOptionLevel+
            JobSatisfaction+employeeid+DistanceFromHome+JobLevel
        +RelationshipSatisfaction+Benefits+ExtracurricularActivities,family='binomial',data=TrainingDataSet)
summary(Logistic_Regression_Model)

PredictedProbability<-predict(Logistic_Regression_Model,TestingDataSet,type='response')

EmployeeProbability<- data.frame(empid=TestingDataSet$employeeid,PredictedProbability)

EmployeeProbability

HR_Data_Prediction<-sqldf('
                           SELECT employeeid
                         	, Age
                          , PredictedProbability AS Probability_to_Left
                          , Attrition
                          , CASE WHEN PredictedProbability >= 0.5 THEN 1 ELSE 0 END AS Predicted_Status
                          , BusinessTravel
                          , Department
                          , DistanceFromHome
                          , EducationField
                          , EnvironmentSatisfaction
                          , Gender
                          , JobInvolvement
                          , JobLevel
                          , JobRole
                          , JobSatisfaction
                          , MaritalStatus
                          , NumCompaniesWorked
                          , Over18
                          , OverTime
                          , PercentSalaryHike
                          , PerformanceRating
                          , RelationshipSatisfaction
                          , StockOptionLevel
                          , TotalWorkingYears
                          , TrainingTimes
                          , WorkLifeBalance
                          , YearsAtCompany
                          , YearsInCurrentRole
                          , YearsSinceLastPromotion
                          , YearsWithCurrManager
                          , YearOfJoining
                          , TechnologyAdoption
                          , Benefits
                          , Facilities
                          , ChallengingLevels
                          , ExtracurricularActivities
                          , YearOfLeaving
                          FROM TestingDataSet
                          INNER JOIN EmployeeProbability ON TestingDataSet.employeeid = EmployeeProbability.empid
           ')
HR_Data_Prediction

Predicted_Attrition<-ifelse(PredictedProbability>=0.5,1,0)

Predicted_Attrition

#Confusion Matrix
ConfusionMatrix<-table(predicted=Predicted_Attrition,Actual=TestingDataSet$Attrition)

ConfusionMatrix


#Accuracy
Accuracy<-(sum(diag(ConfusionMatrix))/sum(ConfusionMatrix))*100


Accuracy
