setwd('C:/Users/kiran.m/Desktop/hack/Hack/File')
source<-read.csv('C:/Users/kiran.m/Desktop/hack/Hack/File/hack.csv')
install.packages('CARET')
install.packages('ggplot2')
library(caret)
library(ggplot2)
library(plotly)
library(e1071)
set.seed(3456)
inTrain <- createDataPartition(source$Attrition,p=0.7,list = FALSE)
TrainingData <- source[inTrain,]
TestingData <- source[-inTrain,]
k<-ggplot(TrainingData,aes(Attrition,fill=Attrition))+geom_bar()+ggtitle("Count of Attrition")
ggplotly(k)


##Random Forest
mod_RF <- randomForest(Attrition ~ .,TrainingData)
plot(mod_RF, main = "Model ")

pred_RF <- predict(mod_RF, TestingData, type="class")
confusionMatrix(pred_RF, TestingData$Attrition)
p <- predict(mod_RF, TestingData)
head(p,10)

##########RF--Training data
mod_RF <- randomForest(Attrition ~ .,TrainingData)
plot(mod_RF, main = "Model ")

pred_RF <- predict(mod_RF, TestingData, type="class")
confusionMatrix(pred_RF, TestingData$Attrition)
p <- predict(mod_RF, TestingData)
head(p,10)


##RF-- Testing data

mod_RF <- randomForest(Attrition ~ .,TestingData)
plot(mod_RF, main = "Model ")

pred_RF <- predict(mod_RF, TrainingData, type="class")
confusionMatrix(pred_RF, TrainingData$Attrition)
p <- predict(mod_RF, TrainingData)
head(p,10)



###lr
mod_lgr<-glm(Attrition ~ .,data=subset(TrainingData, select=c( -Over18 ) ) , family = 'binomial')
mod_lgr
summary(mod_lgr)
precdict_glm<-predict(mod_lgr,TestingData, type = "response")
precdict_glm
head(precdict_glm)


mod_LR_exp <- coef(summary(mod_lgr))
mod_LR_exp
str(source)


df<- data.frame(TestingData$employeeid,pp=predict(mod_lgr,TestingData, type = "response"))
df


write.table(df, file = "C:/Users/kiran.m/Desktop/hack/Hack/MyData.csv",row.names=F, na="",col.names=T, sep=",")
            
  head(df,4)

m<-max(df)
m
####Relations

#Attrition vs BusinessTravel

travelPlot <- ggplot(TrainingData,aes(Attrition,fill=BusinessTravel))+geom_bar(stat="identity")
ggplotly(travelPlot)

travelPlo<- ggplot(TrainingData,aes(BusinessTravel,fill=Attrition))+geom_bar()+ggtitle("Attrition Vs BusinessTravel")
ggplotly(travelPlo)

#Attrition vsdailyrate
library(sqldf)
drr<-sqldf("select distancefromhome, (case when DistanceFromHome between 0 and 5 then '0-5'
when DistanceFromHomie between 6 and 10 then '6-10' 
when DistanceFromHome between 11 and 15 then '11-15' 
when DistanceFromHome between 16 and 20 then '16-20' 
when DistanceFromHome between 21 and 25 then '21-25' 
when DistanceFromHome between 26 and 30 then '26-30' 
end
 ) from source ")


##DistanceFromHome

Dr<-ggplot(TrainingData,aes(DistanceFromHome,fill=Attrition))+geom_bar()+ggtitle("DistanceFromHome VS Attrition")
ggplotly(Dr)


#Education 
edu<-ggplot(TrainingData,aes(Education ,fill=Attrition))+geom_bar()
ggplotly(edu)

#EnvironmentSatisfaction
env<-ggplot(TrainingData,aes(EnvironmentSatisfaction ,fill=Attrition))+geom_histogram()
ggplotly(env)

#Gender
gen<-ggplot(TrainingData,aes(Gender,fill=Attrition))+geom_bar()
ggplotly(gen)

#JobRole

jr<-ggplot(TrainingData,aes(JobRole,fill=Attrition))+geom_bar()
ggplotly(jr)

#JobSatisfaction
js<-ggplot(TrainingData,aes(JobSatisfaction,fill=Attrition))+geom_bar()
ggplotly(js)


#MaritalStatus
Ms<-ggplot(TrainingData,aes(MaritalStatus,fill=Attrition))+geom_bar()
ggplotly(Ms)

#NumCompaniesWorked
No.CW<-ggplot(TrainingData,aes(NumCompaniesWorked,fill=Attrition))+geom_bar()
ggplotly(No.CW)

#OverTime
ot<-ggplot(TrainingData,aes(OverTime,fill=Attrition))+geom_bar()
ggplotly(ot)

#RelationshipSatisfaction 
Rs<-ggplot(TrainingData,aes(RelationshipSatisfaction ,fill=Attrition))+geom_bar()
ggplotly(Rs)

#StockOptionLevel
Sl<-ggplot(TrainingData,aes(StockOptionLevel,fill=Attrition))+geom_bar()
ggplotly(Sl)

#TotalWorkingYears 
TWY<-ggplot(TrainingData,aes(TotalWorkingYears ,fill=Attrition))+geom_bar()
ggplotly(TWY)

#TrainingTimesLastYear
TTLY<-ggplot(TrainingData,aes( TrainingTimesLastYear,fill=Attrition))+geom_bar()
ggplotly(TTLY)

#WorkLifeBalance
WLB<-ggplot(TrainingData,aes( WorkLifeBalance,fill=Attrition))+geom_bar()
ggplotly(WLB)

#YearsAtCompany
YC<-ggplot(TrainingData,aes( YearsAtCompany,fill=Attrition))+geom_bar()
ggplotly(YC)

#YearsSinceLastPromotion 
LP<-ggplot(TrainingData,aes( YearsSinceLastPromotion ,fill=Attrition))+geom_bar()
ggplotly(LP)

#employeeid  
EMP<-ggplot(TrainingData,aes(  Attrition,alpha=employeeid))+geom_bar()
ggplotly(EMP)


