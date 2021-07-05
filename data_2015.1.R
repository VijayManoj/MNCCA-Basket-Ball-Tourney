#Reading the CSV File of 2015
data_2015.1<-read.csv("MEvents2015.csv")



#To Find the Dimensions of the Data
dim(data_2015.2)


#To find the names of the columns 
colnames(data_2015.1)

#To View the Data

View(data_2015.1)

#Removing the Unwanted Columns 

data_2015.1<-data_2015.1[,-17]

data_2015.1<-data_2015.1[,-16]

data_2015.1<-data_2015.1[,-15]


data_2015.1<-data_2015.1[,-14] #This is EventSubtype having more NA values 

#Point for the Team based on Goals
#Made1-1
#Made2-2
#Made3-3


#Removing unwanted Rows based on Event Type beacuse the score is increased will be only made1,made2,made3 and Current Scores of Respected Team


install.packages("car")

library(car)

EventSubtype<-recode(data_2015.1$EventType,"'block'=1;'reb'=1;'steal'=1;'assist'=1;'foul'=1;'timeout'=1;'sub'=1;'turnover'=1;'miss1'=1;'miss2'=1;'miss3'=1");


data_2015.1$EventType<-EventSubtype


a=which(data_2015.1$EventType==1)


data_2015.1<-data_2015.1[-a,]

#Rows are not removed based on the EventSub type they are removed based on the Eventtype and Current Score of repected Team

#Knowing the values of the Winnnig Current Score and Loosing Current Score Equal to Winning Final Score and Loosing Final Score


#Basic one to check our predections Linear Regression 


#Predection for the Winnnig Final Score 

d1<-data_2015.1$WFinalScore


d2<-data_2015.1$WCurrentScore

l1<-lm(d1~d2)

a=data.frame(d2=116)

b<-predict(l1,a)


summary(l1)

#Trainig and Testing for Predecting the Winning Final Score

#WinningTeam 

# Taking the Columns which are needed to Predict the Wining Team Final Score Columns are WiningTeamID-WTeamID, LosingTeamID-LTeamID, WiningFinalScore-WFinalScore, Time-ElapsedSeconds, GoalDoneByTema-EventTeamID

wining<-data_2015.1[1:50000,6] 

wscore<-data_2015.1[1:50000,8]

WID<-data_2015.1[1:50000,4]

e<-data_2015.1[1:50000,10]

LID<-data_2015.1[1:50000,5]

#Training Data

train_1<-data_2015.1[1:50000,c(6,8,4,10,5)]

a<-lm(wining~wscore+WID+e+LID,data=train1)

#Summary
summary(a)

#Testing Data 

test_1<-data_2015.1[50001:80000,c(6,8,4,10,5)]


f=predict(a,test1)


#By Taking Eventtype
# Taking the Columns which are needed to Predict the Wining Team Final Score Columns are WiningTeamID-WTeamID, LosingTeamID-LTeamID, WiningFinalScore-WFinalScore, Time-ElapsedSeconds, GoalDoneByTema-EventTeamID,Eventtype-event

wining<-data_2015.1[1:50000,6]

wscore<-data_2015.1[1:50000,8]

WID<-data_2015.1[1:50000,4]

e<-data_2015.1[1:50000,10]

LID<-data_2015.1[1:50000,5]

event<-data_2015.1[1:50000,13]

#Training Data 
train1<-data_2015.1[1:50000,c(6,8,4,10,5,13)]

a<-lm(wining~wscore+WID+e+LID+event,data=train1)

#Summary 
summary(a)

#Testing Data 

test1<-data_2015.1[50001:80000,c(6,8,4,5)]

w1<-data_2015.1[50001:80000,6]

f=predict(a,test1)

#By Including the EventType the Multiple R-Squared value has been  same no change when we included the EventType


#Predecting the Loosing Final Score

#Loosing Team


loosing<-data_2015.1[c(1:50000),7] # Losing Team Final Score 

lscore<-data_2015.1[c(1:50000),9] #Losing Team Current Score 


WID<-data_2015.1[c(1:50000),4] #Wining Team ID  

LID<-data_2015.1[c(1:50000),5] #Losing Team ID 

e1<-data_2015.1[c(1:50000),10]  #Elapsed Time 

e2<-data_2015.1[c(1:50000),13] #Event Type 

#Training Data 
train2<-data_2015.1[c(1:50000),c(6,8,4,5,10,13)]



b<-lm(loosing~lscore+WID+LID+e1+e2,data=train2)

#Summary 
summary(b)

#Testing Data

test2<-data_2015.1[c(50001:80000),c(6,8,4,5,10,13)]

#Predicting Values 

g=predict(b,test2)



#Decision Tree Machine Learning Algorithm

#Final Score for Wining Team 

install.packages("caTools")

library(caTools)

# Taking the Columns which are needed to Predict the Wining Team Final Score Columns are WiningTeamID-WTeamID, LosingTeamID-LTeamID, WiningFinalScore-WFinalScore, Time-ElapsedSeconds, GoalDoneByTema-EventTeamID


wining<-data_2015.1[50000:80000,6]

wscore<-data_2015.1[50000:80000,8]

WID<-data_2015.1[50000:80000,4]

e<-data_2015.1[50000:80000,10]

LID<-data_2015.1[50000:80000,5]

#Training Data
train_2<-data_2015.1[50000:80000,c(6,8,4,10,5)]

#Testing Data 
test_2<-data_2015.1[90000:100000,c(6,8,4,10,5)]

library(rpart)

#Applying Machine Learning Algorithm

t<-rpart(formula=train_2$WFinalScore~+train_2$WTeamID+train_2$LTeamID+train_2$WCurrentScore+train_2$ElapsedSeconds)

#Predicting Values 

t1<-predict(t,data=test_2)


install.packages("rpart.plot")

#Installing Libraries


library(rpart.plot)

rpart.plot(t,nn=TRUE)




#Decision Tree Machine Learning Algorithm

#Final Score for Losing  Team 


install.packages("caTools")

library(caTools)

# Taking the Columns which are needed to Predict the Wining Team Final Score Columns are WiningTeamID-WTeamID, LosingTeamID-LTeamID, LosingFinalScore-LFinalScore, Time-ElapsedSeconds, GoalDoneByTema-EventTeamID



loosing<-data_2015.1[c(50000:100000),7] # Loosing Team Final Score 

lscore<-data_2015.1[c(50000:100000),9] #Loosing Team Current Score 


WID<-data_2015.1[c(50000:100000),4] #Wining Team ID  

LID<-data_2015.1[c(50000:100000),5] #Loosing Team ID 

e1<-data_2015.1[c(50000:100000),10]  #Elapsed Time 


#Training Data
train_3<-data_2015.1[50000:100000,c(7,9,4,5,10)]

#Testing Data 
test_3<-data_2015.1[100000:130000,c(7,9,4,5,10)]

library(rpart)

#Applying Machine Learning Algorithm

t<-rpart(formula=train_3$LFinalScore~+train_3$LCurrentScore+train_3$WTeamID+train_3$LTeamID+train_3$ElapsedSeconds)

#Predicting Values 

t1<-predict(t,data=test_3)


install.packages("rpart.plot")

#Installing Libraries


library(rpart.plot)

rpart.plot(t,nn=TRUE)


#Sir we have calculated the accuracy for Decision Tree based on the Sample of WFinalScore and WLoosingFinalScore given in the Data  and we cheked with the Decision Tree graph based on rpart.plot()


#Both the Machine Learning Algorithm have given the at most Same but Decision Tree is some what better. So, Decision Tree is little better Machine Learning Algorithm. 

