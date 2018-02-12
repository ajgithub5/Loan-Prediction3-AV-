loantrain<-read.csv("Y:\\Projects\\Loan Prediction 3 - AV HACK\\train_loan.csv")
loantest<-read.csv("Y:\\Projects\\Loan Prediction 3 - AV HACK\\test_loan.csv")

View(loantrain)
str(loantrain)

summary(loantrain)
summary(loantest)

suppressMessages(library(Mice))
loantrain$Credit_History<-factor(loantrain$Credit_History)
loantest$Credit_History<-factor(loantest$Credit_History)

loantrain$Dependents<-as.character(loantrain$Dependents)
loantrain[loantrain$Dependents=="3+",4]="3"
loantrain$Dependents<-as.numeric(loantrain$Dependents)

loantest$Dependents<-as.character(loantest$Dependents)
loantest[loantest$Dependents=="3+",4]="3"
loantest$Dependents<-as.numeric(loantest$Dependents)

loantrain$Loan_ID<-NULL
loantest$Loan_ID<-NULL

loantrain$Gender[loantrain$Gender==""]<-NA
loantrain$Married[loantrain$Married==""]<-NA
loantrain$Self_Employed[loantrain$Self_Employed==""]<-NA
loantest$Gender[loantest$Gender==""]<-NA
loantest$Married[loantest$Married==""]<-NA
loantest$Self_Employed[loantest$Self_Employed==""]<-NA

loantrain$Married<-droplevels(loantrain$Married)
summary(loantrain)

#outlier graphically
ggplot(loantrain,aes(Loan_Status,ApplicantIncome))+geom_boxplot()
ggplot(loantrain,aes(Loan_Status,CoapplicantIncome))+geom_boxplot()
ggplot(loantrain,aes(Loan_Status,LoanAmount))+geom_boxplot()

#There must be one or more extreme value of applicants' income
#More than 25% of the applicants do not have coaaplicant
#The applicants has trends toward male, graduate, or self-employed
#Most applicants' credit history meet the guidlines
#The response variable, loan status is not balanced with a ratio of about 2:1
#There are variables that have missing values in data

#missing value treatment
loantrain_NAs<-NULL
loantest_NAs<-NULL
for(i in 1:nrow(loantrain)) loantrain_NAs[i]<-sum(is.na(loantrain[i,]))
loantrain_NAs$nos<-loantrain_NAs
loantest$Loan_Status<-NULL
for(i in 1:nrow(loantest)) loantest_NAs[i]<-sum(is.na(loantest[i,]))
loantest_NAs$nos<-loantest_NAs

names<-names(loantrain)
names<-data.frame(variable=names,missing_proportion=sapply(names,function(x)
  sum(is.na(loantrain[x]))/nrow(loantrain)))
names    #none of the proportion is more than 10% for missing values

#imputing missing values
#install.packages('mice')
#install.packages('VIM')

library(mice)
md.pattern(loantrain) #use mice package to check missing values pattern

library(VIM)   #use to plot the pattern of missing values
mice_plot <-aggr(loantrain, col=c('navyblue','red'), numbers=TRUE, 
                  sortVars=TRUE, labels=names(loantrain), cex.axis=.7, 
                  gap=3, ylab=c("Histogram of missing data","Pattern"))


#input missing values by package mice, considering that the missing values 
#are not MNAR(missing not at random) .They are MCAR(missing completely at random)

loantrainimp<-mice(data=loantrain,m=5,maxit=50,method='pmm',printflag=FALSE,seed=0817)
#estimate fitting values for continuous variables using predictive mean matching of mice

methods(mice)

loantrain1<-complete(loantrainimp,2) #imput the estimated missing values
sum(is.na(loantrain1)) #all missing value are imputed

loantestimp<-mice(data=loantest,m=5,maxit=50,method='pmm',printflag=FALSE,seed=0817)
loantest1<-complete(loantestimp,2)
sum(is.na(loantest1))

#plots to check distribution imputed values vs existing values
densityplot(loantrainimp)
stripplot(loantrainimp,pch=20,cex=1.2)

#EDA 
#Univariate Analysis
par(mfrow=c(2,2))
hist(loantrain1$Dependents) # most of the applicants dont have any applicants
hist(loantrain1$ApplicantIncome) #Applicant Income is extremely right skewed
hist(loantrain1$CoapplicantIncome) #CoApplicant Income is extremely right skewed
plot(as.factor(loantrain1$Loan_Amount_Term)) #85% of the applied loans are 360 terms.

#Feature Engineering. Applicantincome divided by loan. Same for Coapplicantincome
loantrain1$abl<-loantrain1$ApplicantIncome/loantrain1$LoanAmount
loantrain1$cbl<-loantrain1$CoapplicantIncome/loantrain1$LoanAmount
loantest1$abl<-loantest1$ApplicantIncome/loantest1$LoanAmount
loantest1$cbl<-loantest1$CoapplicantIncome/loantest1$LoanAmount

hist(loantrain1$abl) #right skewed
hist(log10(loantrain1$abl)) #taking log transformation
hist(loantrain1$cbl)
hist(log10(loantrain1$cbl))

loantrain1$logabl<-log10(loantrain1$abl)  #log tranformation for normal distribution
loantest1$logabl<-log10(loantest1$abl)

emical<-function(P,n,r){
  r=r/1200
  emi=(P*r*(1+r)**n)/(((1+r)**n)-1)
  return(emi)
}

loantrain1$emi<-emical(loantrain1$LoanAmount,loantrain1$Loan_Amount_Term,10)
loantest1$emi<-emical(loantest1$LoanAmount,loantest1$Loan_Amount_Term,10)

#Creating a dummy variable of whether coapplicant is there or not
loantrain1$coapp<-NA
for(i in 1:nrow(loantrain1)){
  if(loantrain1$CoapplicantIncome[i]>0){
    loantrain1$coapp[i]<-1
  }
  else
    loantrain1$coapp[i]<-0
}

loantest1$coapp<-NA
for(i in 1:nrow(loantest1)){
  if(loantest1$CoapplicantIncome[i]>0){
    loantest1$coapp[i]<-1
  }
  else
    loantest1$coapp[i]<-0
}

#arranging the columns in the same order in train and test
suppressMessages(library(dplyr))
loantrain1<-loantrain1%>%select(-Loan_Status,everything())

#Built Model
#Without tuning and with original dataset

library(randomForest)
set.seed(817)
original_rf<-randomForest(Loan_Status~.,loantrain1[,-c(12:16)])
original_rf #OOB error without tuning is 20.03%

#rf with featured engineering without tuning
featured_rf<-randomForest(Loan_Status~.,loantrain1)
featured_rf #OOB error improved to 19.22%

#rf with tuning parameters feature engineering.
suppressMessages(library(gridExtra))
suppressMessages(library(Hmisc))

set.seed(817)
tunegrid<-expand.grid(mtry=c(1:10),ntree=c(500,1000,1500,2000))

mtry<-tunegrid[[1]]   
ntree<-tunegrid[[2]]
OOB<-NULL

for(i in 1:nrow(tunegrid)){
  rf<-randomForest(Loan_Status~.,loantrain1,mtry=mtry[i],ntree=ntree[i])
  confusion<-rf$confusion
  temp<-(confusion[2]+confusion[3])/614
  OOB<-append(OOB,temp)
  }
tunegrid$OOB<-OOB

head(tunegrid[order(tunegrid["OOB"]), ], 5) 
#best model is with (mtry3 & ntree1500) with 18.89% OOB

finalrf<-randomForest(Loan_Status~.,loantrain1,mtry=3,ntree=2000)
finalrf

#prediction rf on testset
predictrf<-predict(finalrf,loantest1[])




