View(Fraud)
library(C50)
install.packages("caret")
library(caret)
attach(Fraud)

# Data exploration
summary(Fraud)
str(Fraud)

summary(Undergrad)
summary(Marital.Status)
summary(City.Population)
summary(Work.Experience)


# Splitting the data into two categories
frd <- ifelse(Taxable.Income <= 30000,"Risky","Good")
View(frd)

frd <- as.factor(frd)
summary(frd)
str(frd)

Fraud <- Fraud[-3]
Fraud <- cbind(Fraud, frd)
Fraud <- as.data.frame(Fraud)
View(Fraud)
str(Fraud)
attach(Fraud)

# Data partion for model building and testing
inTraininglocal <- createDataPartition(Fraud$frd,p=.75, list=F)
training <- Fraud[inTraininglocal,]
testing <- Fraud[-inTraininglocal,]

#model building
model <- C5.0(training$frd~.,data = training,trails = 400)
?C5.0

# Generating the model summary
summary(model)
pred <- predict.C5.0(model,testing[,-6])
a <- table(testing$frd,pred)
View(a)

# Accuracy
sum(diag(a)/sum(a))
plot(model)

###Bagging####
acc<-c()
for(i in 1:400)
{
  print(i)
  inTraininglocal <- createDataPartition(Fraud$frd,p=.75, list=F)
  training1 <- Fraud[inTraininglocal,]
  testing <- Fraud[-inTraininglocal,]
  
  fittree<-C5.0(training$frd~.,data = training)
  pred<-predict.C5.0(fittree,testing[,-6])
  a<-table(testing$frd,pred)
  acc<-c(acc,sum(diag(a))/sum(a))
  
}
summary(acc)
acc
