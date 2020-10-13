View(Comp)
library(C50)
install.packages("caret")
library(caret)
library(moments)
attach(Comp)

# Data exploration
summary(Comp)
str(Comp)

# Graphical exploration
hist(Sales)
summary(Sales)
skewness(Sales)
kurtosis(Sales)
qqnorm(Sales)
qqline(Sales)

hist(CompPrice)
summary(CompPrice)
skewness(CompPrice)
kurtosis(CompPrice)
qqnorm(CompPrice)
qqline(CompPrice)

hist(Income)
summary(Income)
skewness(Income)
kurtosis(Income)
qqnorm(Income)
qqline(Income)

hist(Price)
summary(Price)
skewness(Price)
kurtosis(Price)
qqnorm(Price)
qqline(Price)

# Splitting the Sales into two categories
sale <- ifelse(Sales <= 10,"Low","High")
View(sale)

Comp1 <- Comp[-1]
Comp1 <- cbind(Comp1,sale)
View(Comp1)

# Data partion for model building and testing
inTraininglocal <- createDataPartition(Comp1$sale,p=.75, list=F)
training <- Comp1[inTraininglocal,]
testing <- Comp1[-inTraininglocal,]

#model building
model <- C5.0(training$sale~.,data = training,trails = 400)
?C5.0

# Generating the model summary
summary(model)

# Prediction
pred <- predict.C5.0(model,testing[,-11])

# Accuracy
a <- table(testing$sale,pred)
View(a)
sum(diag(a)/sum(a))
plot(model)

###Bagging####
acc<-c()
for(i in 1:400)
{
  print(i)
  inTraininglocal <- createDataPartition(Comp1$sale,p=.75, list=F)
  training1 <- Comp1[inTraininglocal,]
  testing <- Comp1[-inTraininglocal,]
  
  fittree<- C5.0(training1$sale~.,data = training)
  pred<-predict.C5.0(fittree,testing[,-11])
  a<-table(testing$sale,pred)
  acc<-c(acc,sum(diag(a))/sum(a))
  
}
summary(acc)
acc
