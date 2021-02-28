# Import Data 
dat = read.csv("C:/Users/91773/Desktop/MSc Data Analytics/DMML/Project/Adult/adult.csv")

# Check the structure of the data
str(dat)

#Check the summary of the data
summary(dat)

# 1. Remove redundant variables: fnlwgt, education, and relationship
dat = dat[,-c(3,4,8)] 

boxplot(dat$age) 
boxplot(dat$capital.gain) 
boxplot(dat$capital.loss) 
boxplot(dat$hours.per.week)


# 2. Clean the Outliers by using IQR method
## Cleaning outliers in age ##
summary(dat$age)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   17.00   28.00   37.00   38.58   48.00   90.00
Q1_age = 28
Q3_age = 48
IQR_age = Q3_age - Q1_age#IQR = Q3 - Q1
IQR_age
## [1] 20

# Find lowest value (LowerWhisker = Q1 - 1.5 * IQR_age) 
LowerW_age = Q1_age - (1.5*IQR_age)
LowerW_age
## [1] -2

# Find upper value (UpperWhisker = Q3 + 1.5 * IQR_age)
UpperW_age = Q3_age + 1.5 * IQR_age
UpperW_age
## [1] 78

# Find observations above 78 (as UpperW_age =78)
dat = subset(dat, age <= 78)

## Cleaning outliers in education.num ##
summary(dat$education.num)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1.00    9.00   10.00   10.08   12.00   16.00
Q1_education.num  = 9
Q3_education.num  = 12
IQR_education.num = Q3_education.num  - Q1_education.num
IQR_education.num 
## [1] 3

# Find lowest value (LowerWhisker = Q1 - 1.5 * IQR_education.num) 
LowerW_education.num  = Q1_education.num - 1.5*IQR_education.num
LowerW_education.num 
## [1] 4.5

# Find upper value: (UpperWhisker = Q3 + 1.5 * IQR_education.num)
UpperW_education.num  = Q3_education.num  + 1.5*IQR_education.num
UpperW_education.num 
## [1] 16.5

# Find observations below 4.5
dat = subset(dat, educational.num >= 4.5)

## Cleaning outliers in capital.gain ##
library(ggplot2)
summary(dat$capital.gain)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0       0       0    1109       0   99999
box_plot = ggplot(dat, aes(x=capital.gain))+ geom_boxplot()
box_plot
dat = subset(dat, capital.gain < 99999)


# 3. Reclassifying Categorical Variables
## Change the "?" to Unknown ##
dat$occupation = gsub("?", "Unknown", dat$occupation, fixed = T )
dat$occupation = as.factor(dat$occupation)

dat$workclass = gsub("?", "Unknown", dat$workclass, fixed = T )
dat$workclass = as.factor(dat$workclass)

## Reclassify field values ##
## For marital.status ##
dat$marital.status = as.factor(dat$marital.status)
levels(dat$marital.status)
## [1] "Divorced"              "Married-AF-spouse"     "Married-civ-spouse"   
## [4] "Married-spouse-absent" "Never-married"         "Separated"            
## [7] "Widowed"
levels(dat$marital.status)[c(2,3,4)] = 'Married'

## For workclass ##
# Grouping "Federal-gov" "Local-gov", and "State-gov" into "Gov"
levels(dat$workclass)
## [1] "Federal-gov"      "Local-gov"        "Never-worked"     "Private"         
## [5] "Self-emp-inc"     "Self-emp-not-inc" "State-gov"        "Unknown"         
## [9] "Without-pay"
levels(dat$workclass)[c(1,2,7)] = 'Gov'
# Grouping "Self-emp-inc" and "Self-emp-not-inc" into "Self-emp"
levels(dat$workclass)
## [1] "Gov"              "Never-worked"     "Private"          "Self-emp-inc"    
## [5] "Self-emp-not-inc" "Unknown"          "Without-pay"
levels(dat$workclass)[4:5] = 'Self-emp'
levels(dat$workclass)
## [1] "Gov"          "Never-worked" "Private"      "Self-emp"     "Unknown"     
## [6] "Without-pay"

## For native.country ##
t1 = table(dat$native.country) 
prop.table(t1)
dat$native.country = as.factor(dat$native.country)
levels(dat$native.country)[c(28)] = 'United-States'
levels(dat$native.country)[c(1:27,29:41)] = 'Non-U.S.'
levels(dat$native.country)
## [1] "Non-U.S."      "United-States"

## For occupation ##
levels(dat$occupation)
##  [1] "Adm-clerical"      "Armed-Forces"      "Craft-repair"     
##  [4] "Exec-managerial"   "Farming-fishing"   "Handlers-cleaners"
##  [7] "Machine-op-inspct" "Other-service"     "Priv-house-serv"  
## [10] "Prof-specialty"    "Protective-serv"   "Sales"            
## [13] "Tech-support"      "Transport-moving"  "Unknown"
levels(dat$occupation)[c(6,8,9)] = 'Service'
levels(dat$occupation)[c(4,8)] = 'Professional/Managerial'
levels(dat$occupation)[c(1,7)] = 'Administration'
levels(dat$occupation)



library(caret)
library("dplyr")
datnorm <- select_if(dat, is.numeric)
datnorm


# 5. Creating training and test data set
# Divide the dataset into 2 portions in the ratio of 75: 25 for the training and test data set respectively.
DF=dat
set.seed(123)
samp = sample(1:nrow(DF),round(0.75*nrow(DF)))
training = DF[samp,]
test= DF[-samp,]

library(data.table)
library(mltools)
test_1h <- one_hot(as.data.table(test))
train_1h <- one_hot(as.data.table(training))

## For income ##
test_1h$income[test_1h$income == "<=50K"] <- "0"
test_1h$income[test_1h$income == ">50K"] <- "1"
test_copy = test_1h
test_copy$income <- as.factor(test_copy$income)
test_1h$income <- as.integer(test_1h$income)

train_1h$income[train_1h$income == "<=50K"] <- "0"
train_1h$income[train_1h$income == ">50K"] <- "1"
train_copy = train_1h
train_copy$income <- as.factor(train_copy$income)
##train_1h$income <- as.integer(train_1h$income)

actual_train <- train_copy$income
actual <- test_copy$income
test_data <- subset (test_1h, select = -income)



#####################################################################################################
## Naive Bayes ## RAN SUCCESSFULLY
#####################################################################################################
library(arules)
library(data.tree)
library(caret)
library(e1071)

library(naivebayes)


census.model <- naiveBayes(income ~ .,data = train_copy)
census.out <- predict(census.model,test_copy)

census.confusion.matrix <- table(census.out, actual)
census.model.tp <- census.confusion.matrix[1]
census.model.tn <- census.confusion.matrix[4]
census.model.fp <- census.confusion.matrix[3]
census.model.fn <- census.confusion.matrix[2]
census.model.accuracy <- (census.model.tp+census.model.tn)/
  (census.model.tp+census.model.tn+census.model.fp+census.model.fn)
census.model.recall <- (census.model.tp)/(census.model.tp+census.model.fn)
census.model.precision <- (census.model.tp)/(census.model.tp+census.model.fp)
census.model.f1.score <- 2*((census.model.precision*census.model.recall)/
                              (census.model.precision+census.model.recall))

confusionMatrix(census.out, actual)

#####################################################################################################

#####################################################################################################
## Logistic Regression ## RAN SUCCESSFULLY
#####################################################################################################


### logistic regression for predicting "income" OLD
glm.fit <- glm(income ~. , data=train_copy, family=binomial)
summary(glm.fit)
glm.probs <- predict(glm.fit, newdata=test_copy, type="response")
glm.pred <- ifelse(glm.probs>0.5, 1, 0)
glm.pred <- as.factor(glm.pred)
confusionMatrix(glm.pred, actual) ####
### logistic regression for predicting "income" OLD


##Fitting a logistic model

model.logit=glm(income~.,data=train_copy,family="binomial")

summary(model.logit)

#For test set
pred.logit=predict(model.logit,type="response",newdata = test_copy)

pred.def=ifelse(pred.logit>0.5,"1","0")

table(predict=pred.def,true=test_copy$income)
err2[1]=(871+209)/6000

#For training set
pred.def=ifelse(predict(model.logit,type="response",newdata = train_copy)>0.5,"1","0")

table(predict=pred.def,true=train_copy$income)
err1[1]=(3395+874)/24000

##Ploting ROC curve and AUC for test and train set

par(mfrow=c(1,2))
par(pty="s")


library(pROC)
library(ROCR)

#For training set
roc(train_copy$income,model.logit$fitted.values,plot=T,col="#69b3a2",print.auc=T,legacy.axes=TRUE,percent = T,
    xlab="False Positive percentage",ylab="True Positive percentage",lwd=5,main="Train Set")

#For test set
roc(test_copy$income,pred.logit,plot=T,col="navyblue",print.auc=T,legacy.axes=TRUE,percent = T,
    xlab="False Positive percentage",ylab="True Positive percentage",lwd=5,main="Test Set")


#Cumulative gain chart for test set
area_mod=performance(prediction(sort(pred.logit,decreasing=F),test_copy$income[order(pred.logit,decreasing=F)])
                     ,measure = "auc")@y.values[[1]] #Area under curve

x=unlist(slot(performance(prediction(sort(pred.logit,decreasing=F),test_copy$income[order(pred.logit,decreasing=F)])
                          ,"tpr","rpp"),'x.values'))
y=unlist(slot(performance(prediction(sort(pred.logit,decreasing=F),test_copy$income[order(pred.logit,decreasing=F)])
                          ,"tpr","rpp"),'y.values'))
par(mfrow=c(1,1))
par(pty="m")
plot(x,y,type="l",col="green",lwd=4,main="Cumulative gain chart for Logit model",font.main=2,xlab="Rate of Positive Prediction",
     ylab="Cumulative proportion of target")
abline(a=0,b=1,col="red")
text(0.8,0.2,"Area under model\n curve is 0.8960",font = 2,col="steelblue4")
text(0.8,0.69,"<-base line")
lines(x=c(0,0.2166667,1),y=c(0,1,1),lwd=2,col="blue")
#####################################################################################################




