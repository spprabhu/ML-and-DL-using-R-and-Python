#############################################################




## Remove the existing variables
rm(list = ls(all=T))

## Load the required libraries'
library(DMwR)
library(MASS)
library(caret)
library(glmnet)
library(forecast)
library(dplyr)
library(data.table)
library(doParallel)
library(TTR)

## Set Working directory
setwd("F:/DataAnalytics/INSOFE/Tests/CUTe2/Rcode")

## Import the data
train <- read.csv("data.csv",header = T,stringsAsFactors = F)
test <- read.csv("test.csv",header = T,stringsAsFactors = F)

## Check for the str of the data
str(train)

## Check for the number of missing values in the train and test data
sum(is.na(train))
sum(is.na(test))


## Delete columns from test and train data which have more than 30% missing values
threshhold <- 30
for (i in names(train)){
  perc_missing <- sum(is.na(train[,i])/nrow(train)*100)
  if(perc_missing > threshhold)
  {
    train[i] <- NULL
    test[i] <- NULL
  }
}
rm(threshhold)

## Again check the missing values in the data
sum(is.na(train))

sum(is.na(train$f_25))
sum(is.na(train$f_47))
sum(is.na(train$f_47) & is.na(train$f_25)) ##  --> Both have missing values in the same rows

## Delete the rows from the train data which have more than 15% missing values in them 
train_new<- train[-manyNAs(train,0.15),]

## Check the number of missing values
sum(is.na(train_new))
miss_col(train_new)

###  --> As the column names are anonymous, Perfoming Exploratory Data Analysis won't help much in imputing missing values. ###

## Replace missing values using KNN imputation technique
train_without_target <- train_new[!(names(train_new) %in% c("y1","y2"))] ## Removing target for KNN Imputation
train_imputed <- knnImputation(data = train_without_target,scale = T)

## Attach the target variable to the imputed values dataframe
train_imputed_withy1 <- cbind(train_imputed,train_new$y1) 
train_imputed_withy2 <- cbind(train_imputed,train_new$y2)
names(train_imputed_withy1)[names(train_imputed_withy1)=="train_new$y1"] <- "y1"
names(train_imputed_withy2)[names(train_imputed_withy2)=="train_new$y2"] <- "y2"

## Check the summary for finding outliers
summary(train_imputed[1:20])
summary(train_imputed[21:40])
summary(train_imputed[41:60])
summary(train_imputed[61:80])
summary(train_imputed[81:96]) ##  --> There are no outliers

######################################## Basic Linear Regression ####################################################

### Linear models
par(mar=rep(2,4))
hist(train_imputed_withy1$y1)
# --> It is normally distributed. Hence, Transformation is not required

#remove the timestamp
train_withouttimestamp <- train_imputed_withy1[,-1]

## Building linear model
model_linear_basic <- lm(y1~.,data = train_withouttimestamp)
summary(model_linear_basic) # --> Adjusted R-squared:  0.06891
# --> There are many insignificant variables in the model
# --> The adjusted R-squared value is very less.

## Perform the step wise regression to remove insignificant variables
stepAIC(object = model_linear_basic)

## New model as suggested by AIC procedure
model_linear_basic_AIC <- lm(formula = y1 ~ d_0 + d_1 + f_2 + f_5 + f_7 + f_10 + f_11 + 
                               f_14 + f_18 + f_19 + f_21 + f_22 + f_23 + f_25 + f_29 + f_30 + 
                               f_32 + f_34 + f_36 + f_37 + f_41 + f_42 + f_43 + f_47 + f_48 + 
                               f_50 + f_51 + f_52 + f_55 + f_56 + f_58 + f_60 + f_62 + t_0 + 
                               t_1 + t_9 + t_10 + t_13 + t_16 + t_19 + t_20 + t_21 + t_24 + 
                               t_27 + t_30 + t_31 + t_32 + t_33 + t_34 + t_37 + t_38 + t_40 + 
                               t_42 + t_43, data = train_withouttimestamp)

summary(model_linear_basic_AIC) # --> Adjusted R-squared:  0.08083
#  --> Even After performing stepwise regression, Adjusted R-squared value is very less.


## Perform the PCA
pca1 <- prcomp(train_withouttimestamp[!(names(train_withouttimestamp) == "y1")],center = T,scale. = T)
summary(pca1) # 2 components explain about 98% variance 
pca1$rotation
screeplot(pca1,type="l",npcs = 50) # --> There is shoulder at second component

df_pca1 <- data.frame(pca1$x[,c(1:10)],y1=train_withouttimestamp$y1)
head(df_pca1)
model_linear_basic_pca <- lm(y1~.,df_pca1) ## Adjusted R-squared
summary(model_linear_basic_pca) # --> Adjusted R-squared:  0.0001 

############################################### Regularised Linear models ############################################
## Data Preparation
x1=model.matrix(train_withouttimestamp$y1~.,train_withouttimestamp)

## Lasso Model
fit.cv.lasso <- cv.glmnet(x = x1,y = train_withouttimestamp$y1,type.measure="mae", alpha=1,family="gaussian",nfolds=5,parallel=TRUE)

## Ridge Model
fit.cv.ridge <- cv.glmnet(x = x1,y = train_withouttimestamp$y1,type.measure="mae", alpha=0,family="gaussian",nfolds=5,parallel=TRUE)

# Prediction on the test data
test_new <- test
test_new$y1 <- rep(0,30)
x.test2 = model.matrix(test_new$y1~.,test_new[,-1])
pred.lasso.y1 <- predict(fit.cv.ridge,x.test2,s = fit.cv.lasso$lambda.min)

################################################ Time Series ##############################################

## Removing the independent variables
train_timeseries <- train[names(train) %in% c("timestamp","y1")]
head(train_timeseries)

# Check for missing values
min_stamp = min(train_timeseries$timestamp)
max_stamp = max(train_timeseries$timestamp)
seq = data.frame("seq"=seq(min_stamp,max_stamp,1))
setdiff(seq$seq,train_timeseries$timestamp) ##0 
# --> So there are no missing values in the timestamp
# --> That is time stamp is continuous from minimum to maximum values

## Convert the data in time series
ts.train <- ts(train_timeseries$y1,frequency = 365)

## Let us decompose and check the data
plot(decompose(ts.train)) ## No significant trend and seasonality

## Check the data
plot(ts.train)
par(mfrow=c(1,2))
Acf(ts.train,lag.max = 100)
Pacf(ts.train,lag.max = 100) 
## Trend is not Significant
## Seasonality is not important

## Check the number of seasonal and non-seasonal differences required to make the data stationary
ndiffs(ts.train)
nsdiffs(ts.train)

## Let us build the ARIMA model 
ARIMA1 <- auto.arima(y = ts.train,ic = "aic")
summary(ARIMA1)
# ARIMA(0,0,0)

pred.ARIMA1 <- forecast(ARIMA1,h = 30)
par(mfrow=c(1,1))
plot(pred.ARIMA1)

# Box-Ljung test
Box.test(ARIMA1$residuals, lag = 1) ## p-value 0.9774 ## Residuals are not significant

## Holt Winters model
Model_HoltWinters1 <- HoltWinters(x = ts.train,alpha = NULL, beta = NULL,gamma = NULL,seasonal = "additive")
Model_HoltWinters1

# Smoothing parameters:
# alpha: 0.001524162
# beta : 0.005198288
# gamma: 0.5605847

pred.Holt1 <- forecast(Model_HoltWinters1,30)

### Moving average method
fitwma = WMA(ts.train, n=10, 1:10)
predwma = forecast(fitwma[!is.na(fitwma)], h=30)
plot(predwma)

######################################## Models for volatility - Logistic Regression (Regularised #########################################

## Y2
## Removing y1 from train_imputed1
train_imputed1 <- train_imputed_withy2
test1 <- test
test1$y2 <- rep(0,30)

## Converting y2 to a factor variable
train_imputed1$y2<- as.factor(as.numeric(train_imputed1$y2))

## Trying Regularization
#Everything on the Train data
preProc_train<-preProcess(train_imputed1[,setdiff(names(train_imputed1),"y2")])
preProc_train

train_rows_new<- predict(preProc_train,train_imputed1)
train_rows_new$timestamp<- NULL

# Create the test and train data
rows<- createDataPartition(train_rows_new$y2,p = 0.7,list=F)
sub.train<- train_rows_new[rows,]
sub.test<- train_rows_new[-rows,]

# Generating Parallelization
registerDoParallel(8)
x=model.matrix(sub.train$y2~.,sub.train)
fit.ridge.cv <- cv.glmnet(x,train_rows_new, type.measure="mae", alpha=0, 
                          family="binomial",nfolds=10,parallel=TRUE)


c = coef(fit.ridge.cv,s=fit.ridge.cv$lambda.1se)
inds <- which(c!=0)
imp_attributes_ridge <- row.names(c)[inds]
imp_attributes_ridge<- imp_attributes_lasso[-c(grep("Intercept",imp_attributes_ridge))]
imp_attributes_ridge

x.test = model.matrix(sub.test$y2~.,sub.test)
y.test = sub.test$y2

pred.ridge.csv <- predict(fit.ridge.cv,x.test,s = fit.ridge.cv$lambda.min,type="class")

confusionMatrix(data=pred.ridge.csv, sub.test$y2,positive="1")

x.test1 = model.matrix(test1$y2~.,test1[,-1])
y.test1 = test1$y2

pred.ridge.csv <- predict(fit.ridge.cv,x.test1,s = fit.ridge.cv$lambda.min,type="class")

############################################ Save the output in the CSV file ################################################

## Save the answer in the CSV file
answer_df <- data.frame("timestamp"=test$timestamp, "y1"=pred.lasso.y1,"y2"=pred.ridge.csv)
colnames(answer_df) <- c("timestamp","y1","y2")
write.csv(x = answer_df,file = "Submission6.CSV",row.names = F)
