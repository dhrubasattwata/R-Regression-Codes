# ---
# title: "Linear Regression"
# author: "Dhrubasattwata Roy Choudhury"
# ---

### Dataset

cars <- cars
summary(cars)


### Graphical Analysis

scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")  # scatterplot

par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out), col="midnightblue")  # box plot for 'speed'
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out), col="midnightblue")  # box plot for 'distance'

Density plot – Check if the response variable is close to normality

library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(cars$speed), col="midnightblue")
plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'
polygon(density(cars$dist), col="midnightblue")

### Correlation

cor(cars$speed, cars$dist)  # calculate correlation between speed and distance 

### Build Linear Model

linear.model <- lm(dist ~ speed, data=cars)  # build linear regression model on full data
output <- summary(linear.model)
output

### Prediction using Linear Models

set.seed(123)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data

Step 2: Develop the model on the training data

linear.mod <- lm(dist ~ speed, data=trainingData)  # build the model
summary <- summary(linear.mod)
summary

r2 <- summary$adj.r.squared # higher the better
aic <- AIC(linear.mod)  # lower the better
bic <- BIC(linear.mod)  # lower the better

df <- rbind(r2,aic,bic)
rownames(df) <- c("Adj. R2", "AIC", "BIC")
colnames(df) <- "value"
df


Step 3:  Use it to predict the distance on test data

distPred <- predict(linear.mod, testData)  # predict distance


Step 4: Calculate prediction accuracy and error rates

actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=round(distPred,3)))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
actuals_preds
print(paste0("The correlation is ", round(correlation_accuracy[1,2],3)*100, "%"))

### Performance Metrics

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals) 
print(paste0("Min_Max_Accuracy is ", round(min_max_accuracy,3)))
print(paste0("MAPE is ", round(mape,3)))


### Cross Validation: k- Fold Cross validation

library(DAAG)
cvResults <- suppressWarnings(CVlm(data=cars, form.lm=dist ~ speed, m=10, dots=FALSE, seed=100, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."))  # performs the CV
# m = 10 to divide into 10 sets, so test set is of 10% data
attr(cvResults, 'ms') 




















































