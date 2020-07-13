# ---
# title: "Logistic Regression"
# author: "Dhrubasattwata Roy Choudhury"
# ---

## Logistic Regression 

### Dataset
Lets try and predict if an individual will earn more than $50K using logistic regression based on demographic variables available in the adult data. Download it from here: https://datahub.io/machine-learning/adult#resource-adult_zip

inputData <- read.csv("C:/Users/Dhruba/Documents/R/data/adult.csv")
head(inputData) #first 6 rows

table(inputData$ABOVE50K)

### Create Training and Test Samples

# Create Training Data
input_ones <- inputData[which(inputData$ABOVE50K == 1), ]  # all 1's
input_zeros <- inputData[which(inputData$ABOVE50K == 0), ]  # all 0's
set.seed(100)  # for repeatability of samples
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))  # 0's for training. Pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 

# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 


library(smbinning)
# segregate continuous and factor variables
factor_vars <- c ("WORKCLASS", "EDUCATION", "OCCUPATION", "RELATIONSHIP", "RACE", "SEX")
continuous_vars <- c("AGE", "FNLWGT","EDUCATION.NUM", "HOURSPERWEEK", "CAPITALGAIN", "CAPITALLOSS")

iv_df <- data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(12))  # init for IV results

# compute IV for categoricals
for(factor_var in factor_vars){
  smb <- smbinning.factor(trainingData, y="ABOVE50K", x=factor_var)  # WOE table
  if(class(smb) != "character"){ # heck if some error occured
    iv_df[iv_df$VARS == factor_var, "IV"] <- smb$iv
  }
}

# compute IV for continuous vars
for(continuous_var in continuous_vars){
  smb <- smbinning(trainingData, y="ABOVE50K", x=continuous_var)  # WOE table
  if(class(smb) != "character"){  # any error while calculating scores.
    iv_df[iv_df$VARS == continuous_var, "IV"] <- smb$iv
  }
}

iv_df <- iv_df[order(-iv_df$IV), ]  # sort
iv_df



### BUILD the Logistic Model and Predict


logit.model <- glm(ABOVE50K ~ relationship + age + capitalgain + occupation + education.num, data=trainingData, family=binomial(link="logit"))
predicted <- predict(logit.model, testData, type="response") 

library(InformationValue)
optCutOff <- optimalCutoff(testData$ABOVE50K, predicted)[1] 
print (paste0("The optimized cutoff is ", optCutOff))


### Model Statistics

summary(logit.model)

mis.error <- misClassError(testData$ABOVE50K, predicted, threshold = optCutOff)
print (paste0("The Misclassification error is ", mis.error))


### Performance Metrics

## ROC
plotROC(testData$ABOVE50K, predicted)

## Concordance
Concordance(testData$ABOVE50K, predicted)

## Specificity and Sensitivity
sensitivity(testData$ABOVE50K, predicted, threshold = optCutOff)
specificity(testData$ABOVE50K, predicted, threshold = optCutOff)


## Confusion Matrix
confusionMatrix(testData$ABOVE50K, predicted, threshold = optCutOff)





