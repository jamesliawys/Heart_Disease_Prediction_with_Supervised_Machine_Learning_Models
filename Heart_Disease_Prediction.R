rm(list = ls())
#Set Working Directory and read file
setwd("C:/Users/Lenovo/Desktop/Applied Machine")
df <- read.csv('Final_Heart_Disease_NA.csv', header = TRUE)   

# Input libraries 
library(dplyr)
library(DataExplorer)
library(VIM)
library(caret)
library(ROCR)
library(ggplot2)

# Briefly view the structure
head(df, n=20) # Review the first 20 records
dim(df) # Check the dimension of the features
glimpse(df) # 14 character, 1 double and 3 integer
names(df) # Check all the feature name
dim(df)
str(df)
head(df)
summary(df)

# EDA Here with visualisation
plot_str(df, fontSize=50)
plot_histogram(df) #Only 4 numerical features
plot_bar(df)
plot_density(df)  #Only 4 feature again
plot_boxplot(df, by='HeartDisease') #Imbalanced dataset

# Missing Data
plot_missing(df)
sum(is.na(df)) # sum of missing values in df
colSums(sapply(df,is.na)) # missing values by columns

# For continuous
# Imputing missing values in continuous variables using mean
df$BMI = ifelse(is.na(df$BMI),
                ave(df$BMI, FUN = function(x) mean(x, na.rm = TRUE)),
                df$BMI)

df$PhysicalHealth = ifelse(is.na(df$PhysicalHealth),
                           ave(df$PhysicalHealth, FUN = function(x) mean(x, na.rm = TRUE)),
                           df$PhysicalHealth)

df$MentalHealth = ifelse(is.na(df$MentalHealth),
                         ave(df$MentalHealth, FUN = function(x) mean(x, na.rm = TRUE)),
                         df$MentalHealth)

df$SleepTime = ifelse(is.na(df$SleepTime),
                      ave(df$SleepTime, FUN = function(x) mean(x, na.rm = TRUE)),
                      df$SleepTime)

#check for the changes
sum(is.na(df)) 
colSums(sapply(df,is.na)) 

# For Categorical and Binary
# Function to find the mode value 
Mode <- function(x) { 
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))] 
}


# Find the Mode Value for each categorical and binary features
heartdisease_mode <- Mode(df$HeartDisease)
Smoking_mode <- Mode(df$Smoking)
AlcoholDrinking_mode <- Mode(df$AlcoholDrinking)
Stroke_mode <- Mode(df$Stroke)
DiffWalking_mode <- Mode(df$DiffWalking)
Sex_mode <- Mode(df$Sex)
AgeCategory_mode <- Mode(df$AgeCategory)
Race_mode <- Mode(df$Race)
Diabetic_mode <- Mode(df$Diabetic)
PhysicalActivity_mode <- Mode(df$PhysicalActivity)
GenHealth_mode <- Mode(df$GenHealth)
Asthma_mode <- Mode(df$Asthma)
KidneyDisease_mode <- Mode(df$KidneyDisease)
SkinCancer_mode <- Mode(df$SkinCancer)

# Imputation with Mode
df$HeartDisease <- ifelse(is.na(df$HeartDisease), heartdisease_mode, df$HeartDisease)
df$Smoking <- ifelse(is.na(df$Smoking), Smoking_mode, df$Smoking)
df$AlcoholDrinking <- ifelse(is.na(df$AlcoholDrinking), AlcoholDrinking_mode, df$AlcoholDrinking)
df$Stroke <- ifelse(is.na(df$Stroke), Stroke_mode, df$Stroke)
df$DiffWalking <- ifelse(is.na(df$DiffWalking), DiffWalking_mode, df$DiffWalking)
df$Sex <- ifelse(is.na(df$Sex), Sex_mode, df$Sex)
df$AgeCategory <- ifelse(is.na(df$AgeCategory), AgeCategory_mode, df$AgeCategory)
df$Race <- ifelse(is.na(df$Race), Race_mode, df$Race)
df$Diabetic <- ifelse(is.na(df$Diabetic), Diabetic_mode, df$Diabetic)
df$PhysicalActivity <- ifelse(is.na(df$PhysicalActivity), PhysicalActivity_mode, df$PhysicalActivity)
df$GenHealth <- ifelse(is.na(df$GenHealth), GenHealth_mode, df$GenHealth)
df$Asthma <- ifelse(is.na(df$Asthma), Asthma_mode, df$Asthma)
df$KidneyDisease <- ifelse(is.na(df$KidneyDisease), KidneyDisease_mode, df$KidneyDisease)
df$SkinCancer <- ifelse(is.na(df$SkinCancer), SkinCancer_mode, df$SkinCancer)

#check for the changes AGAIN
sum(is.na(df)) 
colSums(sapply(df,is.na))

# Now then the data is consistent we can random sampling
# Random sampling
Class1 <- subset(df, HeartDisease == "Yes")
HeartClass1 <- Class1[sample(nrow(Class1), 2000),]
Class2 <- subset(df, HeartDisease == "No")
HeartClass2 <- Class2[sample(nrow(Class2), 2000),]

# Check the results
df <- rbind(HeartClass1, HeartClass2)
table(df$HeartDisease)
str(df)

# One hot-encoding
# Race
dmy <- dummyVars(~ Race, data = df)
Race_Onehot <- data.frame(predict(dmy, newdata = df))
str(Race_Onehot)
df <- cbind(df,Race_Onehot)
df <- df[, -which(names(df) == "Race")]
names(df)

# Diabetic
dmy <- dummyVars(~ Diabetic, data = df)
Diabetic_Onehot <- data.frame(predict(dmy, newdata = df))
View(Diabetic_Onehot)
str(Diabetic_Onehot)
df <- cbind(df,Diabetic_Onehot)
names(df)
df <- df[, -which(names(df) == "Diabetic")]
names(df)
View(df)

# Feature Encoding
df$HeartDisease <- factor(df$HeartDisease,
                          levels = c('Yes', 'No'),
                          labels = c(1,0))

df$Smoking  <- factor(df$Smoking,
                      levels = c('Yes', 'No'),
                      labels = c(1,0))

df$AlcoholDrinking <- factor(df$AlcoholDrinking,
                             levels = c('Yes', 'No'),
                             labels = c(1,0))


df$Stroke  <- factor(df$Stroke,
                     levels = c('Yes', 'No'),
                     labels = c(1,0)) 

df$DiffWalking <- factor(df$DiffWalking,
                         levels = c('Yes', 'No'),
                         labels = c(1,0))

df$Sex  <- factor(df$Sex,
                  levels = c('Male', 'Female'),
                  labels = c(1,0))

df$PhysicalActivity <- factor(df$PhysicalActivity,
                              levels = c('Yes', 'No'),
                              labels = c(1,0))

df$Asthma <- factor(df$Asthma,
                    levels = c('Yes', 'No'),
                    labels = c(1,0))

df$KidneyDisease <- factor(df$KidneyDisease,
                           levels = c('Yes', 'No'),
                           labels = c(1,0))

df$SkinCancer <- factor(df$SkinCancer,
                        levels = c('Yes', 'No'),
                        labels = c(1,0))

df$AgeCategory <- factor(df$AgeCategory,
                         levels = c('18-24', '25-29','30-34', '35-39', '40-44',
                                    '45-49', '50-54', '55-59', '60-64', '65-69',
                                    '70-74', '75-79', '80 or older'),
                         labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13))
df$AgeCategory <- as.numeric(df$AgeCategory)

df$GenHealth <- factor(df$GenHealth,
                       levels = c('Poor', 'Fair', 'Good','Very good','Excellent'),
                       labels = c(1,2,3,4,5))
df$GenHealth <- as.numeric(df$GenHealth)

#Check again
names(df)
dim(df)
str(df)
head(df)
summary(df)
View(df)
colSums(sapply(df,is.na))

#Min-max normalization for numerical continuous data
df$BMI <- (df$BMI - min(df$BMI))/(max(df$BMI) - min(df$BMI))
df$PhysicalHealth <- (df$PhysicalHealth - min(df$PhysicalHealth))/(max(df$PhysicalHealth) - min(df$PhysicalHealth))
df$MentalHealth <- (df$MentalHealth - min(df$MentalHealth))/(max(df$MentalHealth) - min(df$MentalHealth))
df$SleepTime <- (df$SleepTime - min(df$SleepTime))/(max(df$SleepTime) - min(df$SleepTime))

#Round the number
df$BMI <- round(df$BMI, digits = 3)
df$PhysicalHealth <- round(df$PhysicalHealth, digits = 3)
df$MentalHealth <- round(df$MentalHealth, digits = 3)
df$SleepTime <- round(df$SleepTime, digits = 3)
View(df)
str(df)

#Save file as backup
write.csv(df, 'Final_dataset.csv')
dfv2 <- read.csv('Final_dataset.csv', header = TRUE)
View(dfv2)
dfv2 = dfv2[,-1]
df = dfv2
str(df)

#------------------------------- FULL CLEANED--------------------------
#Run EDA again
plot_str(df, fontSize=30)
plot_histogram(df) # 6 continuous features
plot_bar(df)
plot_density(df)  #Only 4 feature again
plot_boxplot(df, by= 'HeartDisease') 
plot_correlation(df)
cor(x = df, y = df$HeartDisease)
sum(is.na(df)) 
colSums(sapply(df,is.na)) 

#____________________________ Splitting dataset _________________________________
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(1234)
dt <- df
ind <- sample(2, nrow(dt), replace=TRUE, prob=c(0.7, 0.3))
train_set <- dt[ind==1,]
test_set <- dt[ind==2,]
str(train_set)
str(test_set)
# Checking Class distribution
table(df$HeartDisease)
prop.table(table(df$HeartDisease))
prop.table(table(train_set$HeartDisease))
prop.table(table(test_set$HeartDisease))



#______________________________Modelling_________________________________
# Naive Bayes, Decision Tree, SVM
# ----------------------------Naive Bayes--------------------------------
library(naivebayes)
library(caret)

# Basic Model
train_set$HeartDisease = as.factor(train_set$HeartDisease)
NB <- naive_bayes(x=train_set[,-1],y=train_set$HeartDisease, laplace = 0)
#________________________________Test___________________________________
# Testing on Training set
# Predicting the Training set results
pred <- predict (NB,  train_set[,-1])
length(pred)
length(train_set$HeartDisease)
str(train_set$HeartDisease)
str(pred)

# ALL Test Go through here
cm = table(Predicted = pred, Actual = train_set$HeartDisease)
cm
accuracy = sum(diag(cm))/sum(cm)*100
accuracy
misclassification = (1-sum(diag(cm))/sum(cm))*100
misclassification

confusionMatrix(table(pred, train_set$HeartDisease))

# To draw ROC we need to predict the prob values. 
pred = prediction(as.numeric(train_set$HeartDisease),as.numeric(pred))
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc <- as.numeric(performance(pred, "auc")@y.values)
auc <-  round(auc, 3)
auc

# Testing on Test set
pred <- predict (NB, test_set[,-1])
length(pred)
length(test_set$HeartDisease)
str(test_set$HeartDisease)
str(pred)


cm = table(Predicted = pred, Actual = test_set$HeartDisease)
cm
accuracy = sum(diag(cm))/sum(cm)*100
accuracy
misclassification = (1-sum(diag(cm))/sum(cm))*100
misclassification

confusionMatrix(table(pred, test_set$HeartDisease))

# To draw ROC we need to predict the prob values. 
pred1 = prediction(as.numeric(test_set$HeartDisease),as.numeric(pred))
perf = performance(pred1, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc <- as.numeric(performance(pred1, "auc")@y.values)
auc <-  round(auc, 3)
auc
#______________________________Test Ends_______________________________________



# With Smoothing
train_set$HeartDisease = as.factor(train_set$HeartDisease)
NB <- naive_bayes(x=train_set[,-1],y=train_set$HeartDisease, laplace = 1)
#________________________________Test___________________________________
# Testing on Training set
# Predicting the Training set results
pred <- predict (NB,  train_set[,-1])
length(pred)
length(train_set$HeartDisease)
str(train_set$HeartDisease)
str(pred)

# ALL Test Go through here
cm = table(Predicted = pred, Actual = train_set$HeartDisease)
cm
accuracy = sum(diag(cm))/sum(cm)*100
accuracy
misclassification = (1-sum(diag(cm))/sum(cm))*100
misclassification

confusionMatrix(table(pred, train_set$HeartDisease))

# To draw ROC we need to predict the prob values. 
pred = prediction(as.numeric(train_set$HeartDisease),as.numeric(pred))
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc <- as.numeric(performance(pred, "auc")@y.values)
auc <-  round(auc, 3)
auc

# Testing on Test set
pred <- predict (NB, test_set[,-1])
length(pred)
length(test_set$HeartDisease)
str(test_set$HeartDisease)
str(pred)


cm = table(Predicted = pred, Actual = test_set$HeartDisease)
cm
accuracy = sum(diag(cm))/sum(cm)*100
accuracy
misclassification = (1-sum(diag(cm))/sum(cm))*100
misclassification

confusionMatrix(table(pred, test_set$HeartDisease))

# To draw ROC we need to predict the prob values. 
pred2 = prediction(as.numeric(test_set$HeartDisease),as.numeric(pred))
perf = performance(pred2, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc <- as.numeric(performance(pred2, "auc")@y.values)
auc <-  round(auc, 3)
auc
#______________________________Test Ends_______________________________________



#CV and HPT 
library(caret)
library(klaR)
set.seed(1234)

# Custom for cross-validation 10 folds and grid for hyp-tuning
custom <- trainControl(method = "cv", number = 10)
grid <- expand.grid(fL = seq(0.1, 1, by = 0.1), usekernel = TRUE, adjust = 0.5)
NB   <- train(HeartDisease~., 
              data = train_set,
              method = 'nb',
              trControl = custom,
              tuneGrid = grid)

#________________________________Test___________________________________
# Testing on Training set
# Predicting the Training set results
pred <- predict (NB,  train_set[,-1])
length(pred)
length(train_set$HeartDisease)
str(train_set$HeartDisease)
str(pred)

# ALL Test Go through here
cm = table(Predicted = pred, Actual = train_set$HeartDisease)
cm
accuracy = sum(diag(cm))/sum(cm)*100
accuracy
misclassification = (1-sum(diag(cm))/sum(cm))*100
misclassification

confusionMatrix(table(pred, train_set$HeartDisease))

# To draw ROC we need to predict the prob values. 
pred = prediction(as.numeric(train_set$HeartDisease),as.numeric(pred))
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc <- as.numeric(performance(pred, "auc")@y.values)
auc <-  round(auc, 3)
auc

# Testing on Test set
pred <- predict (NB, test_set[,-1])
length(pred)
length(test_set$HeartDisease)
str(test_set$HeartDisease)
str(pred)


cm = table(Predicted = pred, Actual = test_set$HeartDisease)
cm
accuracy = sum(diag(cm))/sum(cm)*100
accuracy
misclassification = (1-sum(diag(cm))/sum(cm))*100
misclassification

confusionMatrix(table(pred, test_set$HeartDisease))

# To draw ROC we need to predict the prob values. 
pred3 = prediction(as.numeric(test_set$HeartDisease),as.numeric(pred))
perf = performance(pred3, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc <- as.numeric(performance(pred3, "auc")@y.values)
auc <-  round(auc, 3)
auc
#______________________________Test Ends_______________________________________



#_____________________________________________________________________________
#----------------------------Decision Tree-----------------------------
library(rpart)
library(rpart.plot)
library(party)
library(caret)

# Basic 
tree = rpart(HeartDisease~ ., data=train_set)

##________________________________Test___________________________________
# Testing on Training set
# Predicting the Training set results
# Confusion Matrix
pred = predict (tree, train_set[,-1],type = "class")
length(pred)
length(train_set$HeartDisease)
train_set$HeartDisease
str(train_set$HeartDisease)
str(pred)

cm = table(Predicted = pred, Actual = train_set$HeartDisease)
cm
accuracy = sum(diag(cm))/sum(cm)*100
accuracy

misclassification = (1-sum(diag(cm))/sum(cm))*100
misclassification

confusionMatrix(table(pred, train_set$HeartDisease))

# To draw ROC we need to predict the prob values. 
pred = prediction(as.numeric(train_set$HeartDisease),as.numeric(pred))
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc <- as.numeric(performance(pred, "auc")@y.values)
auc <-  round(auc, 3)
auc

# Testing on Testing Set
# Confusion Matrix
pred = predict (tree, test_set[,-1],type = "class")
length(pred)
length(test_set$HeartDisease)
test_set$HeartDisease
str(test_set$HeartDisease)
str(pred)

cm = table(Predicted = pred, Actual = test_set$HeartDisease)
cm
accuracy = sum(diag(cm))/sum(cm)*100
accuracy

misclassification = (1-sum(diag(cm))/sum(cm))*100
misclassification

confusionMatrix(table(pred, test_set$HeartDisease))

# To draw ROC we need to predict the prob values. 
pred = prediction(as.numeric(test_set$HeartDisease),as.numeric(pred))
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc <- as.numeric(performance(pred, "auc")@y.values)
auc <-  round(auc, 3)
auc

#______________________________Test Ends_______________________________________




# Here we use tree with parameter settings.
# This code generates the tree with training data
tree = rpart(HeartDisease ~ ., data=train_set, method="class", minsplit = 2, minbucket = 10, cp = 0)
rpart.plot(tree, extra = 101, nn = TRUE)
prp (tree)
print(tree)
summary(tree)
plot(tree)
text(tree)
plotcp(tree) # CP = 0.0035

# Basic but with Regularization
tree <- rpart(HeartDisease ~ ., data = train_set, cp = 0.0035) #after parameter setting

##________________________________Test___________________________________
# Testing on Training set
# Predicting the Training set results
# Confusion Matrix
pred = predict (tree, train_set[,-1],type = "class")
length(pred)
length(train_set$HeartDisease)
train_set$HeartDisease
str(train_set$HeartDisease)
str(pred)

cm = table(Predicted = pred, Actual = train_set$HeartDisease)
cm
accuracy = sum(diag(cm))/sum(cm)*100
accuracy

misclassification = (1-sum(diag(cm))/sum(cm))*100
misclassification

confusionMatrix(table(pred, train_set$HeartDisease))

# To draw ROC we need to predict the prob values. 
pred = prediction(as.numeric(train_set$HeartDisease),as.numeric(pred))
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc <- as.numeric(performance(pred, "auc")@y.values)
auc <-  round(auc, 3)
auc

# Testing on Testing Set
# Confusion Matrix
pred = predict (tree, test_set[,-1],type = "class")
length(pred)
length(test_set$HeartDisease)
test_set$HeartDisease
str(test_set$HeartDisease)
str(pred)

cm = table(Predicted = pred, Actual = test_set$HeartDisease)
cm
accuracy = sum(diag(cm))/sum(cm)*100
accuracy

misclassification = (1-sum(diag(cm))/sum(cm))*100
misclassification

confusionMatrix(table(pred, test_set$HeartDisease))

# To draw ROC we need to predict the prob values. 
pred = prediction(as.numeric(test_set$HeartDisease),as.numeric(pred))
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc <- as.numeric(performance(pred, "auc")@y.values)
auc <-  round(auc, 3)
auc

#______________________________Test Ends_______________________________________




# Tree with CV and HPT
# Specify the training control for cross-validation
custom <- trainControl(method = "cv", number = 10)
grid <- expand.grid(.cp = seq(0.01, 0.5, by = 0.01))
tree <- train(HeartDisease ~ .,
              data = train_set,
              method = "rpart",
              trControl = custom,
              tuneGrid = grid)

##________________________________Test___________________________________
# Testing on Training set
# Predicting the Training set results
# Confusion Matrix
pred = predict (tree, train_set[,-1],type = "raw") #For CV and HPT
length(pred)
length(train_set$HeartDisease)
train_set$HeartDisease
str(train_set$HeartDisease)
str(pred)

cm = table(Predicted = pred, Actual = train_set$HeartDisease)
cm
accuracy = sum(diag(cm))/sum(cm)*100
accuracy

misclassification = (1-sum(diag(cm))/sum(cm))*100
misclassification

confusionMatrix(table(pred, train_set$HeartDisease))

# To draw ROC we need to predict the prob values. 
pred = prediction(as.numeric(train_set$HeartDisease),as.numeric(pred))
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc <- as.numeric(performance(pred, "auc")@y.values)
auc <-  round(auc, 3)
auc

# Testing on Testing Set
# Confusion Matrix
pred = predict (tree, test_set[,-1],type = "raw") #For CV and HPT
length(pred)
length(test_set$HeartDisease)
test_set$HeartDisease
str(test_set$HeartDisease)
str(pred)

cm = table(Predicted = pred, Actual = test_set$HeartDisease)
cm
accuracy = sum(diag(cm))/sum(cm)*100
accuracy

misclassification = (1-sum(diag(cm))/sum(cm))*100
misclassification

confusionMatrix(table(pred, test_set$HeartDisease))

# To draw ROC we need to predict the prob values. 
pred = prediction(as.numeric(test_set$HeartDisease),as.numeric(pred))
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc <- as.numeric(performance(pred, "auc")@y.values)
auc <-  round(auc, 3)
auc

#______________________________Test Ends_______________________________________




#----------------------------SVM-----------------------------
# SVM in R
library(ggplot2)
library(e1071)
library(caret)

# Basic radial basis function
svm1 <- svm(HeartDisease~., data = train_set)

##________________________________Test___________________________________

# Testing on Training set
# Confusion Matrix
pred = predict (svm1, train_set[,-1])
pred
length(pred)
length(train_set$HeartDisease)
train_set$HeartDisease
str(train_set$HeartDisease)
str(pred)
#pred = pred[,-1]

cm = table(Predicted = pred, Actual = train_set$HeartDisease)
cm
accuracy = sum(diag(cm))/sum(cm)*100
accuracy

misclassification = (1-sum(diag(cm))/sum(cm))*100
misclassification


confusionMatrix(table(pred, train_set$HeartDisease))

# To draw ROC we need to predict the prob values. 
pred = prediction(as.numeric(train_set$HeartDisease),as.numeric(pred))
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc <- as.numeric(performance(pred, "auc")@y.values)
auc <-  round(auc, 3)
auc

# Testing Testing Set
# Confusion Matrix
pred = predict (svm1, test_set[,-1])
pred
length(pred)
length(test_set$HeartDisease)
test_set$HeartDisease
str(test_set$HeartDisease)
str(pred)
#pred = pred[,-1]

cm = table(Predicted = pred, Actual = test_set$HeartDisease)
cm
accuracy = sum(diag(cm))/sum(cm)*100
accuracy

misclassification = (1-sum(diag(cm))/sum(cm))*100
misclassification


confusionMatrix(table(pred, test_set$HeartDisease))

# To draw ROC we need to predict the prob values. 
pred = prediction(as.numeric(test_set$HeartDisease),as.numeric(pred))
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc <- as.numeric(performance(pred, "auc")@y.values)
auc <-  round(auc, 3)
auc

#______________________________Test Ends_______________________________________





# For Basic Linear
svm1 = svm (HeartDisease~., data = train_set, kernel = "linear")
summary (svm1)
svm1$gamma

##________________________________Test___________________________________

# Testing on Training set
# Confusion Matrix
pred = predict (svm1, train_set[,-1])
pred
length(pred)
length(train_set$HeartDisease)
train_set$HeartDisease
str(train_set$HeartDisease)
str(pred)
#pred = pred[,-1]

cm = table(Predicted = pred, Actual = train_set$HeartDisease)
cm
accuracy = sum(diag(cm))/sum(cm)*100
accuracy

misclassification = (1-sum(diag(cm))/sum(cm))*100
misclassification


confusionMatrix(table(pred, train_set$HeartDisease))

# To draw ROC we need to predict the prob values. 
pred = prediction(as.numeric(train_set$HeartDisease),as.numeric(pred))
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc <- as.numeric(performance(pred, "auc")@y.values)
auc <-  round(auc, 3)
auc

# Testing Testing Set
# Confusion Matrix
pred = predict (svm1, test_set[,-1])
pred
length(pred)
length(test_set$HeartDisease)
test_set$HeartDisease
str(test_set$HeartDisease)
str(pred)
#pred = pred[,-1]

cm = table(Predicted = pred, Actual = test_set$HeartDisease)
cm
accuracy = sum(diag(cm))/sum(cm)*100
accuracy

misclassification = (1-sum(diag(cm))/sum(cm))*100
misclassification


confusionMatrix(table(pred, test_set$HeartDisease))

# To draw ROC we need to predict the prob values. 
pred = prediction(as.numeric(test_set$HeartDisease),as.numeric(pred))
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc <- as.numeric(performance(pred, "auc")@y.values)
auc <-  round(auc, 3)
auc

#______________________________Test Ends_______________________________________




# CV and HPT and smoothing for RBF
set.seed(1234)
custom <- trainControl(method = "cv", number = 10)
grid = expand.grid(C = c(0.1, 1, 10), sigma = c(0.1, 1, 10))
svm1 <- train(HeartDisease~ .,
              train_set,
              method = 'svmRadial',
              trControl = custom,
              tuneGrid = grid)

##________________________________Test___________________________________

# Testing on Training set
# Confusion Matrix
pred = predict (svm1, train_set[,-1])
pred
length(pred)
length(train_set$HeartDisease)
train_set$HeartDisease
str(train_set$HeartDisease)
str(pred)
#pred = pred[,-1]

cm = table(Predicted = pred, Actual = train_set$HeartDisease)
cm
accuracy = sum(diag(cm))/sum(cm)*100
accuracy

misclassification = (1-sum(diag(cm))/sum(cm))*100
misclassification


confusionMatrix(table(pred, train_set$HeartDisease))

# To draw ROC we need to predict the prob values. 
pred = prediction(as.numeric(train_set$HeartDisease),as.numeric(pred))
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc <- as.numeric(performance(pred, "auc")@y.values)
auc <-  round(auc, 3)
auc

# Testing Testing Set
# Confusion Matrix
pred = predict (svm1, test_set[,-1])
pred
length(pred)
length(test_set$HeartDisease)
test_set$HeartDisease
str(test_set$HeartDisease)
str(pred)
#pred = pred[,-1]

cm = table(Predicted = pred, Actual = test_set$HeartDisease)
cm
accuracy = sum(diag(cm))/sum(cm)*100
accuracy

misclassification = (1-sum(diag(cm))/sum(cm))*100
misclassification


confusionMatrix(table(pred, test_set$HeartDisease))

# To draw ROC we need to predict the prob values. 
pred = prediction(as.numeric(test_set$HeartDisease),as.numeric(pred))
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc <- as.numeric(performance(pred, "auc")@y.values)
auc <-  round(auc, 3)
auc

#______________________________Test Ends_______________________________________



