if(!require(psych)) install.packages("psych")
if(!require(randomForest)) install.packages("randomForest")
if(!require(caret)) install.packages("caret")
if(!require(pRoc)) install.packages("pRoc")
if(!require(corrplot)) install.packages("corrplot")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(devtools)) install.packages("devtools")
if(!require(sqldf)) install.packages("sqldf")
require(randomForest)
library(psych)
library(caret)
library('pROC')
library(tidyverse)
library(caret)
library(randomForest)
library(ggplot2)
library(dplyr)
library(corrplot)
library(rpart)
library(sqldf)
library(devtools)
devtools::install_github("collectivemedia/tictoc")
library(tictoc)


setwd("/Users/schanna/Documents/Me/edX/ILPR-Project/ILPR-Project")

liver_df <- read.csv("data/indian_liver_patient.csv")
# Lets glance thru some data
class(liver_df)
glimpse(liver_df)

# Summary of our dataset
summary(liver_df)

# Rename column Dataset to Diseased
colnames(liver_df)[colnames(liver_df)=='Dataset'] = 'Diseased'
colnames(liver_df)

# Showing Gender ratio in the data set
table(liver_df$Gender)

# Change values of Diseased to "0" and "1" from "1" and "2"

table(liver_df$Diseased)
liver_df$Diseased <- as.numeric(ifelse(liver_df$Diseased == 2, 1, 0))
table(liver_df$Diseased)

# Convert Diseased to a factor variable

class(liver_df$Diseased)
liver_df$Diseased <- as.factor(liver_df$Diseased)
class(liver_df$Diseased)

# Remove NA values in each column

colSums(sapply(liver_df, is.na))
dim(liver_df)
liver_df = na.omit(liver_df)

# Compare the results from the above dim(liver_df)

dim(liver_df)

# Visualize the dataset

ggplot(liver_df, aes(factor(Gender))) + 
geom_bar(width = 0.4, aes(fill=factor(Gender))) + 
geom_text(stat = "count", check_overlap = TRUE, aes( label = ..count..), position = position_stack(vjust = 0.5)) +
labs(x = "Gender", y = "$ of Records", title = "Gender Distribution") +
theme(plot.title = element_text(hjust = 0.5)) 

# Diseased distribution among Gender
print("using sqldf")
sqldf("select count(1), Gender, Diseased from liver_df group by Gender, Diseased order by 2")
#  count(1) Gender Diseased
#1       50      F        0
#2       92      F        1
#3      117      M        0
#4      324      M        1

table(liver_df$Diseased, liver_df$Gender)
     
#       F   M
#  0   50 117
#  1   92 324

ggplot(liver_df, aes(factor(Gender), fill=factor(Diseased))) + 
geom_bar(width = 0.4) + 
geom_text(aes(label = ..count..), stat="count", 
position = position_stack(0.5)) + 
labs(x = "Gender", y = "$ of Records/Disased", title = "Diseased distribution among Gender") +
theme(plot.title = element_text(hjust = 0.5)) 

# Plot by Age and Gender - Ok not that pretty
# problem with y axis not showing number of diseases
ggplot(data = liver_df, aes(Age, Diseased)) + 
geom_bar(stat = "identity", aes(fill = Gender)) +
ggtitle("Diseased distribution by Age and Gender") +
theme(plot.title = element_text(hjust = 0.5)) 

# Histogram of Age Distribution
describe(liver_df$Age)

hist(liver_df$Age, main = "Age Distribution", col = "orange", xlab = "Age", ylab = "Fequency") 

# Plot other data elements together.
# Do it all for other key attributes showing the mean,sd and scew data using describe(liver_df$attribute)

describe(liver_df$Total_Bilirubin)
describe(liver_df$Direct_Bilirubin)
describe(liver_df$Alkaline_Phosphotase)
describe(liver_df$Alamine_Aminotransferase)
describe(liver_df$Aspartate_Aminotransferase)
describe(liver_df$Total_Protiens)
describe(liver_df$Albumin)
describe(liver_df$Albumin_and_Globulin_Ratio)

#par(mfrow=c(4,2))

hist(log(liver_df$Total_Bilirubin), main = "Total Bilirubin Distribution", col = "orange", xlab = "Total Bilirubin", ylab = "Fequency") 
 
hist(log(liver_df$Direct_Bilirubin), main = "Direct Bilirubin Distribution", col = "orange", xlab = "Direct Bilirubin", ylab = "Fequency") 

hist(log(liver_df$Alkaline_Phosphotase), main = "Alkaline Phosphotase Distribution", col = "orange", xlab = "Alkaline Phosphotase", ylab = "Fequency") 

hist(log(liver_df$Alamine_Aminotransferase), main = "Alamine Aminotransferase Distribution", col = "orange", xlab = "Alamine Aminotransferase", ylab = "Fequency") 

hist(log(liver_df$Aspartate_Aminotransferase), main = "Aspartate Aminotransferase Distribution", col = "orange", xlab = "Aspartate Aminotransferase", ylab = "Fequency") 

hist(log(liver_df$Total_Protiens), main = "Total Protiens Distribution", col = "orange", xlab = "Total Protiens", ylab = "Fequency") 

hist(log(liver_df$Albumin), main = "Albumin Distribution", col = "orange", xlab = "Age", ylab = "Fequency") 

hist(log(liver_df$Albumin_and_Globulin_Ratio), main = "Albumin and Globulin Ratio Distribution", col = "orange", xlab = "Albumin and Globulin Ratio", ylab = "Fequency") 

#par(mfrow=c(1,1))

# Creating Correlation Matrix

non_pred_cols <- c('Gender', 'Diseased')
correlationMatrix <- cor(liver_df[, !(names(liver_df) %in% non_pred_cols)])
corrplot(correlationMatrix)

# Feature Selection
# Automatic feature selection methods can be used to build many models with different subsets of a dataset and identify those attributes that are and are not required to build an accurate model.

# A popular automatic method for feature selection provided by the caret R package is called Recursive Feature Elimination or RFE.

# We use a Recursive Feature Elimination or RFE to remove the unnecessary features

set.seed(7)
# define the control using a random forest selection function
rfeControl <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(liver_df[,1:10], liver_df[,11],sizes = c(1:10), rfeControl=rfeControl)
print(results)
# list the chosen features
predictors <- predictors(results)
print(predictors)
# plot the results
plot(results, type=c("g", "o"))

# Resize the original dataset by removing others which are not listed from predictors

subset_liver_df <- liver_df[,c(predictors,"Diseased")]
colnames(subset_liver_df)

## Modeling

# Random Forest default
# We create a sample data using a random sample, the seed has to be set to generate same indices everytime. That will generate our train and test samples using a 70/30 split for train/test.

smp_size <- floor(0.7 * nrow(subset_liver_df))
set.seed(9)
train_ind <- sample(seq_len(nrow(subset_liver_df)), size = smp_size)
train <- subset_liver_df[train_ind, ]

#index <- createDataPartition(subset_liver_df$Diseased, p = 0.7, list = FALSE)
#extract training using  indexes
#train <- subset_liver_df[index,]
#extract test using indexes
#test <- subset_liver_df[-index,]
#with the above index and seed accuracy is completley different!!!

dim(train)
table(train$Diseased)
test = subset_liver_df[-train_ind, ]
dim(test)
table(test$Diseased)

# My addition

### Logistic Regression Model with more than one predictor (from course material)

#glm
fit_glm <- suppressWarnings(glm(Diseased ~ ., data = train, family = "binomial"))
p_hat_glm <- predict(fit_glm, test)
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0))
glm_acc <- 
suppressWarnings(confusionMatrix(data = y_hat_glm, reference = test$Diseased)$overall["Accuracy"])
print(glm_acc)

accuracy_results <- 
  suppressWarnings(data_frame(Model = "Model#1: GLM(Generalized Liner Model) Predictions", 
  Accuracy = glm_acc))

# for r_part
xfit <- rpart(Diseased ~., data = subset_liver_df)
plot(xfit, margin = 0.1)
text(xfit, cex = 0.75)
train_rpart <- train(Diseased ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)), data = train)
plot(train_rpart)
rpart_acc <- confusionMatrix(predict(train_rpart, test), test$Diseased)$overall["Accuracy"]
print(rpart_acc)
# Accuracy 
#0.7011494 

# Persist prediction results
accuracy_results <- bind_rows(accuracy_results,
  data_frame(Model = "Model#2: RPART Predictions", 
  Accuracy = rpart_acc))


# Random Forest can be tune up using Random Search or Grid Search. We use the second one to try various combinations of parameters using Cross-Validation.

# First we create a K-fold cross-validation of 10 folds:

trControl <- trainControl(method = "cv",
    number = 10,
    search = "grid")

# Then we run the model for the default (or not optimized) Random Forest version:

set.seed(1234)
rf_default <- train(Diseased~.,
    data = train,
    method = "rf",
    metric = "Accuracy",
    trControl = trControl)
print(rf_default)

# The best Accuracy obtained in the search space was 0.7157927. The final value used for the model was mtry = 2.


## Hyperparameter Optimization
## Best mtry
set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(3: 10))
rf_mtry <- suppressWarnings(train(Diseased~.,
    data = train,
    method = "rf",
    metric = "Accuracy",
    tuneGrid = tuneGrid,
    trControl = trControl,
    importance = TRUE,
    nodesize = 14,
    ntree = 300))
print(rf_mtry)

#Accuracy was used to select the optimal model using the largest value.
#The final value used for the model was mtry = 3.

#We save the best value for the mtry parameter for tunning the other paramaters.

best_mtry <- rf_mtry$bestTune$mtry 
max(rf_mtry$results$Accuracy)
#[1] 0.7279878
best_mtry
#> best_mtry
#[1] 3


# Best maxnodes
#This parameter set the maximum of the terminal nodes in the forest For this parameter will need to create a list of values and then summarize the results:

#List to store the values
store_maxnode <- list()
#test only with the best mtry parameter already obtained
tuneGrid <- expand.grid(.mtry = best_mtry)
#iterate over different values
for (maxnodes in c(20: 30)) {
    set.seed(1234)
    rf_maxnode <- train(Diseased~.,
        data = train,
        method = "rf",
        metric = "Accuracy",
        tuneGrid = tuneGrid,
        trControl = trControl,
        importance = TRUE,
        nodesize = 14,
        maxnodes = maxnodes,
        ntree = 300)
    current_iteration <- toString(maxnodes)
    store_maxnode[[current_iteration]] <- rf_maxnode
}
results_maxnode <-  resamples(store_maxnode)
summary(results_maxnode)

# The option maxnodes=25 is the best option.



## Best ntrees
#This parameter is the number of trees of the ensamble. Same as maxnodes we need to iterate between various ntress values and summarize in the end. We use the already obtained values for mtry and maxnodes.

store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
    set.seed(1234)
    rf_maxtrees <- train(Diseased~.,
        data = train,
        method = "rf",
        metric = "Accuracy",
        tuneGrid = tuneGrid,
        trControl = trControl,
        importance = TRUE,
        nodesize = 14,
        maxnodes = 25,
        ntree = ntree)
    key <- toString(ntree)
    store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

#The best value gets with 1000 trees.



## Evaluate
# We evaluate each model.

maxtree_prediction <-predict(rf_maxtrees,test)
maxnode_prediction <- predict(rf_maxnode,test)
mtry_prediction <- predict(rf_mtry,test)
default_prediction <- predict(rf_default,test)

# We get the confusion matrix for each model:

# default
default_acc <- confusionMatrix(default_prediction, test$Diseased)$overall["Accuracy"]
print(default_acc)

# Persist prediction results
accuracy_results <- bind_rows(accuracy_results,
  data_frame(Model = "Model#3: Default Predictions", 
  Accuracy = default_acc))

#accuracy_results %>% knitr::kable()


# mtry+default
mtry_acc <- confusionMatrix(mtry_prediction, test$Diseased)$overall["Accuracy"]
print(mtry_acc)

# Append to accuracy_results table
accuracy_results <- bind_rows(accuracy_results, 
  data_frame(Model = "Model#4: Default + MTRY Predictions", 
             Accuracy = mtry_acc))


# maxnodes+mtry+default
maxnode_acc <- confusionMatrix(maxnode_prediction, test$Diseased)$overall["Accuracy"]
print(maxnode_acc)

# Append to accuracy_results table
accuracy_results <- bind_rows(accuracy_results, 
  data_frame(Model = "Model#5: Default + MTRY + MaxNodes Predictions", 
             Accuracy = maxnode_acc))

# maxnodes+mtry+ntrees+default
maxtree_acc <- confusionMatrix(maxtree_prediction, test$Diseased)$overall["Accuracy"]
print(maxtree_acc)

# Append to accuracy_results table
accuracy_results <- bind_rows(accuracy_results, 
  data_frame(Model = "Model#6: Default + MTRY + MaxNodes + N-Trees Predictions",
             Accuracy = maxtree_acc
            ))


## ROC Curves

# with glm
glm_pred_prob <- as.data.frame(predict(fit_glm, test, type = "response"))
plot(roc(test$Diseased, glm_pred_prob$`predict(fit_glm, test, type = "response")`, legacy.axes = TRUE), main = "GLM ROC Curve")
print(roc(test$Diseased, glm_pred_prob$`predict(fit_glm, test, type = "response")`))

# with rpart
rpart_pred_prob <- as.data.frame(predict(train_rpart, test, type = "prob"))
plot(roc(test$Diseased, rpart_pred_prob$`0`, legacy.axes = TRUE), main = "RPART ROC Curve")
print(roc(test$Diseased, rpart_pred_prob$`0`))

# with default
default_pred_prob <- as.data.frame(predict(rf_default, test, type = "prob"))
plot(roc(test$Diseased, default_pred_prob$`0`), legacy.axes = TRUE, main = "Default ROC Curve")
print(roc(test$Diseased, default_pred_prob$`0`))

# with mtry
mtry_pred_prob <- as.data.frame(predict(rf_mtry, test, type = "prob"))
plot(roc(test$Diseased, mtry_pred_prob$`0`), legacy.axes = TRUE, main = "MTRY ROC Curve")
print(roc(test$Diseased, mtry_pred_prob$`0`))

# with maxnodes
maxnode_pred_prob <- as.data.frame(predict(rf_maxnode, test, type = "prob"))
plot(roc(test$Diseased, maxnode_pred_prob$`0`), legacy.axes = TRUE, main = "MaxNodes ROC Curve")
print(roc(test$Diseased, maxnode_pred_prob$`0`))

#with maxtrees
maxtree_pred_prob <- as.data.frame(predict(rf_maxtrees, test, type = "prob"))
plot(roc(test$Diseased, maxtree_pred_prob$`0`), legacy.axes = TRUE, main = "MaxTrees ROC Curve")
print(roc(test$Diseased, maxtree_pred_prob$`0`))

#ROC Curve for all the models**
roc_glm = suppressWarnings(roc(test$Diseased, glm_pred_prob$`predict(fit_glm, test, type = "response")`))
roc_rpart = roc(test$Diseased, rpart_pred_prob$`0`)
roc_rf_default = roc(test$Diseased, default_pred_prob$`0`)
roc_mtry = roc(test$Diseased, mtry_pred_prob$`0`)
roc_maxnode = roc(test$Diseased, maxnode_pred_prob$`0`)
roc_maxtree = roc(test$Diseased, maxtree_pred_prob$`0`)

par(pty="s")
plot(roc_glm, main = "ROC Curve for all the Models")
lines(roc_rpart, 'orange')
lines(roc_rf_default, 'magenta')
lines(roc_mtry, col = 'blue')
lines(roc_maxnode, col = 'red')
lines(roc_maxtree, col = 'brown')
legend(0.35, 0.5, legend=c("GLM", "RPART", "RF-Default", "MTRY", "MaxNodes", "MaxTrees"), 
       col=c("black", "orange", "magenta", "blue", "red", "brown"), lty=1:2, cex=0.8)

# Conclusion 

# Print Accuracies obtained from all the Models

accuracy_results %>% knitr::kable()




