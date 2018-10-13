# FOLLOWING SCRIPT CONTAIN SOULTION TO
#
# COURSE :- Pridictive Analytics II
# MODULE :- Assignment Support vector machine (SVM)
#
#Submitted by :- Ayushi Chouksey (Roll no. :- DDA1810142) 
#
#====================================Business Understanding============================
#handwritten digit recognition
#We have an image of a digit submitted by a user via a scanner, a tablet, or other digital devices. 
#The goal is to develop a model that can correctly identify the digit (between 0-9) written in an image. 
#We have to develop a model using Support Vector Machine which should correctly classify the handwritten digits based 
#on the pixel values given as features.
#Kernels enable the linear SVM model to separate nonlinearly separable data points.
#
#
#====================================== ASSUMPTIONS =======================================
#
# 1. I assume that working directory is set throught Session-> Set Working Directory to the folder where all necessary files are kept
# 2. The data is not in zip file but exploded in the working directory.
#
setwd("D:/course 6/M3- SVM Assignment")
getwd()
#
#
# #========================= CHECK  AND INSTALL PACAKGES =======================
#
#Check if pacakges are installed or not and if not get them installed
#This is done so that If a package is not installed in evaluators environment
#script should not fail cost us marks.
#
#
#Required pacakge list
pkg <-
  c(
    "caret",
    "kernlab",
    "dplyr",
    "readr",
    "ggplot2",
    "gridExtra"
  )

#user defined function for checking and if not installed installing all the required pacakges.
check_and_install <- function(pkg) {
  if (!is.element(pkg, installed.packages()[, 1]))
    install.packages(pkg, dependencies  = TRUE)
}

# installing packages
status <- lapply(pkg, check_and_install)

# loading libraries
status <- lapply(pkg, library, character.only = TRUE)
#
#
#
########################## Loading Data to a variable #########################
#
mnist_train <- read.csv("mnist_train.csv")
#
#Check number of rows and columns
nrow(mnist_train)
ncol(mnist_train)

#========================= USER DEFINED FUNCTIONS ============================
# HELPER FUNCTION
# check for blanks in dataframe columns
check_cols_for_Blanks <- function(df) {
  sapply(df, function(x)
    length(which(x == "")))
}

# check for NAs in dataframes
check_for_nas <- function(df) {
  sapply(df, function(x)
    sum(is.na(x)))
}

#=================================== Data Understanding =========================
#Understanding Dimensions
dim(mnist_train)
# The mnist_train dataset have approx 60000 rows and 785 columns
# All the columns has pixel values given as features.

#Structure of the dataset
str(mnist_train)
# All the variables are of integer data type

#printing first few rows
head(mnist_train)

#Exploring the data
summary(mnist_train)

# Check for blank values
sum(check_cols_for_Blanks(mnist_train))
##### No blank values in dataset

# Check for NAs 
sum(check_for_nas(mnist_train))
#### No NAs in dataset


###################################### Data Set Prepration ###############################################

# Create a Label variable
names(mnist_train)[1] <- "label"

#Convert label variable into factor
mnist_train$label <- factor(mnist_train$label)

# Splitting the data between train and test

set.seed(100)
train.indices = sample(1:nrow(mnist_train), 0.1*nrow(mnist_train))
# Because the data set is very large so we are taking only 10% sample data for training.

train = mnist_train[train.indices, ]
test = mnist_train[-train.indices, ]


# Scaling data 

max(train[ ,2:ncol(train)]) 
# max pixel value is 255, lets use this to scale data
train[ , 2:ncol(train)] <- train[ , 2:ncol(train)]/255

test <- cbind(label = test[ ,1], test[ , 2:ncol(test)]/255)


####################### Graphs ########################
 
# Frequency distribution of original mnist_train dataset
plot1 <- ggplot(mnist_train, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar() + 
  labs(y = "frequency", title = "mnist_train dataset") + 
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.20)) +
  geom_text(stat = "count", aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))

plot1

# Frequency distribution of training dataset
plot2 <- ggplot(train, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar() +
  labs(y = "Relative frequency", title = "training dataset") + 
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.20)) +
  geom_text(stat = "count", aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))
plot2

# Frequency distribution of Test dataset
plot3 <- ggplot(test, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar() +
  labs(y = "Relative frequency", title = "mnist_train dataset") + 
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.20)) +
  geom_text(stat = "count", aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))
plot3

# Same frequency distribution found in all three datasets (mnist_train, train, test)

############################################# Constructing a model #############################

#====================================Using Linear Kernel========================================

# The linear kernel: This is the same as the support vector classifier, 
# or the hyperplane, without any transformation at all

Model_linear <- ksvm(label~ ., data = train, scaled = FALSE, kernel = "vanilladot", C = 1)
Eval_linear<- predict(Model_linear, test)

summary(Model_linear)
summary(Eval_linear)


#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$label)

####### Observations
# Accuracy :- 90.9%
# Sensitivities :- 83.4% - 97.7% 
# Specificities :- >98.4% (Quite high)

#
# 
#Using Linear Kernel using high value of C
Model_linear_2 <- ksvm(label~ ., data = train, scaled = FALSE, kernel = "vanilladot", C = 10)
Eval_linear_2<- predict(Model_linear_2, test)

print(Model_linear_2)
summary(Eval_linear_2)


#confusion matrix - Linear Kernel using high value of C
confusionMatrix(Eval_linear_2,test$label)

## Observations
# Accuracy :- 90.8%
# Sensitivities :- 82.9% - 97.7% 
# Specificities :- >98.3% 


#################################################################################################################
######################### hyperparameter tuning and cross validation ###

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
# Number = 5 implies Number of folds in CV.
trainControl <- trainControl(method="cv", number=5)
# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.
metric <- "Accuracy"

#################################################################################################

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
# Defining the range of C
grid_linear <- expand.grid(C= c(0.001, 0.1 ,1 ,10 ,100)) 



fit.linear <- train(label ~ ., data = train, metric = "Accuracy", method = "svmLinear",
                    tuneGrid = grid_linear, preProcess = NULL,
                    trControl = trainControl)

# printing results of 5 cross validation
print(fit.linear) 
plot(fit.linear)

# Observations :- 
# Best accuracy of 92.2% at C = 1e-01 i.e. the value of 0.1

# Evaluate linear model on test data
eval_linear_cv <- predict(fit.linear, newdata = test)
confusionMatrix(eval_linear_cv, test$label)

# Observations
# Overall Accuracy :- 92.2%
# Sensitivities :- 86.6% - 97.7%
# Specificities :- 98.8% - 99.5%
# Model accuracy slightly increased. 

#========================================= Using Polynomial Kernel =========================

# The polynomial kernel: It is capable of creating nonlinear, polynomial decision boundaries 

# Creting a poly kernel model with value C = 1 and dregree = 2 
# degree:- parameter needed for kernel of type polynomial 
Model_poly1 <- ksvm(label ~ ., data = train, kernel = "polydot", scaled = FALSE, C = 1, 
                    kpar = list(degree = 2, scale = 1, offset = 1))
print(Model_poly1)

eval_poly1 <- predict(Model_poly1, newdata = test)
confusionMatrix(eval_poly1, test$label)

# Observations
# Overall Accuracy :- 95.4%
# Sensitivities :- 92.6% - 98.3%
# Specificities :- > 99%

## Creating Polynomial kernel with different offset
Model_poly2 <- ksvm(label ~ ., data = train, kernel = "polydot", scaled = FALSE, C = 1, 
                    kpar = list(degree = 2, scale = 1, offset = 10))
print(Model_poly2)

eval_poly2 <- predict(Model_poly2, newdata = test)
confusionMatrix(eval_poly2, test$label)

# Observations :- 
# Overall Accuracy :- 95.4
# Sensitivities :- 92.6% - 98.4%
# specificities :- >99%
# Model performance same as previous model

## Creating Polynomial kernel with higher C value
Model_poly3 <- ksvm(label ~ ., data = train, kernel = "polydot", scaled = FALSE, C = 5, 
                    kpar = list(degree = 2, scale = 1, offset = 1))
print(Model_poly3)

eval_poly3 <- predict(Model_poly3, newdata = test)
confusionMatrix(eval_poly3, test$label)

# Observations :- 
# Overall Accuracy :- 95.4
# Sensitivities :- 92.6% - 98.3%
# specificities :- >99%
# Model performance same as previous model


################## Hyperparameter and Cross Validation ###########

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
grid_poly = expand.grid(C= c(0.01, 0.1, 1, 10), degree = c(1, 2, 3, 4, 5), 
                        scale = c(-100, -10, -1, 1, 10, 100))

fit.poly <- train(label ~ ., data = train, metric = "Accuracy", method = "svmPoly",tuneGrid = grid_poly,
                  trControl = trainControl, preProcess = NULL)

# printing results of cross validation
print(fit.poly) 
plot(fit.poly)

eval_cv_poly <- predict(fit.poly, newdata = test)
confusionMatrix(eval_cv_poly, test$label)

# observation
# Accuracy :- 95.4$


##=========================================== Using RBF Kernel ======================================
# The radial basis function (RBF) kernel: This is the most complex one, 
# which is capable of transforming highly nonlinear feature spaces to linear ones. 
# It is even capable of creating elliptical (i.e. enclosed) decision boundaries

# Radial kernel with default values.
# Default parameter of kpar is "Automatic"
Model_RBF <- ksvm(label~ ., data = train, scaled = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, test)

print(Model_RBF)
#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$label)

###### Observations
# Accuracy :- 94.9%
# Sensitivities :- 92.3% - 97.7%
# Specificities :- >99%

# Radial kernel with different value of sigma
Model_RBF1 <- ksvm(label~., data = train, scaled = FALSE, kernel = "rbfdot", kpar = list(sigma = 0.1))
Eval_RBF1<- predict(Model_RBF1, test)
#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF1,test$label)

# Observations 
# Overall Accuracy :- 83.9% (Drop 11% from preveious model)
# sensitivities :- 78.8% to 99.3%
# specificities :- >99% for all the classes except class2.
# Model is overfitting

############# Hyperparameter and cross validation ####################

# Defining ranges of C and Sigma
grid_rbf <- expand.grid(.sigma=c(0.025, 0.05), .C=c(0.1,0.5,1,2) )


#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

fit.rbf <- train(label~., data=train, method="svmRadial", metric=metric, 
                 tuneGrid=grid_rbf, trControl=trainControl)

print(fit.rbf)
plot(fit.rbf)

# Observations
#

eval_cv_rbf <- predict(fit.rbf, newdata = test)
confusionMatrix(eval_cv_rbf, test$label)

# Observations
# overall accuracy :- 96.4%
# Sensitivities :- 94.3% - 98.4%
# Specificities :- > 99%

################################# Final Model ###################################

# We got best Accuracy of model in Radial Basis Function Kernel
# so the final model is

Final_model <- fit.rbf
print(Final_model)
plot(Final_model)

# We achieved highest accuracy at svm using RBF kernel.
# The final values used in the model were :- Sigma = 0.025 and C = 2






