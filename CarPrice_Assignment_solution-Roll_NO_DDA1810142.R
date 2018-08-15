# FOLLOWING SCRIPT CONTAIN SOULTION TO
#
# COURSE :- Pridictive Analytics I
# MODULE :- Assignment Linear Regression
#
#Submitted by :- Ayushi Chouksey (Roll no. :- DDA1810142) 
#
#
#========================= ASSUMPTIONS =======================================
#
#  1. I assume that working directory is set throught Session-> Set Working Directory to the folder where all necessary files are kept
#  2. The data is not in zip file but exploded in the working directory.
#
setwd("D:/course 3/M2-assignment LR")
getwd()
#
#========================= CHECK  AND INSTALL PACAKGES =======================
#
#Check if pacakges are installed or not and if not get them installed
#This is done so that If a package is not installed in evaluators environment
#script should not fail cost us marks.
#
#
#Required pacakge list
pkg <-
  c(
    "tidyr",
    "dplyr",
    "stringr",
    "tools",
    "ggplot2",
    "lubridate",
    "reshape2",
    "ggcorrplot",
    "corrplot",
    "car",
    "MASS",
    "bindrcpp"
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

######################### Loading Data to a variable #########################

carprice <- read.csv("CarPrice_Assignment.csv")
str(carprice)


##################### Understanding data in term of business understanding ##################

###Information about car for company's perspective
#carId - Unique id of each observation (Interger)		
#symboling - Its assigned insurance risk rating, A value of +3 indicates that the auto is risky, -3 that it is probably pretty safe.(Categorical) 		
#car_name(company+model) - Name of car company (Categorical)		
#carprice - price of the car

### Car's outer body information
#Door_no - Number of doors in a car (Categorical)		
#car_body - body of car (Categorical)		
#drivewheel - type of drive wheel (Categorical)		
#carlength - Length of car (Numeric)		
#carwidth - Width of car (Numeric)		
#carheight - Height of car (Numeric)		


### car's inner mechanical information
#fuletype - Car fuel type i.e gas or diesel (Categorical)		
#aspiration - Aspiration used in a car (Categorical)		
#engine location - Location of car engine (Categorical)		
#wheelbase - Weelbase of car (Numeric)		
#curbweight - The weight of a car without occupants or baggage. (Numeric)		
#enginetype - Type of engine. (Categorical)		
#cylindernumber - cylinder placed in the car (Categorical)		
#enginesize - Size of engine (Numeric)		
#fulesystem - Fuel system of car (Categorical)		
#boreration - Boreratio of car (Numeric)		
#stroke -  Stroke or volume inside the engine (Numeric)		
#compressionratio - compression ratio of car (Numeric)		
#horsepower - Horsepower (Numeric)		
#peakrpm - car peak rpm (Numeric)		
#citympg - Mileage in city (Numeric)
#highwaympg - Mileage in highway (Numeric)

##################### Meta Data ####################
# If you glance the meta data,you could find three different types of variables such as 
#variable related to company's perspective
#variable related to car's structure
#variable related to car's inner functionality

######################## Business Objective #######################
# Which variables are significant in predicting the price of a car
# How well those variables describe the price of a car

####################### Data Cleaning #####################

# Check for duplicate values
sum(duplicated(carprice$car_ID))

# Check for NA values
#count of NA values by column: just to verify if the NA values cleared
carprice %>%
  summarise_all(funs(sum(is.na(.))))
sum(is.na(carprice)) # no NA values

# Check missing values
# Treating invalid values, any variable having more than 15% of data points
# missing is not eligible for imputation hence it makes sense to compute
# and drop those variables
missing_values <- carprice %>%
  summarise_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values,key='feature',value = 'missing_percentage')
missing_values # no missing values 

# check for duplicated rows
Car_dup<-unique(carprice$car_ID)
nrow(carprice) # no duplicate values

#remove car_id variable
carprice$car_ID <- NULL

#seprate variable CarName into two variables. first company name and second model.
#consider only company name as the independent variable for the model building
carprice <- separate(carprice, CarName, c('company', 'model'), sep = '[[:blank:]]', extra = 'merge', fill = 'right')


#Removing the Car Model name as it should not be considered as an independent variable
carprice$model<-NULL
View(carprice)

#Cleaning the Incorrect names of the Car Companies:

unique(carprice$company)

change_company <- function(name){
  changed_name <- name
  if(name=='maxda'){
    changed_name = 'mazda'
  } else if (name=='Nissan') {
    changed_name = 'nissan'
  } else if (name == 'porcshce') {
    changed_name = 'porsche'
  } else if (name == 'toyouta') {
    changed_name = 'toyota'
  } else if (name %in% c('vokswagen', 'vw')){
    changed_name = 'volkswagen'
  } else if (name=='alfa-romero'){
    changed_name = 'alfa-romeo'
  } 
  return(changed_name)
}
carprice$company <- sapply(carprice$company, change_company)

unique(carprice$company)
View(carprice)

# Remove the outliers


# check and set the outliers in variable wheelbase
quantile(carprice$wheelbase,seq(0,1,0.01))
carprice$wheelbase[which(carprice$wheelbase>114.00)]<-114.00
carprice$wheelbase[which(carprice$wheelbase<93.00)]<-93.00
boxplot(carprice$wheelbase)

# check and set the outliers in variable carlength
quantile(carprice$carlength,seq(0,1,0.01))
carprice$carlength[which(carprice$carlength>203.00)]<-203.00
carprice$carlength[which(carprice$carlength<150.00)]<-150.00
boxplot(carprice$carlength)

# check and set the outliers in variable carwidth
quantile(carprice$carwidth,seq(0,1,0.01))
carprice$carwidth[which(carprice$carwidth>70.00)]<-70.00
boxplot(carprice$carwidth)

# check and set the outliers in variable carheight
quantile(carprice$carheight,seq(0,1,0.01))
boxplot(carprice$carheight)

# check and set the outliers in variable curbweight
quantile(carprice$curbweight,seq(0,1,0.01))
boxplot(carprice$curbweight)

# check and set the outliers in variable enginesize
quantile(carprice$enginesize,seq(0,1,0.01))
carprice$enginesize[which(carprice$enginesize>200.00)]<-200.00
boxplot(carprice$enginesize)

# check and set the outliers in variable boreratio
quantile(carprice$boreratio,seq(0,1,0.01))
boxplot(carprice$boreratio)

# check and set the outliers in variable stroke
quantile(carprice$stroke,seq(0,1,0.01))
carprice$stroke[which(carprice$stroke>3.80)]<-3.80
carprice$stroke[which(carprice$stroke<3.00)]<-3.00
boxplot(carprice$stroke)

# check and set the outliers in variable compressionratio
quantile(carprice$compressionratio,seq(0,1,0.01))
carprice$compressionratio[which(carprice$compressionratio>10.00)]<-10.00
carprice$compressionratio[which(carprice$compressionratio<7.50)]<-7.50
boxplot(carprice$compressionratio)

# check and set the outliers in variable horsepower
quantile(carprice$horsepower,seq(0,1,0.01))
carprice$horsepower[which(carprice$horsepower > 185.00)] <- 185.00
boxplot(carprice$horsepower)

# check and set the outliers in variable peakrpm
quantile(carprice$peakrpm,seq(0,1,0.01))
carprice$peakrpm[which(carprice$peakrpm>6000.00)]<-6000.00
boxplot(carprice$peakrpm)

# check and set the outliers in variable citympg
quantile(carprice$citympg,seq(0,1,0.01))
carprice$citympg[which(carprice$citympg>45.00)]<-45.00
boxplot(carprice$citympg)

# check and set the outliers in variable highwaympg
quantile(carprice$highwaympg,seq(0,1,0.01))
carprice$highwaympg[which(carprice$highwaympg>45.00)]<-45.00
boxplot(carprice$highwaympg)

#### Removed all the outliers from the dataframe

#Converting Factor Variable FuelType to Numeric:
unique(carprice$fueltype)
levels(carprice$fueltype)<-c(1,0)
carprice$fueltype<-as.numeric(levels(carprice$fueltype))[carprice$fueltype]


#Converting Factor Variable Aspiration to Numeric:
unique(carprice$aspiration)
levels(carprice$aspiration)<-c(1,0)
carprice$aspiration<-as.numeric(levels(carprice$aspiration))[carprice$aspiration]


#converting Factor variable DoorNumber to Numeric:
unique(carprice$doornumber)
levels(carprice$doornumber)<-c(1,0)
carprice$doornumber<-as.numeric(levels(carprice$doornumber))[carprice$doornumber]


#converting Factor variable Engine allocation to Numeric:
unique(carprice$enginelocation)
levels(carprice$enginelocation)<-c(1,0)
carprice$enginelocation<-as.numeric(levels(carprice$enginelocation))[carprice$enginelocation]


############################ Dummy variables ################
# Now we come across variables having more than 3 levels. 
summary(factor(carprice$carbody))
unique(carprice$carbody)
#Converting "carbody" into dummies . 
dummy_1 <- data.frame(model.matrix( ~carbody, data = carprice))
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "carbody" 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "carbody" column
carprice <- cbind(carprice[,-6], dummy_1)
View(carprice)


#Converting Factor Variable drivewheel to Numeric:
summary(factor(carprice$drivewheel))
dummy_2<-data.frame(model.matrix(~drivewheel,data = carprice))
dummy_2<-dummy_2[,-1]
#Combine the Dummy variable drivewheel to the Main Matrix:
carprice<-cbind(carprice[,-6],dummy_2)

#Converting Factor Variable enginetype to Numeric:
summary(factor(carprice$enginetype))
dummy_3<-data.frame(model.matrix(~enginetype,data = carprice))
dummy_3<-dummy_3[,-1]
#Combine the Dummy variable enginetype to the Main Matrix:
carprice<-cbind(carprice[,-12],dummy_3)


#Converting Factor Variable cylindernumber to Numeric:
summary(factor(carprice$cylindernumber))
dummy_4<-data.frame(model.matrix(~cylindernumber,data = carprice))
dummy_4<-dummy_4[,-1]
#Combine the Dummy variable fuelsystem to the Main Matrix:
carprice<-cbind(carprice[,-12],dummy_4)

#Converting Factor Variable fuelsystem to Numeric:
summary(factor(carprice$fuelsystem))
dummy_5<-data.frame(model.matrix(~fuelsystem,data = carprice))
dummy_5<-dummy_5[,-1]
#Combine the Dummy variable fuelsystem to the Main Matrix:
carprice<-cbind(carprice[,-13],dummy_5)

#Converting Factor Variable company to Numeric:
summary(factor(carprice$company))
dummy_6<-data.frame(model.matrix(~company,data = carprice))
dummy_6<-dummy_6[,-1]
#Combine the Dummy variable fuelsystem to the Main Matrix:
carprice<-cbind(carprice[,-2],dummy_6)


#check NA values again
carprice %>%
  summarise_all(funs(sum(is.na(.))))
sum(is.na(carprice))


#### Here we done with the data prepration part #############
####################################################################################################################

############# Multiple linear regression model ################

# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(carprice), 0.7*nrow(carprice))
# generate the train data set
train = carprice[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = carprice[-trainindices,]

#Execute the first model_1 multilinear model in the training set. 
model_1 <-lm(price~.,data=train)

# Check the summary of model. 
summary(model_1)

# Now, lets see how to use stepAIC

# In stepAIC function, we pass our first model i.e model_1 and 
# direction is ser as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously 

# We have a total of 65 variables considered into the model 
#Now let;s run the code. 

step <- stepAIC(model_1, direction="both")
#Great, so many iterations have been done through the stepwise command. 
# now we need to know our model equation so lets write the Step command here. 

step
# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Now store the last model equation of stepwise method into an object called model_2

# Let's execute this model here

model_2 <- lm(formula = price ~ symboling + fueltype + aspiration + enginelocation + 
                wheelbase + curbweight + enginesize + stroke + compressionratio + 
                peakrpm + citympg + carbodywagon + drivewheelfwd + enginetypel + 
                enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree + fuelsystem2bbl + 
                fuelsystemmpfi + companybmw + companybuick + companydodge + 
                companyhonda + companyisuzu + companyjaguar + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + 
                companyrenault + companysaab + companytoyota + companyvolkswagen + 
                companyvolvo, data = train)
summary(model_2)
vif(model_2)

# The variable "cylindernumberthree" is insignificant p- value(0.21) > 0.05
# so we remove the variable "cylindernumberthree"

model_3 <- lm(formula = price ~ symboling + fueltype + aspiration + enginelocation + 
                wheelbase + curbweight + enginesize + stroke + compressionratio + 
                peakrpm + citympg + carbodywagon + drivewheelfwd + enginetypel + 
                enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + 
                fuelsystemmpfi + companybmw + companybuick + companydodge + 
                companyhonda + companyisuzu + companyjaguar + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + 
                companyrenault + companysaab + companytoyota + companyvolkswagen + 
                companyvolvo, data = train)
summary(model_3)
vif(model_3)

# Now we see the variable "citympg" have become highly insignificant p-value (0.23) > 0.05
# so we remove the vriable "citympg"

model_4 <- lm(formula = price ~ symboling + fueltype + aspiration + enginelocation + 
                wheelbase + curbweight + enginesize + stroke + compressionratio + 
                peakrpm + carbodywagon + drivewheelfwd + enginetypel + 
                enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + 
                fuelsystemmpfi + companybmw + companybuick + companydodge + 
                companyhonda + companyisuzu + companyjaguar + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + 
                companyrenault + companysaab + companytoyota + companyvolkswagen + 
                companyvolvo, data = train)
summary(model_4)
vif(model_4)

# Now we see the variable "companyhonda" have become highly insignificant p-value (0.15) > 0.05
# so we remove the vriable "companyhonda"

model_5 <- lm(formula = price ~ symboling + fueltype + aspiration + enginelocation + 
                wheelbase + curbweight + enginesize + stroke + compressionratio + 
                peakrpm + carbodywagon + drivewheelfwd + enginetypel + 
                enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + 
                fuelsystemmpfi + companybmw + companybuick + companydodge + 
                companyisuzu + companyjaguar + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + 
                companyrenault + companysaab + companytoyota + companyvolkswagen + 
                companyvolvo, data = train)
summary(model_5)
vif(model_5)

# Now we see the variable "peakrpm" have become highly insignificant p-value (0.16) > 0.05
# so we remove the vriable "peakrpm"

model_6 <- lm(formula = price ~ symboling + fueltype + aspiration + enginelocation + 
                wheelbase + curbweight + enginesize + stroke + compressionratio + 
                carbodywagon + drivewheelfwd + enginetypel + 
                enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + 
                fuelsystemmpfi + companybmw + companybuick + companydodge + 
                companyisuzu + companyjaguar + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + 
                companyrenault + companysaab + companytoyota + companyvolkswagen + 
                companyvolvo, data = train)
summary(model_6)
vif(model_6)


# Now we see the variable "curbweight" have highest VIF = 26.00
# so we remove the vriable "curbweight"

model_7 <- lm(formula = price ~ symboling + fueltype + aspiration + enginelocation + 
                wheelbase + enginesize + stroke + compressionratio + 
                carbodywagon + drivewheelfwd + enginetypel + 
                enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + 
                fuelsystemmpfi + companybmw + companybuick + companydodge + 
                companyisuzu + companyjaguar + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + 
                companyrenault + companysaab + companytoyota + companyvolkswagen + 
                companyvolvo, data = train)
summary(model_7)
vif(model_7)

# Now we see the variable "drivewheelfwd" have become highly insignificant p-value-0.64 > 0.05
# so we remove the vriable "drivewheelfwd"

model_8 <- lm(formula = price ~ symboling + fueltype + aspiration + enginelocation + 
                wheelbase + enginesize + stroke + compressionratio + 
                carbodywagon + enginetypel + 
                enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + 
                fuelsystemmpfi + companybmw + companybuick + companydodge + 
                companyisuzu + companyjaguar + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + 
                companyrenault + companysaab + companytoyota + companyvolkswagen + 
                companyvolvo, data = train)
summary(model_8)
vif(model_8)

# Now we see the variable "carbodywagon" have become highly insignificant p-value-0.53 > 0.05
# so we remove the vriable "carbodywagon"

model_9 <- lm(formula = price ~ symboling + fueltype + aspiration + enginelocation + 
                wheelbase + enginesize + stroke + compressionratio + 
                enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + 
                fuelsystemmpfi + companybmw + companybuick + companydodge + 
                companyisuzu + companyjaguar + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + 
                companyrenault + companysaab + companytoyota + companyvolkswagen + 
                companyvolvo, data = train)
summary(model_9)
vif(model_9)

# Now we see the variable "symboling" have become highly insignificant p-value-0.11 > 0.05
# so we remove the vriable "symboling"

model_10 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                wheelbase + enginesize + stroke + compressionratio + 
                enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + 
                fuelsystemmpfi + companybmw + companybuick + companydodge + 
                companyisuzu + companyjaguar + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + 
                companyrenault + companysaab + companytoyota + companyvolkswagen + 
                companyvolvo, data = train)
summary(model_10)
vif(model_10)


# Now we see the variable "fuelsystem2bbl" have become highly insignificant p-value-0.91 > 0.05
# so we remove the vriable "fuelsystem2bbl"

model_11 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                wheelbase + enginesize + stroke + compressionratio + 
                enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystemmpfi + companybmw + companybuick + companydodge + 
                companyisuzu + companyjaguar + companymazda + 
                companymercury + companymitsubishi + companynissan + companyplymouth + 
                companyrenault + companysaab + companytoyota + companyvolkswagen + 
                companyvolvo, data = train)
summary(model_11)
vif(model_11)


# Now we see the variable "companyrenault" have become highly insignificant p-value-0.13 > 0.05
# so we remove the vriable "companyrenault"

model_12 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                 wheelbase + enginesize + stroke + compressionratio + 
                 enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + fuelsystemmpfi + companybmw + companybuick + companydodge + 
                 companyisuzu + companyjaguar + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + 
                 companysaab + companytoyota + companyvolkswagen + 
                 companyvolvo, data = train)
summary(model_12)
vif(model_12)

# Now we see the variable "companysaab" have become highly insignificant p-value-0.06 > 0.05
# so we remove the vriable "companysaab"

model_13 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                 wheelbase + enginesize + stroke + compressionratio + 
                 enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + fuelsystemmpfi + companybmw + companybuick + companydodge + 
                 companyisuzu + companyjaguar + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + 
                 companytoyota + companyvolkswagen + 
                 companyvolvo, data = train)
summary(model_13)
vif(model_13)

# Now we see the variable "fuelsystemmpfi" have become highly insignificant p-value-0.07 > 0.05
# so we remove the vriable "fuelsystemmpfi"

model_14 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                 wheelbase + enginesize + stroke + compressionratio + 
                 enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + companybmw + companybuick + companydodge + 
                 companyisuzu + companyjaguar + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + 
                 companytoyota + companyvolkswagen + 
                 companyvolvo, data = train)
summary(model_14)
vif(model_14)

# Now we see the variable "fueltype" have become highly insignificant p-value-0.18 > 0.05
# so we remove the vriable "fueltype"

model_15 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + enginesize + stroke + compressionratio + 
                 enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + companybmw + companybuick + companydodge + 
                 companyisuzu + companyjaguar + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + 
                 companytoyota + companyvolkswagen + 
                 companyvolvo, data = train)
summary(model_15)
vif(model_15)

# Now we see the variable "companyvolkswagen" have become highly insignificant p-value-0.18 > 0.05
# so we remove the vriable "companyvolkswagen"

model_16 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + enginesize + stroke + compressionratio + 
                 enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + companybmw + companybuick + companydodge + 
                 companyisuzu + companyjaguar + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + 
                 companytoyota + companyvolvo, data = train)
summary(model_16)
vif(model_16)

# Now we see the variable "cylindernumbersix" have highest vif - 5.6
# so we remove the vriable "cylindernumbersix"

model_17 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + enginesize + stroke + compressionratio + 
                 enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                  companybmw + companybuick + companydodge + 
                 companyisuzu + companyjaguar + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + 
                 companytoyota + companyvolvo, data = train)
summary(model_17)
vif(model_17)

# Now we see the variable "companybmw" have become highly insignificant p-value-0.09 > 0.05
# so we remove the vriable "companybmw"

model_18 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + enginesize + stroke + compressionratio + 
                 enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 companybuick + companydodge + 
                 companyisuzu + companyjaguar + companymazda + 
                 companymercury + companymitsubishi + companynissan + companyplymouth + 
                 companytoyota + companyvolvo, data = train)
summary(model_18)
vif(model_18)


# Now we see the variable "companymercury" have become highly insignificant p-value-0.019
# so we remove the vriable "companymercury"

model_19 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + enginesize + stroke + compressionratio + 
                 enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 companybuick + companydodge + 
                 companyisuzu + companyjaguar + companymazda + 
                 companymitsubishi + companynissan + companyplymouth + 
                 companytoyota + companyvolvo, data = train)
summary(model_19)
vif(model_19)


# Now we see the variable "companysuzu" have become highly insignificant p-value-0.020
# so we remove the vriable "companysuzu"

model_20 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + enginesize + stroke + compressionratio + 
                 enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 companybuick + companydodge + companyjaguar + companymazda + 
                 companymitsubishi + companynissan + companyplymouth + 
                 companytoyota + companyvolvo, data = train)
summary(model_20)
vif(model_20)


# Now we see the variable "companyvolvo" have become highly insignificant p-value-0.0064
# so we remove the vriable "companyvolvo"

model_21 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + enginesize + stroke + compressionratio + 
                 enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 companybuick + companydodge + companyjaguar + companymazda + 
                 companymitsubishi + companynissan + companyplymouth + 
                 companytoyota , data = train)
summary(model_21)
vif(model_21)


# Now we see the variable "companymazda" have become highly insignificant p-value-0.014
# so we remove the vriable "companymazda"

model_22 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + enginesize + stroke + compressionratio + 
                 enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 companybuick + companydodge + companyjaguar + 
                 companymitsubishi + companynissan + companyplymouth + 
                 companytoyota , data = train)
summary(model_22)
vif(model_22)


# Now we see the variable "compressionratio" have become highly insignificant p-value-0.014
# so we remove the vriable "compressionratio"

model_23 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + enginesize + stroke + 
                 enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 companybuick + companydodge + companyjaguar + 
                 companymitsubishi + companynissan + companyplymouth + 
                 companytoyota , data = train)
summary(model_23)
vif(model_23)



# Now we see the variable "companydodge" have become highly insignificant p-value-0.038
# so we remove the vriable "companydodge"

model_24 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + enginesize + stroke + 
                 enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 companybuick + companyjaguar + 
                 companymitsubishi + companynissan + companyplymouth + 
                 companytoyota , data = train)
summary(model_24)
vif(model_24)


# Now we see the variable "companyplymouth" have become highly insignificant p-value-0.043
# so we remove the vriable "companyplymouth"

model_25 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + enginesize + stroke + 
                 enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 companybuick + companyjaguar + 
                 companymitsubishi + companynissan + 
                 companytoyota , data = train)
summary(model_25)
vif(model_25)

# Now we see the variable "companymitsubishi" have become highly insignificant p-value-0.068 > 0.05
# so we remove the vriable "companymitsubishi"

model_26 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + enginesize + stroke + 
                 enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 companybuick + companyjaguar +  companynissan + companytoyota , data = train)
summary(model_26)
vif(model_26)

# Now we see the variable "companynissan" have become highly insignificant p-value-0.034
# so we remove the vriable "companynissan"

model_27 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + enginesize + stroke + 
                 enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 companybuick + companyjaguar +  companytoyota , data = train)
summary(model_27)
vif(model_27)

# Now we see the variable "aspiration" have become highly insignificant p-value-0.0032
# so we remove the vriable "aspiration"

model_28 <- lm(formula = price ~  enginelocation + 
                 wheelbase + enginesize + stroke + 
                 enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                 companybuick + companyjaguar +  companytoyota , data = train)
summary(model_28)
vif(model_28)

# Now we see the variable "cylindernumberfive" have become highly insignificant p-value-0.0066
# so we remove the vriable "cylindernumberfive"

model_29 <- lm(formula = price ~  enginelocation + 
                 wheelbase + enginesize + stroke + 
                 enginetypel + enginetypeohcf + cylindernumberfour + 
                 companybuick + companyjaguar +  companytoyota , data = train)
summary(model_29)
vif(model_29)


# Now we see the variable "enginetypel" have become highly insignificant p-value-0.0088
# so we remove the vriable "enginetypel"

model_30 <- lm(formula = price ~  enginelocation + 
                 wheelbase + enginesize + stroke + enginetypeohcf + cylindernumberfour + 
                 companybuick + companyjaguar +  companytoyota , data = train)
summary(model_30)
vif(model_30)


# Now we see the variable "enginetypeohcf" have become highly insignificant p-value-0.0017
# so we remove the vriable "enginetypeohcf"

model_31 <- lm(formula = price ~  enginelocation + 
                 wheelbase + enginesize + stroke +  cylindernumberfour + 
                 companybuick + companyjaguar +  companytoyota , data = train)
summary(model_31)
vif(model_31)

# Now we see the variable "enginesize" have highest VIF
# so we remove the vriable "enginesize"

model_32 <- lm(formula = price ~  enginelocation + 
                 wheelbase  + stroke +  cylindernumberfour + 
                 companybuick + companyjaguar +  companytoyota , data = train)
summary(model_32)
vif(model_32)

# Now we see the variable "stroke" have become highly insignificant p-value-0.12
# so we remove the vriable "stroke"

model_33 <- lm(formula = price ~  enginelocation + 
                 wheelbase  +  cylindernumberfour + 
                 companybuick + companyjaguar +  companytoyota , data = train)
summary(model_33)
vif(model_33)

# Now we see the variable "companytoyota" have become highly insignificant p-value-0.053
# so we remove the vriable "companytoyota"

model_34 <- lm(formula = price ~  enginelocation + 
                 wheelbase  +  cylindernumberfour + 
                 companybuick + companyjaguar  , data = train)
summary(model_34)
vif(model_34)

# Now we see the variable "companyjagaur" have become highly insignificant p-value-0.00182
# so we remove the vriable "companyjagaur"

model_35 <- lm(formula = price ~  enginelocation + 
                 wheelbase  +  cylindernumberfour + 
                 companybuick , data = train)
summary(model_35)
vif(model_35)

###################### R squared is  and adjusted R squared ############## 

# R squared - 79% (0.7987)
# Adjusted R squared - 79% (0.7929)

predicted_price <- predict(model_35, test[,-which(colnames(test)=='price')])
test$test_price <- predicted_price

r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared 

#predicted price
carprice$predicted_price <- predict(model_35, carprice)
carprice$error <-  carprice$price- carprice$predicted_price

###############################  PLOTS  ###########################################

# residual plot
residualPlot(model_35)

ggplot(carprice, aes(price, error)) +geom_point() +geom_smooth() # It shows as price increases error also increases.

############################# Analysis ###########

# Variables which are significant in predicting the price of a car
# Enginelocation
# Wheelbase
# Cylindernumberfour
# companybuick

