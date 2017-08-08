getwd()
setwd("C:/Users/Bala/Documents/House Dataset")

options(scipen = 999)
library(dplyr)
library(ggplot2)
library(lattice)
library(caret)
library(ggrepel)
library(factoextra)
library(foreach)
library(glmnet)
library(xgboost)

# Import train data

# For now import train and test as Factors = FALSE, so that we can impute missing values, perform Feature Engineering and then change them to Factors

train <- read.csv("C:\\Users\\Bala\\Documents\\House Dataset\\train.csv", stringsAsFactors = FALSE)

# Import test data

test <- read.csv("C:\\Users\\Bala\\Documents\\House Dataset\\test.csv", stringsAsFactors = FALSE)

################################## DATA PREPARATION #########################################

# This dataset has more missing values in the test than in train and the best way to handle this is to bind
# train and test together and fill up the missing values and then separate them back. In order to ensure that
# train records before bind will be the same after bind, we are introducing a variable Data that fills "Train"
# for all traindata and "Test" for all testdata. 

#As we are binding both train and test, let me create salePrice in test as well and assign it to NA

test$SalePrice <- NA

train$Data <- "Train"
test$Data <- "Test"

# Before dealing with Missing values we found #-of-factors which are not equal between 
# train and test dataset . if we did not impute, this will bring "contrast error" during prediction

# Utilities
# Condition2
# HouseStyle
# RoofMatl
# Exterior1st
# Exterior2nd
# Heating
# Electrical
# GarageQual
# PoolQC
# MiscFeature

# Utilities

table(train$Utilities, useNA = "ifany")
table(test$Utilities, useNA = "ifany")

test$Utilities[is.na(test$Utilities)] <- rep("NoSeWa", 2)

# Condition2
table(train$Condition2, useNA = "ifany")

table(test$Condition2, useNA = "ifany")

train$Condition2 <- ifelse(train$Condition2 == "Artery" | train$Condition2 == "Feedr" | train$Condition2 == "Norm",
                           train$Condition2, "Other_Condn")

test$Condition2 <- ifelse(test$Condition2 == "PosA" | test$Condition2 == "PosN",
                          "Other_Condn", test$Condition2)

#House_Style

table(train$HouseStyle, useNA = "ifany")
table(test$HouseStyle, useNA = "ifany")

Fst <- c("1.5Fin","1.5Unf","1Story")
Snd <- c("2.5Fin","2.5Unf","2Story")

train$HouseStyle <- ifelse(train$HouseStyle %in% Fst,"1+Story",
                           ifelse(train$HouseStyle %in% Snd, "2+Story",train$HouseStyle))

test$HouseStyle <- ifelse(test$HouseStyle %in% Fst, "1+Story",
                          ifelse(test$HouseStyle %in% Snd, "2+Story", test$HouseStyle))

#RoofMatl

table(train$RoofMatl, useNA = "ifany")
table(test$RoofMatl, useNA = "ifany")

train$RoofMatl <- ifelse(train$RoofMatl == "CompShg","CompShg",
                         ifelse(train$RoofMatl == "Tar&Grv","Tar&Grv","Othr_RM"))

test$RoofMatl <- ifelse(test$RoofMatl == "CompShg","CompShg",
                        ifelse(test$RoofMatl == "Tar&Grv","Tar&Grv","Othr_RM"))

# Exterior1st

table(train$Exterior1st, useNA = "ifany")
table(test$Exterior1st, useNA = "ifany")

test$Exterior1st[is.na(test$Exterior1st)] <- rep("Plywood", 1)

train$Exterior1st <- ifelse(train$Exterior1st == "ImStucc" | train$Exterior1st == "Stone" | train$Exterior1st == "CBlock",
                            "Other",train$Exterior1st)


test$Exterior1st <- ifelse(test$Exterior1st == "CBlock", "Other", test$Exterior1st)

#Exterir2nd

table(train$Exterior2nd, useNA = "ifany")
table(test$Exterior2nd, useNA = "ifany")

test$Exterior2nd[is.na(test$Exterior2nd)] <- rep("Plywood",1)

train$Exterior2nd <- ifelse(train$Exterior2nd == "ImStucc" | train$Exterior2nd == "Stone" | train$Exterior2nd == "CBlock",
                            "Other",train$Exterior2nd)

test$Exterior2nd <- ifelse(test$Exterior2nd == "ImStucc" | test$Exterior2nd == "Stone" | test$Exterior2nd == "CBlock",
                           "Other",test$Exterior2nd)

# Heating

table(train$Heating, useNA = "ifany")
table(test$Heating, useNA = "ifany")

train$Heating <- ifelse(train$Heating == "Grav" | train$Heating == "OthW" | train$Heating == "Wall" | train$Heating == "Floor",
                        "Other",train$Heating)

test$Heating <- ifelse(test$Heating == "Grav" | test$Heating == "Wall","Other", test$Heating)

#ELectrical

table(train$Electrical, useNA = "ifany")
table(test$Electrical, useNA = "ifany")

train$Electrical[is.na(train$Electrical)] <- rep("Other",1)

train$Electrical <- ifelse(train$Electrical == "FuseP" | train$Electrical == "Mix",
                           "Other", train$Electrical)

test$Electrical <- ifelse(test$Electrical == "FuseP","Other", test$Electrical)

#GarageQual

table(train$GarageQual, useNA = "ifany")
table(test$GarageQual, useNA = "ifany")

train$GarageQual[is.na(train$GarageQual)] <- rep("No_Garage", 81)

train$GarageQual <- ifelse(train$GarageQual == "Po" | train$GarageQual == "Ex",
                           "Others", train$GarageQual)

test$GarageQual <- ifelse(test$GarageArea == 360 & is.na(test$GarageQual) ,"TA",test$GarageQual)

test$GarageQual[is.na(test$GarageQual)] <- rep("No_Garage", 77)

test$GarageQual <- ifelse(test$GarageQual == "Po", "Others", test$GarageQual)

#PoolQC

table(train$PoolQC, useNA = "ifany")
table(test$PoolQC, useNA = "ifany")

train$PoolQC <- ifelse(train$PoolQC == "Other_ratings" & train$PoolArea == "0", "No_Pool", train$PoolQC)

train$PoolQC <- ifelse(train$PoolQC == "Fa" | train$PoolQC == "Gd","Other_ratings",
                       ifelse(train$PoolQC == "Ex","Ex", train$PoolQC))

train$PoolQC[is.na(train$PoolQC)] <- rep("No_Pool", 1453)

test$PoolQC[is.na(test$PoolQC)] <- rep("No_Pool", 1456)

test$PoolQC <- ifelse(test$PoolQC == "Gd", "Other_ratings", test$PoolQC)

# MiscFeature

table(train$MiscFeature, useNA = "ifany")
table(test$MiscFeature, useNA = "ifany")

train$MiscFeature[is.na(train$MiscFeature)] <- rep("No_Misc",1406) 

test$MiscFeature[is.na(test$MiscFeature)] <- rep("No_Misc",1408)

train$MiscFeature <- ifelse(train$MiscFeature == "TenC","Othr",train$MiscFeature)

# Bind Train and Test datasets

house_data <- rbind(train,test) 

colnames(house_data[,sapply(house_data, function(x) any(is.na(x)))])

############################## Dealing with Intentional NAs ###################################

# There are certain Features in this dataset that are assigned as NA because there is no value for that.
# For example, if there is no Alley, the observations show NA for those. I call these as Intentional NAs

table(house_data$Alley, useNA = "ifany") # Alley

house_data$Alley[is.na(house_data$Alley)] <- rep("No_Alley",2721)

table(house_data$BsmtQual, useNA = "ifany") # Basement Quality

house_data$BsmtQual[is.na(house_data$BsmtQual)] <- rep("No_Basement",81)

table(house_data$BsmtCond, useNA = "ifany") # Basement Condition

house_data$BsmtCond[is.na(house_data$BsmtCond)] <- rep("No_Basement",82)

table(house_data$BsmtExposure, useNA = "ifany") # Basement Exposure

house_data$BsmtExposure[is.na(house_data$BsmtExposure)] <- rep("No_Basement",82)

table(house_data$BsmtFinType1, useNA = "ifany") # Bsmt FinType1

house_data$BsmtFinType1[is.na(house_data$BsmtFinType1)] <- rep("No_Basement",79)

table(house_data$BsmtFinType2, useNA = "ifany") # Bsmt FinType2

house_data$BsmtFinType2[is.na(house_data$BsmtFinType2)] <- rep("No_Basement",80)

table(house_data$FireplaceQu, useNA = "ifany") # FireplaceQu

house_data$FireplaceQu[is.na(house_data$FireplaceQu)] <- rep("No_Fireplace",1420)  

table(house_data$GarageYrBlt, useNA = "ifany") # Garage Year Built

house_data$GarageYrBlt[is.na(house_data$GarageYrBlt)] <- rep("No_Garage",159)

table(house_data$GarageType, useNA = "ifany") # Garage type

house_data$GarageType[is.na(house_data$GarageType)] <- rep("No_Garage",157)

table(house_data$GarageFinish, useNA = "ifany") # Garage Finish

house_data$GarageFinish[is.na(house_data$GarageFinish)] <- rep("No_Garage",159)

table(house_data$GarageQual, useNA = "ifany") # Garage quality

house_data$GarageQual[is.na(house_data$GarageQual)] <- rep("No_Garage",159)

table(house_data$GarageCond, useNA = "ifany") # Garage Condn

house_data$GarageCond[is.na(house_data$GarageCond)] <- rep("No_Garage",159)

table(house_data$Fence, useNA = "ifany") # Fence

house_data$Fence[is.na(house_data$Fence)] <- rep("No_Fence",2348)

table(is.na(house_data$GarageArea)) # GarageArea

house_data$GarageArea[is.na(house_data$GarageArea)] <- rep(0, 1)

table(house_data$GarageCars, useNA = "ifany")  # GarageCars

house_data$GarageCars[is.na(house_data$GarageCars)] <- rep(0, 1)

############# Other missing values imputation ###################

# Sale type is very imp. Analyze and predict

table(house_data$SaleType, useNA = "ifany")

aggregate(house_data[,79], list(house_data$SaleCondition), length)

# On analyzing the adjacent variables , we decide Sale Type to be WD

house_data$SaleType[is.na(house_data$SaleType)] <- rep("WD",1)

table(house_data$BsmtFinSF1, useNA = "ifany") 
house_data$BsmtFinSF1[is.na(house_data$BsmtFinSF1)] <- rep(0, 1) 

table(house_data$BsmtFinSF2 == 0)
house_data$BsmtFinSF2[is.na(house_data$BsmtFinSF2)] <- rep(0, 1) 

table(house_data$BsmtUnfSF == 0)
house_data$BsmtUnfSF[is.na(house_data$BsmtUnfSF)] <- rep(0, 1)

table(house_data$TotalBsmtSF == 0)
house_data$TotalBsmtSF[is.na(house_data$TotalBsmtSF)] <- rep(0,1)

house_data$BsmtFullBath[is.na(house_data$BsmtFullBath)] <- rep(0, 2)

house_data$BsmtHalfBath[is.na(house_data$BsmtHalfBath)] <- rep(0, 2)

house_data$BsmtHalfBath[is.na(house_data$BsmtHalfBath)] <- rep("TA", 1)

table(house_data$BsmtHalfBath, useNA = "ifany")

table(house_data$Functional, useNA = "ifany")

house_data$Functional[is.na(house_data$Functional)] <- rep("Typ", 2)

# MSZoning NA can be a house or a commercial building. see other features like bedroom , roof etc

house_data$MSZoning[is.na(house_data$MSZoning)] <- rep("RL", 4)

# KitchenQual - take the average and see the most relevant value and assign it

house_data$KitchenQual[is.na(house_data$KitchenQual)] <- rep("TA", 1)

# MasVnrType & MasVnrArea computation

house_data[(is.na(house_data$MasVnrArea)) | (is.na(house_data$MasVnrType)), c("MasVnrType","MasVnrArea")]

table(house_data$MasVnrType, useNA = "ifany")

# This only has one missing record, others are zero and None

house_data$MasVnrType <- ifelse(house_data$MasVnrArea == 198 & is.na(house_data$MasVnrType), "Stone", 
                                house_data$MasVnrType)

# Replace the NA MasVnrArea with zero

house_data$MasVnrArea[is.na(house_data$MasVnrArea)] <- rep(0, 23)

# Replace the NA MasVnrType with None

house_data$MasVnrType[is.na(house_data$MasVnrType)] <- rep("None", 23)


table(house_data$LotFrontage, useNA = "ifany")

neigh_mean_LF <- house_data[,c("LotFrontage","Neighborhood")] %>%
                  group_by(Neighborhood) %>%
                  summarise(LF_Median = median(LotFrontage, na.rm = TRUE)) 

neigh_mean_LF  # To display a table wise average of the LotFrontage and Neighborhood

LF_null <- which(is.na(house_data$LotFrontage))

ga <- house_data$LotFrontage

ga <- ave(1:nrow(house_data),house_data$Neighborhood, 
          FUN = function(i)mean(house_data$LotFrontage[i],na.rm = T))

ga <- round(ga, digits = 0)

house_data$LotFrontage[is.na(house_data$LotFrontage)] <- ga[is.na(house_data$LotFrontage)]

table(house_data$LotFrontage, useNA = "ifany")

colnames(house_data[,sapply(house_data[!names(house_data) %in% "SalePrice"],function(x) any(is.na(x)))])

# character(0) which means no missing values found

paste('There are', sum(sapply(house_data[!names(house_data) %in% "SalePrice"], is.na)), 'missing values left')

# [1] "There are 0 missing values left"