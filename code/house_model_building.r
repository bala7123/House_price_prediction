
source(house_data_prep.r) # Load data preparation of house data

source(house_feature_engg.r) # Load Feature Engineering of house data

# here we try Linear and RF models without any Feature Engineering. 
# This is to identify the Significant features of the model 
# And to see how the model performs without any FE


############################ Split data into Train and Test ########################

# here we can clearly see why we created a feature Data with only two values. We will not be using this
# until now to differentiate Train and Test data. 

house_train <- house_all[house_all$Data == "Train",]

house_test <- house_all[house_all$Data == "Test",]

SalePrice <- train$SalePrice

house_train <- cbind(house_train, SalePrice)

house_train$Data <- NULL
house_test$Data <- NULL

house_train$GarageYrBlt <- NULL
house_test$GarageYrBlt <- NULL

anyNA(house_train)
#FALSE

# Linear Regression modeling

# The IVs in the below model has been reduced from a set of 80 variables based on significance.

lm_model1 <- lm(house_train$SalePrice ~ MSSubClass+MSZoning+LotFrontage+LotArea+Street+LotShape+LandContour+LotConfig+ 
           Neighborhood+ Condition2+ BldgType+ HouseStyle+ OverallQual+OverallCond+YearBuilt+ YearRemodAdd+ 
           ExterQual+ BsmtQual+ BsmtExposure + TotalBsmtSF+ 
           X1stFlrSF+ X2ndFlrSF+ GrLivArea+ BsmtFullBath+ BsmtHalfBath+ FullBath+ HalfBath +BedroomAbvGr+ KitchenAbvGr+ 
           TotRmsAbvGrd+Fireplaces+ FireplaceQu+GarageType+ GarageCars+ GarageArea+ 
           YrSold+Other_Area_sqft+ Is_Renovated , data = house_train)

summary(lm_model1)

lm1_pred <- predict(lm_model1 , house_test[ names(house_test) %in% 
                                      c("MSSubClass","MSZoning","LotFrontage","LotArea",
"Street","LotShape","LandContour","LotConfig","Neighborhood","Condition2","BldgType", 
"HouseStyle","OverallQual","OverallCond","YearBuilt","YearRemodAdd","ExterQual", 
"BsmtQual","BsmtExposure","TotalBsmtSF","X1stFlrSF","X2ndFlrSF", "GrLivArea",
"BsmtFullBath","BsmtHalfBath","FullBath","HalfBath" ,"BedroomAbvGr","KitchenAbvGr",
"TotRmsAbvGrd","Fireplaces","FireplaceQu","GarageType", "GarageCars","GarageArea",
"YrSold","Other_Area_sqft","Is_Renovated")])

# The RMSE is one of the most used error metrics used in practice and gives us a good score to determine how well our model trains on new data

rmse_eval <- function(SalePrice, lm1_pred) {
  mse_eval <- sum((SalePrice - lm1_pred)^2) / length(SalePrice)
  return(sqrt(mse_eval))
}

# To plot using the above LM model

train_sp <- house_train$SalePrice[-1460] # As we have only 1359 values in house_test (One less than train)

result <- cbind(train_sp, mo_pred)

colnames(result) <- c("Actual Values","Predicted Values")

result <- as.data.frame(result)

plot(result$`Actual Values` ~ result$`Predicted Values`, data = house_test[-1460],
     main = "Actual price by Predicted price")

abline(a = 0, b = 1)


# Random Forest Modeling

house_rf1 <- randomForest(house_train$SalePrice ~. , data = house_train[-c(67)], 
                          importance = T, mtry = 50, nodesize = 15, ntrees = 10000)

house_rf1

rf1_pred <- predict(house_rf1, house_test[-67])

# To plot using the above RF model 

result1 <- cbind(house_train$SalePrice, house_rf2$predicted)

colnames(result1) <- c("Actual Values","Predicted Values")

result1 <- as.data.frame(result1)

plot(result1$`Actual Values` ~ result1$`Predicted Values`, data = house_test,
     main = "Actual price by Predicted price")

abline(a = 0, b = 1)
