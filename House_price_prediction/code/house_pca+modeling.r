
source(house_data_prep.r) # Load data preparation of house data

source(house_feature_engg.r) # Load Feature Engineering of house data

############################################## PCA ##################################################

# We already have seen PCA FA for analysis, now we shall try some modeling with PCA
# PCA takes only Numeric variables. As we know, categorical variables can explain a lot about a dataset
# So we are going to use the Pricipal Components along with the categorical variables in a regression and tree model 
# This is to see how well we can interpret the data and if it is effective 

# Train data - house_log_train
# Test data - house_log_test
# numeric_vars - a list of numeric variable names

names(house_log_train)
names(house_log_train[numeric_vars])
names(house_log_test[numeric_vars])

numeric_vars <- c(colnames(house_log_train[,sapply(house_log_train,is.numeric)])) 

# Run PCA

house_pca <- prcomp(house_log_train[numeric_vars], scale. = T, center = T)

summary(house_pca)

plot(house_pca, type = "lines")

house_pca$x 

dim(house_pca$x)

# Find std deviaiton of the PCs

st_dev <- house_pca$sdev

variance <- st_dev^2

variance[1:10]

var_prop <- variance / sum(variance)

var_prop

plot(var_prop, type = "b")

eg <- get_eigenvalue(house_pca)


# Take first certain Principal Components & do Factor Analysis

house_fact <- factanal(house_log_train[numeric_vars], factors = 6, rotation = "varimax", lower = 0.01)

house_fact


# Take the selected PComponents, add it with the char variables of Housing data

pca_train <- house_log_train[,which(sapply(house_log_train, is.factor))]

pca_train <- cbind(house_pca$x[,1:15], pca_train)

pca_train <- cbind(pca_train, SalePrice) # Adding SalePrice DV in the train data



# Run PCA on test

house_pca_test <- prcomp(house_log_test[numeric_vars], scale. = T, center = T)

plot(house_pca_test, type = "lines")


pca_test <- house_log_test[,which(sapply(house_log_test, is.factor))]

pca_test <- cbind(house_pca_test$x[,1:15], pca_test)


dim(pca_train)

dim(pca_test)


############################ Random Forest with PCA vals

rf_pca <- randomForest(pca_train$SalePrice ~. , data = pca_train, 
                       importance = T, mtry = 30, nodesize = 5, ntrees = 10000)

print(rf_pca)

importance(rf_pca, sort = TRUE, type = 1)

randomForest::varImpPlot(rf_pca, sort=T,n.var=20, type=1)

rf.pca.prediction1 <- predict(rf_pca, pca_test, type="class")

write.csv(rf.pca.prediction1, "bala_pred6.csv")

# To plot the actual vs Predicted value

pca_train <- pca_train$SalePrice[-1460]

result3 <- cbind(pca_train$SalePrice, rf_pred)

colnames(result3) <- c("Actual Values","Predicted Values")

result3 <- as.data.frame(result3)

plot(result3$`Actual Values` ~ result3$`Predicted Values`, data = pca_test[-45],
     main = "Actual price by Predicted price")

abline(a = 0, b = 1)

