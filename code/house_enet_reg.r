
source(house_data_prep.r) # Load data preparation of house data

source(house_feature_engg.r) # Load Feature Engineering of house data

glm.cv.net <- cv.glmnet(data.matrix(house_tot_train), house_tot_train$SalePrice , alpha = 0.001)

penalty.net <- glm.cv.net$lambda.min

glm.net <- glmnet(x = as.matrix(house_tot_train[ !names(house_tot_train) %in% "SalePrice"]), y = house_tot_train$SalePrice, alpha = 0.001, lambda = penalty.net)

glm.net

y_pred.net <- as.double(predict(glm.net, as.matrix(house_tot_test)))

y_pred.net