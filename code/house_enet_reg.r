library(recommenderlab)
library(arules)
library(proxy)
library(glmnet)
library(foreach)


glm.cv.net <- cv.glmnet(data.matrix(house_tot_train), house_tot_train$SalePrice , alpha = 0.001)

penalty.net <- glm.cv.net$lambda.min

glm.net <- glmnet(x = as.matrix(house_tot_train[ !names(house_tot_train) %in% "SalePrice"]), y = house_tot_train$SalePrice, alpha = 0.001, lambda = penalty.net)

glm.net

y_pred.net <- as.double(predict(glm.net, as.matrix(house_tot_test)))

y_pred.net