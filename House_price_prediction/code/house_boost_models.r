
source(house_data_prep.r) # Load data preparation of house data

source(house_feature_engg.r) # Load Feature Engineering of house data

house_tot_train  # Total train data after FE
house_tot_test   # Total test data after FE
SalePrice # Dependant Variable

# Compute XGBoost for the house dataset

dtrain <- xgb.DMatrix(as.matrix(house_tot_train[ !names(house_tot_train) %in% "SalePrice"]), label = house_tot_train$SalePrice)

dtest <- xgb.DMatrix(as.matrix(house_tot_test))

xgb_params <- list(
  booster = 'gbtree',
  objective = 'reg:linear',
  colsample_bytree=1,
  eta=0.005,
  max_depth=4,
  min_child_weight=3,
  alpha=0.3,
  lambda=0.4,
  gamma=0.01, # less overfit
  subsample=0.6,
  seed=5,
  silent=TRUE)

bst <- xgb.train(xgb_params,dtrain, nrounds = 10000)

xgb_pred <- predict(bst, dtest)

