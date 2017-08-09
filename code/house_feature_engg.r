#################################### FEATURE ENGINEERING #####################################

# As we can see lot of square feet columns, lets try to do some Feature engineering on them

area_ftrs <- house_all[ names(house_all) %in% c("LotArea","MasVnrArea","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","X1stFlrSF","X2ndFlrSF",
  "LowQualFinSF","GrLivArea","GarageArea","WoodDeckSF","OpenPorchSF","EnclosedPorch","X3SsnPorch",
  "ScreenPorch","PoolArea")]

# TotalBsmtSF is sum of BsmtFinSF1, BsmtFinSF2 & BsmtUnfSF, hence we remove them and take only the TotalBsmt into consideration

house_all <- house_all[ !names(house_all) %in% c("BsmtFinSF1","BsmtFinSF2","BsmtUnfSF")]

# There are many Area features which are not so important as most of their observations share zero value. 
# Hence add all the less important areas into one as Other_Area_sqft 

Other_Area_sqft <- (house_all$MasVnrArea + house_all$WoodDeckSF + house_all$OpenPorchSF + 
                  house_all$EnclosedPorch + house_all$X3SsnPorch + house_all$ScreenPorch + 
                  house_all$PoolArea + house_all$LowQualFinSF)

house_all$Other_Area_sqft <- Other_Area_sqft

# Remove the original features from the dataframe

house_all <- house_all[!names(house_all) %in% c("MasVnrArea","WoodDeckSF","OpenPorchSF",
                                                "EnclosedPorch","X3SsnPorch","LowQualFinSF",
                                                "ScreenPorch","PoolArea")]


# Utilities feature has only one distinct value in Train and event that is not present in Test hence remove Utilities from the dataset

house_all <- house_all[ !names(house_all) %in% "Utilities"]

table(house_all$PoolQC)

# Ex       No_Pool Other_ratings 
# 4          2909             6 

#There are only 10 observations with Pool quality rating , others are No-Pool , hence remove this

house_all <- house_all[ !names(house_all) %in% "PoolQC"]

# There is a feature YearRemodAdd is renovated year of the building. Create a feature to find the number of years after which the the renovation happened

house_tot$Renovated_Yrs <- (house_tot$YearRemodAdd - house_tot$YearBuilt)

#FALSE  TRUE 
#1358  1561 

# Just number of years renovated does not give much importance so create a categorical variable with values as renovation done or not

house_tot$Is_Renovated <- ifelse(house_tot$Renovated_Yrs == 0,"No", "Yes")

# FInd the number of categorical variables in the dataset

character_vars <- c(colnames(house_all[,sapply(house_all, is.character)]))

character_vars # 44 Character variables

# Find the age of building keeping the current date as 2010 (provided in the problem) and reduce it from the Yearbuilt

house_all$Age_of_bld <- (2010 - house_all$YearBuilt)

# Bin the Age_of_bld as 0-30, 31-60, 61-90, >91

house_all$Age_Bld_bin <- ifelse(house_all$Age_of_bld >= 0 & house_all$Age_of_bld <=30, 0,
                                ifelse(house_all$Age_of_bld >= 31 & house_all$Age_of_bld <= 60, 1,
                                       ifelse(house_all$Age_of_bld >= 61 & house_all$Age_of_bld <= 90, 2, 3)))

# Check if any garage yr built is > 2010 (year in which this data is prepared)

a <- house_all$GarageYrBlt > 2010

which(a) # To find the if the garage was built after this data was prepared (which is impossible) . If yes, it must be incorrect data

# we have one record with Garage_Year_Blt as 2207, we changed it to 2007 benifit of doubt

house_all$GarageYrBlt <- ifelse(house_all$GarageYrBlt == 2207, 2007, house_all$GarageYrBlt)

# Introduce a feature called Garage age with logic same as Yearbuilt 

house_all$Garage_age <- ifelse(is.na(house_all$GarageYrBlt), 0, (2010 - house_all$GarageYrBlt))

# Bin Garage Age 

house_all$Grg_age_bin <- ifelse(house_all$Garage_age >= 0 & house_all$Garage_age <=25, 0,
                                ifelse(house_all$Garage_age >= 26 & house_all$Garage_age <= 50, 1,
                                       ifelse(house_all$Garage_age >= 51 & house_all$Garage_age <= 80, 2, 3)))

# As regression and RF with the actual dataset did not yield expected results, we are binning the character data
# Create one hot encoding via dummyVars
# Use all the character variables and use one hot encoding on them.


dmy1 <- dummyVars(" ~ .", data = house_all)

trsf <- data.frame(predict(dmy1, newdata = house_all))

dim(trsf)

