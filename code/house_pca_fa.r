
# We are going to perform Principal Component Analysis and Factor Analysis on the House data
# The idea is to understand the data more. what are the different Factors involved in it
# The Eigen values and much more. 

house_pca <- house_data  # To create a separate dataframe for PCA and we would be using only Numerical Varaibles for PCA and FA

numeric_vars <- c(colnames(hs_train[,sapply(hs_train,is.numeric)])) 

############################# PCA ###################################

house_pca <- prcomp(hs_train[numeric_vars], scale. = T, center = T)

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

eigen <- get_eigenvalue(house_pca)


# On seeing the plot, it is clear that as the number of variables are more, we have Eigen value of 1, for more than 12 components.
# we anyway are going to take the first 14 components into account

############################# FA ###################################

# Take first 14 Principal Components & do Factor Analysis

house_fact <- factanal(house_log_train[numeric_vars], factors = 14, rotation = "varimax", lower = 0.01)


house_fact

house_fact$loadings # This will give the loadings of the Factors and these loadings explain the nature of the data

# The loadings of FA is present in "Observations" folder where it is clearly seen how this data can be explained by Factor