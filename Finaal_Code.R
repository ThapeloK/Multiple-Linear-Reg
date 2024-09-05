install.packages("class")
install.packages("gmodels")
install.packages("palmerpenguins")
install.packages("ggpubr")
install.packages("ggstatsplot")
install.packages("performance")
install.packages("see")
install.packages("ggplot2")
library(performance)
library(palmerpenguins)
library(ggplot2)
library(tidyverse)
library(car)
library("lattice")
library(dplyr)
library(ggpubr) 
library(ggstatsplot)
library(class)
library(gmodels)
library(ggcorrplot)
library(gridExtra)
library(caret)
library(lmtest)
library(ISLR)

housing_data <- read.csv("housing.csv", header = TRUE, stringsAsFactors = TRUE)


#EXPLORATORY DATA ANALYSIS

#Display the first few rows of the dataset
head(housing_data)

# Summary statistics of the data
summary(housing_data)

#Check for missing values
sapply(housing_data, function(x) sum(is.na(x)))

#Checking for Duplicated Values in the dataset
sum(duplicated(housing_data))

# Data types of columns
str(housing_data)

# # Unique values in a categorical variable 
# unique(housing_data$Sale_Price)

# Check the distribution of the target variable ['Sale_Price']
ggplot(housing_data, aes(x = Sale_Price)) +
  geom_histogram(bins=20, fill = "steelblue3", color = "white", boundary = 0) +
  labs(title = "Histogram of SalePrice", x = "SalePrice", y = "Frequency") +
  theme_minimal()

##Check Outliers in the dependent variable(target variable)
boxplot(housing_data$Sale_Price,
        main = "Boxplot of House Prices",
        col = "skyblue3")

#Transformation of target variable to make normally distributed using Log Transformation
# Create a histogram for the logarithm of 'SalePrice'
hist(log(housing_data$Sale_Price), 
     main = "Histogram of log(SalePrice)", 
     xlab = "log(SalePrice)", col = "steelblue3", border = "white")

#CategoricaL features in Data
char_features <- housing_data[, sapply(housing_data, is.factor)] #using factor because chars were converted into factors
char_columns <- names(char_features)
char_columns

#Numerical Features in Data
num_features <- housing_data[, sapply(housing_data, is.numeric)]
num_columns <- names(num_features)
num_columns

#Outlier Analysis
#Visualize Distribution of Numerical values
boxplots <- lapply(num_columns, function(feature) {
  ggplot(data = housing_data, aes(x = 1, y = .data[[feature]])) +
    geom_boxplot() +
    labs(title = feature) +
    theme_minimal()
})
# Arrange the boxplots in a four columns
grid.arrange(grobs = boxplots, ncol = 4)

# Visualize Distribution of Numerical values using histograms to also check for Linearity Assumption
histograms <- lapply(num_columns, function(feature) {
  ggplot(data = housing_data, aes(x =.data[[feature]])) +
    geom_histogram(bins = 5, fill = "steelblue", color = "white", alpha = 0.9) +
    labs(title = feature, x = feature, y = "Frequency") +
    theme_minimal()
})
# Arrange the histograms in a three columns
grid.arrange(grobs = histograms, ncol = 3)

#Visualize Distribution of Categorical values
par(mfrow = c(2, 2))  
bldg_type <- table(housing_data$Bldg_Type)
barplot(bldg_type, xlab = "Dwelling Type", ylab = "Count", main = "Dwelling Type",col = "steelblue")

house_style <- table(housing_data$House_Style)
barplot(house_style, xlab = "Dwelling Style", ylab = "Count", main = "Dwelling Style",col = "steelblue")

overall_cond <- table(housing_data$Overall_Cond)
barplot(overall_cond, xlab = "Overall Condition of House", ylab = "Count", main = "Overall Condition of House",col = "steelblue")

exter_cond <- table(housing_data$Exter_Cond)
barplot(exter_cond, xlab = "Condition of Material on Exterior of House", ylab = "Count", main = "Condition of Material on Exterior of House",col = "steelblue")
par(mfrow = c(1, 1))

#Checking Near Zero Variance features that violate the assumption of Multiple Linear Regression
nearZeroVar(housing_data, saveMetrics = TRUE) #Kitchen_AbvGr has near to zero variance

# Using ggcorrplot to get correlation between features
#Correlation matrix: (0<=|c|<0.3: weak), (0.3<=|c|<0.7: moderate) & (0.7<=|c|<1: strong)
pairs(num_features)
ggcorrplot(cor(num_features),hc.order = TRUE, lab = TRUE,colors = c("#6D9EC1", "white", "#E46726"))


#Review the Multiple Regression Model
# Fit the model using lm() function
model1 <- lm(Sale_Price ~ Lot_Frontage + Lot_Area + Bldg_Type + House_Style +
                   Overall_Cond + Year_Built + Exter_Cond + Total_Bsmt_SF +
                   First_Flr_SF + Second_Flr_SF + Full_Bath + Half_Bath +
                   Bedroom_AbvGr + Kitchen_AbvGr + Fireplaces + Longitude + Latitude,
                 data = housing_data)
summary(model1)
check_model(model1) 
#Posterior Check shows that model-predicted line does not resemble observed data
#Model has non-constant variance error which indicates violation of homoscedasticity assumption

# Check for multicollinearity using VIF (Variance Inflation Factor)
vif(model1) #House_Style has high collinearity as observed in Collinearity plot from model check(i.e vif >10)
#Bldg_Type and Second_Flr_SF also need further investigation as they have moderate vif

# plot results
x <- check_collinearity(model1)
plot(x)
model_back <- step(model1, direction = "backward")
summary(model_back)
#Check normality of residuals
check_normality(model_back) #Non-normality of residuals detected (p < .001).

# An R2 value close to 1 indicates that the model explains a large portion of the variance in the outcome variable. 
# In this first model the R2 = 0.8425 which is good and but we still have some predictors that are not statistically(i.e p-value > 0.05)
# The adjusted R2 = 0.8402, meaning that “84.02%” of the variance in the measure of the response (SalePrice) can be 
# predicted by the selected predictor variables in the model but the model violates Gauss-Markov Assumptions and we need to handle predictors that influence this

#Residual Analysis with Standard diagnosis plots:
#Residuals vs Fitted Values and Scale-Location Plot checks for Homoscedasticity
#Normal Q-Q Plot checks for Normality of Residuals
#Cook's Distance Plot checks for Influential Points
par(mfrow=c(2,2))
plot(model1, pch=23 ,bg='orange',cex=2)

# Breusch-Pagan test for checking Homoscedasticity Assumption
bptest(model1) # Homoscedasticity Assumption is violated since p-value < 0.005 = (p-value < 2.2e-16)

#Plot Outliers in Model1
model1_outliers <- check_outliers(model1) #No outliers detected. Based on the following method and threshold: cook (1)

check_heteroscedasticity(model1) #Heteroscedasticity (non-constant error variance) detected (p < .001).

# Create a data frame with residuals and predictor variable
residuals_df <- data.frame(
  Residuals = residuals(model1),
  Predictor = housing_data$Sale_Price  # Replace with the predictor variable of interest
)

# Plot Residuals vs Predictor
ggplot(residuals_df, aes(x = Predictor, y = Residuals)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Predictor Variable", y = "Residuals", title = "Residuals vs Predictor") +
  theme_minimal()

#Model 2 addressing issues in the first model for Gauss-markov Assumption
#removed House_Style due to high vif and Kitchen_AbvGr having near to zero variance which violate MLR assumption
model2 <- lm(log(Sale_Price) ~ Lot_Frontage + Lot_Area + Bldg_Type +
               Overall_Cond + Year_Built + Exter_Cond + Total_Bsmt_SF +
               First_Flr_SF + Second_Flr_SF + Full_Bath + Half_Bath +
               Bedroom_AbvGr + Fireplaces + Longitude + Latitude,
             data = housing_data)
summary(model2)
check_model(model2) 
#Non-normality of residuals detected (p < .001), which shows significant deviation from normal distribution
#Outliers might have to be handled and some transformations done in the next model iteration

#Check for Homoscedasticity
ncvTest(model2) #Model violates assumption
#Multicollinearity Test:
vif(model2) # the is Low Correlation therefore the model does not have Multicollinearity issue
#Testing for autocorrelation
durbinWatsonTest(model2) #Model meets assumption #1.590529
check_heteroscedasticity(model2) #OK: Error variance appears to be homoscedastic (p = 0.070)

# Model 3 addressing the outliers and violation of normality 

# One-hot encode categorical variables
housing_data <- cbind(housing_data,model.matrix(~. - 1, data = housing_data))

#Data Transformation
#Standardize numeric features for consistency
numeric_features <- c("Lot_Frontage","Lot_Area", "Year_Built", "Total_Bsmt_SF",
                      "First_Flr_SF", "Second_Flr_SF", "Full_Bath", "Half_Bath",
                      "Fireplaces", "Longitude", "Latitude", "House_StyleOne_Story",
                      "House_StyleTwo_Story", "Overall_CondAverage")

summary(housing_data)

#Outlier Detection Using Interquatile Range(IQR) method
# Calculate Q1, Q3, and IQR for each numeric variable
Q1 <- apply(num_features, 2, quantile, 0.25)
Q3 <- apply(num_features, 2, quantile, 0.75)
IQR_values <- apply(num_features, 2, IQR)

# Calculate lower and upper bounds for each variable
lower_bounds <- Q1 - 1.5 * IQR_values
upper_bounds <- Q3 + 1.5 * IQR_values

# Identify outliers for each variable
outliers <- lapply(names(num_features), function(var) {
  var_outliers <- num_features[, var][num_features[, var] < lower_bounds[var] | num_features[, var] > upper_bounds[var]]
  data.frame(Variable = rep(var, length(var_outliers)), Outlier = var_outliers)
})
# Combine the list of data frames into a single data frame
outliers_list <- do.call(rbind, outliers)
#I want to retain the structure of the overall structure of the data so i decide to use
#Imputation instead of using other methods of handling outliers such as transformation of predictors or removing outliers

# Impute outliers for all numeric variables with median
for (variable in names(num_features)) {
  Q1 <- quantile(num_features[[variable]], 0.25)
  Q3 <- quantile(num_features[[variable]], 0.75)
  IQR_value <- IQR(num_features[[variable]])
  
  # Calculate lower and upper bounds
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  # Impute outliers with median
  housing_data[[variable]][housing_data[[variable]] < lower_bound] <- median(housing_data[[variable]], na.rm = TRUE)
  housing_data[[variable]][housing_data[[variable]] > upper_bound] <- median(housing_data[[variable]], na.rm = TRUE)
}

#Check data after imputation
summary(housing_data)
str(housing_data)
#Drop columns with no range as it has no importance
housing_data <- housing_data[, !colnames(housing_data) %in% c("Bldg_Type", "House_Style", "Overall_Cond", "Exter_Cond",
                                                              "Bedroom_AbvGr","Kitchen_AbvGr", "Bldg_TypeDuplex",
                                                              "Bldg_TypeTwnhs", "Bldg_TypeTwnhsE",
                                                              "Bldg_TypeTwoFmCon", "Bldg_TypeOneFam", "House_StyleOne_and_Half_Unf",
                                                              "House_StyleSFoyer", "House_StyleSLvl", "House_StyleTwo_and_Half_Fin",
                                                              "House_StyleTwo_and_Half_Unf", "Overall_CondBelow_Average", "Overall_CondExcellent",
                                                              "Overall_CondFair", "Overall_CondGood", "Overall_CondPoor", "Overall_CondVery_Good",
                                                              "Overall_CondVery_Poor", "Exter_CondFair", "Exter_CondGood", "Exter_CondPoor", "Exter_CondTypical")]

#Data Transformation
#Standardize numeric features for consistency
numeric_features <- c("Lot_Frontage","Lot_Area", "Year_Built", "Total_Bsmt_SF",
                      "First_Flr_SF", "Second_Flr_SF", "Full_Bath", "Half_Bath",
                      "Fireplaces", "Longitude", "Latitude", "House_StyleOne_Story",
                      "House_StyleTwo_Story", "Overall_CondAverage")

#Normalize the data using min-max normalization
min_max_transform <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

#Apply min-max scaling function to relevant columns
housing_data[numeric_features] <- lapply(housing_data[numeric_features], min_max_transform)

summary(housing_data)

# View the resulting data frame
print(housing_data)
summary(housing_data)
str(housing_data)


#Outlier Analysis
#Visualize Distribution of Numerical values after Imputation
numeric_colums <- names(num_features)
boxplots_imputed <- lapply(numeric_colums, function(feature) {
  ggplot(data = housing_data, aes(x = 1, y = .data[[feature]])) +
    geom_boxplot() +
    labs(title = feature) +
    theme_minimal()
})
# Arrange the boxplots in a four columns
grid.arrange(grobs = boxplots_imputed, ncol = 4)

# Detect outliers using IQR

model3 <- lm(log(Sale_Price) ~ Lot_Frontage + Lot_Area + Bldg_Type +
               Overall_Cond + Year_Built + Exter_Cond + Total_Bsmt_SF +
               First_Flr_SF + Second_Flr_SF + Full_Bath + Half_Bath +
               Bedroom_AbvGr + Fireplaces + Longitude + Latitude,
             data = housing_data)
summary(model3)
check_model(model3) 
#Check for Homoscedasticity
ncvTest(model3) #Model violates assumption
#Multicollinearity Test:
vif(model3) # the is Low Correlation therefore the model does not have Multicollinearity issue
#Testing for autocorrelation
durbinWatsonTest(model3) #Model meets assumption #1.608625
check_heteroscedasticity(model3) #Heteroscedasticity (non-constant error variance) detected (p < .001).
check_collinearity(model3)

model_back <- step(model2, direction = "both")
summary(model_back)

# Set the random seed
set.seed(23131535)

# Generate indices for train and test sets,
# 80% of the data is used for training and 20% for testing
train_indices <- sample(seq_len(nrow(housing_data)), size = 0.8 * nrow(housing_data))

# Create the train and test sets
train_data <- housing_data[train_indices, ]
test_data <- housing_data[-train_indices, ]

library(nlme)

# Fit the model using lm() function
test_model <- gls(log(Sale_Price) ~ Lot_Frontage + Lot_Area + Bldg_Type + House_Style +
              Overall_Cond + Year_Built + Exter_Cond + Total_Bsmt_SF +
              First_Flr_SF + Second_Flr_SF + Full_Bath + Half_Bath +
              Bedroom_AbvGr + Kitchen_AbvGr + Fireplaces + Longitude + Latitude,
            data = housing_data, weights = varIdent(form = ~1 | Overall_Cond))

summary(test_model)
check_model(test_model)

#Heteroscedasticity
check_heteroscedasticity(test_model)
# plot results
if (require("see")) {
  x <- check_heteroscedasticity(m)
  plot(x)
}

# Check for multicollinearity using VIF (Variance Inflation Factor)
vif(test_model)




# Detect outliers using IQR
sample_outliers <- detect_outliers_iqr(sample)

# Print the outliers
cat("Outliers from IQR method: ", sample_outliers, "\n")

# Winsorize at 95th percentile
for (col in names(outliers_list)) {
  num_features[[col]][num_features[[col]] > quantile(num_features[[col]], 0.95)] <- quantile(num_features[[col]], 0.95)
}

# Apply log transformation to response variable
housing_data$log_Sale_Price <- log1p(housing_data$Sale_Price)

# Apply log transformation to predictor variables
housing_data$log_Lot_Frontage <- log1p(housing_data$Lot_Frontage)
housing_data$log_Lot_Area <- log1p(housing_data$Lot_Area)
housing_data$log_Year_Built <- log1p(housing_data$Year_Built)
housing_data$log_Total_Bsmt_SF <- log1p(housing_data$Total_Bsmt_SF)
housing_data$log_First_Flr_SF <- log1p(housing_data$First_Flr_SF)
housing_data$log_Second_Flr_SF <- log1p(housing_data$Second_Flr_SF)
housing_data$log_Full_Bath <- log1p(housing_data$Full_Bath)
housing_data$log_Half_Bath <- log1p(housing_data$Half_Bath)
housing_data$log_Bedroom_AbvGr <- log1p(housing_data$Bedroom_AbvGr)
housing_data$log_Fireplaces <- log1p(housing_data$Fireplaces)
housing_data$log_Latitude<- log1p(housing_data$Latitude)
# Add log transformations for other predictor variables...

# Fit linear regression model with log-transformed variables
model1_log <- lm(log_Sale_Price ~ log_Lot_Frontage + log_Lot_Area +
                   log_Year_Built +  log_Total_Bsmt_SF +
                   log_First_Flr_SF + log_Second_Flr_SF + log_Full_Bath + log_Half_Bath +
                   log_Bedroom_AbvGr + log_Fireplaces + log_Latitude,
                 data = housing_data)

# Summary of the log-transformed model
summary(test_model)
vif(test_model)

par(mfrow=c(2,2))
plot(test_model, pch=23 ,bg='orange',cex=2)
#Heteroscedasticity
check_heteroscedasticity(test_model)
#Check normality of residuals
check_normality(test_model)

#Plot Outliers in Model1
model1_outliers <- check_outliers(model1) #No outliers detected. Based on the following method and threshold: cook (1)

bc <- boxcox(model1)
str(bc)

(bc.power <- bc$x[which.max(bc$y)])


