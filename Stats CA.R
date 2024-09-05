install.packages("class")
install.packages("gmodels")
install.packages("palmerpenguins")
install.packages("ggpubr")
install.packages("ggstatsplot")
install.packages("performance")
install.packages("see")
install.packages("ggplot2")
install.packages("knitr")
install.packages("kableExtra")
library(performance)
library(palmerpenguins)
library(ggplot2)
library(tidyverse)
library(car)
library("lattice")
library(dplyr)
library(psych)
library(ggpubr) 
library(ggstatsplot)
library(class)
library(gmodels)
library(ggcorrplot)
library(gridExtra)
library(caret)
library(lmtest)
library(ISLR)
library(knitr)
library(kableExtra)


setwd("/Users/thapelokhantsi/Documents/Postgrad/NCI/Statistics For Data Analytics/CA/Multiple Regression CA")

housing_data <- read.csv("housing.csv", header = TRUE, stringsAsFactors = TRUE)

#EXPLORATORY DATA ANALYSIS

#Display the first few rows of the dataset
head(housing_data)

#Summary statistics of the data
summary(housing_data)
housing_summary <- (summary(housing_data))
# Print the str output as a formatted table
summary_df <- as.data.frame(matrix(unlist(housing_summary), ncol = 7, byrow = TRUE))

kable(data.frame(output = summary_df), "html") %>%
  kable_styling(full_width = FALSE)

#Check for missing values
sapply(housing_data, function(x) sum(is.na(x)))

#Checking for Duplicated Values in the dataset
sum(duplicated(housing_data))

# Data types of columns
housing_str <- capture.output(str(housing_data))
# Print the str output as a formatted table
kable(data.frame(output = housing_str), "html") %>%
  kable_styling(full_width = FALSE)

#Unique values in a categorical variable 
#unique(housing_data$Sale_Price)

#Check the distribution of the target variable ['Sale_Price']
ggplot(housing_data, aes(x = Sale_Price)) +
  geom_histogram(bins=20, fill = "steelblue3", color = "white", boundary = 0) +
  labs(title = "Histogram of SalePrice", x = "SalePrice", y = "Frequency") +
  theme_minimal()

#Check Outliers in the dependent variable(target variable)
boxplot(housing_data$Sale_Price,
        main = "Boxplot of House Prices",
        col = "skyblue3")

#Transformation of target variable to make normally distributed using Log Transformation
# Apply natural log transformation to the dependent variable
# Create a histogram for the logarithm of 'SalePrice'
hist(log(housing_data$Sale_Price), 
     main = "Histogram of log(SalePrice)", 
     xlab = "log(SalePrice)", col = "steelblue3", border = "white")

#CategoricaL features in Data
char_features <- housing_data[, sapply(housing_data, is.factor)] #using factor because chars were converted into factors
char_columns <- names(char_features)
print(char_columns)

#Numerical Features in Data
num_features <- housing_data[, sapply(housing_data, is.numeric)]
num_columns <- names(num_features)
print(num_columns)
#summary(housing_data)
#Compute statistics using the describe function
char_statistics <- describe(num_features)
print(char_statistics)

#Observation of the relationship between predictor variables and target variable (Linearity Assumption) 
#Specify independent variables for the scatter plots
independent_variables <- c("Lot_Frontage", "Lot_Area", "Year_Built", "Total_Bsmt_SF", "First_Flr_SF", "
                           Second_Flr_SF","Full_Bath","Half_Bath","Bedroom_AbvGr","Kitchen_AbvGr","Fireplaces", "Longitude","Latitude")

#Create scatter plots for each independent variable against Sale_Price 
scatter_plots <- lapply(independent_variables, function(variable) {
  ggplot(housing_data, aes_string(x = variable, y = "Sale_Price")) +
    geom_point(color = "steelblue") +
    #geom_smooth(method = "lm", se = FALSE, color = "orange") +
    labs(title = paste("Scatter Plot of", variable, "vs Sale_Price"),
         x = variable, y = "Sale_Price") +
    theme_minimal()
})
# Arrange the scatter plots in a grid
grid.arrange(grobs = scatter_plots, ncol = 3)  

#Outlier Analysis
#Visualize Distribution of Numerical values
boxplots <- lapply(num_columns, function(feature) {
  ggplot(data = housing_data, aes(x = 1, y = .data[[feature]])) +
    geom_boxplot() +
    labs(title = feature) +
    theme_minimal()
})
#Arrange the boxplots in a four columns
grid.arrange(grobs = boxplots, ncol = 4)

#Visualize Distribution of Numerical values using histograms to also check for Linearity Assumption
histograms <- lapply(num_columns, function(feature) {
  ggplot(data = housing_data, aes(x =.data[[feature]])) +
    geom_histogram(bins = 5, fill = "steelblue", color = "white", alpha = 0.9) +
    labs(title = feature, x = feature, y = "Frequency") +
    theme_minimal()
})
#Arrange the histograms in a three columns
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

#Using ggcorrplot to get correlation between features
#Correlation matrix: (0<=|c|<0.3: weak), (0.3<=|c|<0.7: moderate) & (0.7<=|c|<1: strong)
pairs(num_features)
ggcorrplot(cor(num_features),hc.order = TRUE, lab = TRUE,colors = c("#6D9EC1", "white", "#E46726"))


#One-hot encode categorical variables
str(char_columns)
encoded_features <- data.frame(model.matrix(~ . -1, data = housing_data[char_columns]))
str(encoded_features)
#Add the encoded categorical variables to the housing dataframe
housing_data <- cbind(housing_data, encoded_features)

#Drop categorical variables after one-hot encoding
housing_data <- subset(housing_data, select = -c(Bldg_Type, House_Style, Overall_Cond, Exter_Cond))

#Check if there are any missing values after Encoding
sapply(housing_data, function(x) sum(is.na(x)))

summary(housing_data)

#In the Outlier Analysis Boxplot, we observed some outliers and therefore we need to take care of them
#Outlier Detection Using Interquatile Range(IQR) method
#Calculating Q1, Q3, and IQR for each numeric variable
str(num_columns)
summary(num_columns)


numeric_features <- c("Lot_Frontage", "Lot_Area", "Year_Built", "Total_Bsmt_SF",
                      "First_Flr_SF", "Second_Flr_SF", "Full_Bath", "Half_Bath","Bedroom_AbvGr",
                      "Kitchen_AbvGr", "Fireplaces", "Longitude", "Latitude","Bldg_TypeDuplex",
                      "Bldg_TypeOneFam", "Bldg_TypeTwnhs", "Bldg_TypeTwnhsE", "Bldg_TypeTwoFmCon",
                      "House_StyleOne_and_Half_Unf", "House_StyleOne_Story","House_StyleSFoyer", "House_StyleSLvl",
                      "House_StyleTwo_and_Half_Fin","House_StyleTwo_and_Half_Unf", "House_StyleTwo_Story",
                      "Overall_CondAverage", "Overall_CondBelow_Average","Overall_CondExcellent", "Overall_CondFair",
                      "Overall_CondGood", "Overall_CondPoor", "Overall_CondVery_Good","Overall_CondVery_Poor",
                      "Exter_CondFair", "Exter_CondGood","Exter_CondPoor", "Exter_CondTypical")

summary(numeric_features)

# Extract numeric columns from the numeric_feature vector of housing_data
numeric_columns <- housing_data[, numeric_features]

#Outlier Detection Using Interquatile Range(IQR) method
#Calculate Q1, Q3, and IQR for each numeric variable
Q1 <- apply(numeric_columns, 2, quantile, 0.25)
Q3 <- apply(numeric_columns, 2, quantile, 0.75)
IQR_values <- apply(numeric_columns, 2, IQR)

# Calculate lower and upper bounds for each variable
lower_bounds <- Q1 - 1.5 * IQR_values
upper_bounds <- Q3 + 1.5 * IQR_values

# Identify outliers for each variable
outliers <- lapply(names(numeric_columns), function(var) {
  var_outliers <- housing_data[housing_data[[var]] < lower_bounds[var] | housing_data[[var]] > upper_bounds[var], var, drop = FALSE]
  data.frame(Variable = rep(var, nrow(var_outliers)), Outlier = var_outliers[[var]])
})

# Combine the list of data frames into a single data frame
outliers_list <- do.call(rbind, outliers)

# boxplots <- lapply(names(numeric_columns), function(feature) {
#   ggplot(data = housing_data, aes(x = 1, y = .data[[feature]])) +
#     geom_boxplot() +
#     labs(title = feature) +
#     theme_minimal()
# })
# #Arrange the boxplots in a four columns
# grid.arrange(grobs = boxplots, ncol = 6)

# Impute outliers for all numeric variables with median

# Identify and remove outliers
for (variable in numeric_features) {
  Q1_var <- quantile(housing_data[[variable]], 0.25)
  Q3_var <- quantile(housing_data[[variable]], 0.75)
  IQR_value_var <- Q3_var - Q1_var
  
  # Calculate lower and upper bounds
  lower_bound_var <- Q1_var - 1.5 * IQR_value_var
  upper_bound_var <- Q3_var + 1.5 * IQR_value_var
  
  # Remove outliers for each variable
  housing_data <- housing_data[!(housing_data[[variable]] < lower_bound_var | housing_data[[variable]] > upper_bound_var), ]

}
#Check variables that have near to zero variance and drop them
nearZeroVar(housing_data, saveMetrics = TRUE) 

#dropping column with near to zero variance
housing_data <- housing_data[, !colnames(housing_data) %in% c("Kitchen_AbvGr","Bldg_Type", "House_Style", "Overall_Cond", "Exter_Cond",
                                                               "Bldg_TypeDuplex", "Bldg_TypeTwnhs", "Bldg_TypeTwnhsE",
                                                              "Bldg_TypeTwoFmCon", "Bldg_TypeOneFam", "House_StyleOne_and_Half_Unf",
                                                              "House_StyleSFoyer", "House_StyleSLvl", "House_StyleTwo_and_Half_Fin",
                                                              "House_StyleTwo_and_Half_Unf", "Overall_CondBelow_Average", "Overall_CondExcellent",
                                                              "Overall_CondFair", "Overall_CondGood", "Overall_CondPoor", "Overall_CondVery_Good",
                                                              "Overall_CondVery_Poor", "Exter_CondFair", "Exter_CondGood", "Exter_CondPoor", "Exter_CondTypical")]

# Data Transformation
# Standardize numeric features for consistency
# Normalize the data using min-max normalization

#Numeric variables after removing Outliers
num_vars <-c("Lot_Frontage", "Lot_Area", "Year_Built", "Total_Bsmt_SF",
             "First_Flr_SF", "Second_Flr_SF", "Full_Bath", "Half_Bath","Bedroom_AbvGr",
             "Fireplaces", "Longitude", "Latitude","House_StyleOne_Story",
             "House_StyleTwo_Story", "Overall_CondAverage")
min_max_transform <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Apply min-max scaling to relevant columns
housing_data[num_vars] <- lapply(housing_data[num_vars], min_max_transform)


summary(housing_data)
print(housing_data) #View the resulting data frame
summary(housing_data)
str(housing_data)


#Outlier Analysis
#Visualize Distribution of Numerical values after transformation
numeric_columns_modified <- housing_data[, names(housing_data)]
boxplots <- lapply(names(numeric_columns_modified), function(feature) {
  ggplot(data = housing_data, aes(x = 1, y = .data[[feature]])) +
    geom_boxplot() +
    labs(title = feature) +
    theme_minimal()
})
#Arrange the boxplots in a four columns
grid.arrange(grobs = boxplots, ncol = 4)

#Feature Selection via correlation analysis
# Extract numeric columns
feature_selction <- housing_data[, sapply(housing_data, is.numeric)]

# Create a correlation matrix
cor_matrix <- cor(numeric_columns_modified)

# Create a correlation plot using ggcorrplot
ggcorrplot(cor_matrix, hc.order = TRUE, lab = TRUE, colors = c("#6D9EC1", "white", "#E46726"))

#Checking Near Zero Variance features that violate the assumption of Multiple Linear Regression
nearZeroVar(housing_data, saveMetrics = TRUE)  #no near zero variance variables

# Set the random seed
set.seed(23131535)

# Generate indices for train and test sets,
# 80% of the data is used for training and 20% for testing
train_indices <- sample(seq_len(nrow(housing_data)), size = 0.8 * nrow(housing_data))
#Model Building 
# Create the train and test sets
train_data <- housing_data[train_indices, ]
test_data <- housing_data[-train_indices, ]

#Review the Multiple Regression Model
# Fit the model using lm() function
model1 <- lm(Sale_Price ~ Lot_Frontage + Lot_Area + Year_Built + Total_Bsmt_SF +
               First_Flr_SF + Second_Flr_SF + Full_Bath + Half_Bath + Bedroom_AbvGr +
               Fireplaces + Longitude + Latitude + House_StyleOne_Story +
               House_StyleTwo_Story + Overall_CondAverage,
             data = train_data)
summary(model1)
check_model(model1) 
#Posterior Check shows that model-predicted line does not resemble observed data
#Model has non-constant variance error which indicates violation of homoscedasticity assumption

# Check for multicollinearity using VIF (Variance Inflation Factor)
vif(model1) #House_Style,House_StyleOne_Story, House_StyleOne_Story,Total_Bsmt_SF, First_Flr_SF, Second_Flr_SF
#have high collinearity as observed in Collinearity plot from model check(i.e vif >10)
ncvTest(model1)
#Check normality of residuals
check_normality(model1) #Non-normality of residuals detected (p < .001).
#Check auto correlation
check_autocorrelation(model1) #Autocorrelated residuals detected (p = 0.024).

# An R2 value close to 1 indicates that the model explains a large portion of the variance in the outcome variable. 
# In this first model the R2 = 0.8594 which is good and but we still have some predictors that are not statistically(i.e p-value > 0.05)
# The adjusted R2 = 0.8402, meaning that “85.94%” of the variance in the measure of the response (SalePrice) can be 
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

durbinWatsonTest(model1) #Model meets assumption #1.85141

check_heteroscedasticity(model1) #Heteroscedasticity (non-constant error variance) detected (p < .001).
hist(residuals(model1)) 

# Create a data frame with residuals and predictor variable
residuals_df <- data.frame(
  Residuals = residuals(model1),
  Predictor = model.frame(model1)$Sale_Price
  #fitted = fitted(model1)
)

# Plot Residuals vs Predictor
ggplot(residuals_df, aes(x = Predictor, y = Residuals)) +
  geom_point(color="steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Predictor Variable", y = "Residuals", title = "Residuals vs Predictor") +
  theme_minimal()


#Model 2 addressing issues in the first model for Gauss-markov Assumption
missing_values <- sum(is.na(train_data$Sale_Price))
#Log transform on the predictor variable(sales price)
model2 <- lm(log(Sale_Price) ~ Lot_Frontage + Lot_Area + Year_Built + Total_Bsmt_SF +
               First_Flr_SF + Second_Flr_SF + Full_Bath + Half_Bath + Bedroom_AbvGr +
               Fireplaces + Longitude + Latitude + House_StyleOne_Story +
               House_StyleTwo_Story + Overall_CondAverage,
             data = train_data)
summary(model2)
check_model(model2) 
check_normality(model2)
#Non-normality of residuals detected (p < .001), which shows significant deviation from normal distribution
#Outliers might have to be handled and some transformations done in the next model iteration
#Check for Homoscedasticity
ncvTest(model2) #Model violates assumption
#Multicollinearity Test:
vif(model2) 
durbinWatsonTest(model2) #Model meets assumption #1.894991
check_heteroscedasticity(model2) #Heteroscedasticity (non-constant error variance) detected (p < .001).
#Residual Analysis with Standard diagnosis plots:
par(mfrow=c(2,2))
plot(model2, pch=23 ,bg='orange',cex=2)

#Model 3
model3 <- lm(log(Sale_Price) ~ Lot_Frontage + Lot_Area + Year_Built +
               Full_Bath + Half_Bath + Bedroom_AbvGr +
               Fireplaces + Longitude + Latitude + Overall_CondAverage,
             data = train_data)
summary(model3)
check_model(model3) 
#Check for Homoscedasticity
ncvTest(model3) 
#Multicollinearity Test:
vif(model3) # the is Low Correlation therefore the model does not have Multicollinearity issue
#Testing for autocorrelation
durbinWatsonTest(model3) #Model meets assumption #1.608625
check_heteroscedasticity(model3) #OK: Error variance appears to be homoscedastic (p = 0.302).
check_collinearity(model3)  #low correlation

#Residuals Check
par(mfrow=c(2,2))
plot(model3, pch=23 ,bg='orange',cex=2)

#Model 4 to take care of variables that are not statistically significant to the model.
model4 <- lm(log(Sale_Price) ~ Lot_Frontage + Lot_Area + Year_Built +
               Full_Bath + Half_Bath + Bedroom_AbvGr +
               Fireplaces ,
             data = train_data)
summary(model4)
check_model(model4) 
#Check for Homoscedasticity
ncvTest(model4) 
#Multicollinearity Test:
vif(model4) # there is Low Correlation therefore the model does not have Multicollinearity issue
#Testing for autocorrelation
durbinWatsonTest(model4) #Model meets assumption #2.018998
check_heteroscedasticity(model4) #OK: Error variance appears to be homoscedastic (p = 0.391).
check_collinearity(model4) 
#Residuals Check
par(mfrow=c(2,2))
plot(model4, pch=23 ,bg='orange',cex=2)
bptest(model4)

# Plot quantile-quantile plot with confidence bands
qqPlot(model4, main = "Quantile-Quantile Plot with Confidence Bands")


#Evaluation
rsquared <- summary(model4)$r.squared 
print(rsquared) #0.7637516
adj.rsquared <- summary(model4)$adj.r.squared
print(adj.rsquared) #0.7617711

#Residual Standard error 
#Residual Standard Error (RSE): RSE is an estimate of the standard deviation of the model's residuals. 
#It measures the average deviation of observed values from the predicted values.
rse <- summary(model4)$sigma
print(rse)

#Cross-validation
#Define your control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 5)  

#Defining the  model
cvmodel_results <- train(Sale_Price ~ ., data = train_data, method = "lm", trControl = ctrl)

#Print the results
print(cvmodel_results)

#Evaluate the model
predictions <- predict(model4, newdata = test_data)

# Calculate RMSE
rmse_value <- RMSE(predictions, test_data$Sale_Price)
print(rmse_value)
log_sale_price <- log( housing_data$Sale_Price)
#Checking if the model is fitted
if (length(coefficients(model4)) > 0) {
  print("The model is fitted.")
} else {
  print("The model is not fitted.")
}

#Assumption of Normality
hist(model4$residuals,
     col = "skyblue4",
     main = "Histogram of Residuals",
     xlab = "Residuals")
nortest::ad.test(model4$residuals)

#homoscedasticity assumption
plot(model4$fitted.values, model4$residuals,
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red")
bptest(model4)


qqnorm(residuals(model4))
qqline(residuals(model4))




