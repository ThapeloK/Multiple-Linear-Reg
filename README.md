# Multiple Linear Reg
 A project Investigating Factors Influencing Housing Prices Using Multiple Linear Regression

# Housing Prices Prediction using Multiple Linear Regression (MLR)

## Project Overview

This project investigates the **factors influencing housing prices** using **Multiple Linear Regression (MLR)**. The goal is to identify the key variables that affect housing prices and assess their impact. The analysis adheres to the **Gauss-Markov assumptions** and evaluates different models for better accuracy.

### Main Objectives:
1. **Predict housing sale prices** using multiple independent variables.
2. **Evaluate the influence** of various attributes (e.g., house features, environment, location) on housing prices.
3. **Ensure model robustness** by validating assumptions and performing cross-validation.

---

## Dataset Information

- **Instances**: 2,413
- **Attributes**: 18
  - **Numerical Variables**: Lot Area, Total Basement SF, First Floor SF, etc.
  - **Categorical Variables**: House Style, Building Type, Exterior Condition, etc.
  - **Target Variable**: Sale Price

#### Key Characteristics:
- **Skewness**: Most numerical variables are skewed, requiring data transformation.
- **Outliers**: Detected and handled using the **Interquartile Range (IQR)** method and **Min-Max scaling**.
  
---

## Project Workflow

### 1. Exploratory Data Analysis (EDA)
- **Descriptive Statistics**: Variables were explored using mean, skewness, kurtosis, and standard deviation.
- **Data Visualization**: Histograms, scatter plots, and boxplots were used to understand distributions and relationships.
- **Data Cleaning**: Handled skewness through log transformation; no missing values were found.

### 2. Data Preparation
- **Log Transformation**: Applied to Sale Price due to positive skewness.
- **One-Hot Encoding**: Used for categorical variables to avoid interpretation issues during regression.
- **Min-Max Scaling**: Applied to numerical variables for normalization and better performance.

### 3. Modeling and Diagnostics

Four models were developed and evaluated:

- **Model 1**: Initial model; faced multicollinearity and heteroscedasticity issues.
- **Model 2**: Applied log transformation to the response variable. Heteroscedasticity persisted.
- **Model 3**: Removed variables with high multicollinearity; performance improved.
- **Model 4** (Final Model): Refined by removing non-significant variables, leading to the best results.

### 4. Model Evaluation

- **Final R² Score**: **0.7638**, meaning the model explains **76.38%** of the variance in sale prices.
- **Cross-validation Metrics**:
  - **Root Mean Squared Error (RMSE)**: 25,041.71
  - **Mean Absolute Error (MAE)**: 18,631.9
  - **Cross-validated R²**: 85.56%

The final model satisfied all **Gauss-Markov assumptions**, including linearity, no multicollinearity, homoscedasticity, and no autocorrelation.

---

## Requirements

- **Programming Language**: Python 3.8+
- **Libraries**:
  - `pandas`
  - `numpy`
  - `scikit-learn`
  - `matplotlib`
  - `seaborn`

Install the required libraries using the following command:

```bash
pip install pandas numpy scikit-learn matplotlib seaborn
