# Load necessary libraries
library(dplyr)  # For data manipulation
library(caret)  # For machine learning model training and evaluation

# Step 1: Read and preprocess data1
# Read the first dataset
data1 <- read.csv("C:/Users/Admin/Downloads/oulad-students.csv")

# Subset data1 to include specific columns
data1 <- data1[, c("code_module", "code_presentation", "id_student", "gender", "region",
                   "highest_education", "imd_band", "age_band", "num_of_prev_attempts",
                   "studied_credits", "disability", "final_result",
                   "module_presentation_length", "date_registration", "date_unregistration")]

# Data preprocessing for data1
# Convert categorical variables to factors
factor_columns1 <- c("code_module", "code_presentation", "gender", "region",
                     "highest_education", "imd_band", "age_band", "num_of_prev_attempts",
                     "disability", "final_result")

data1[factor_columns1] <- lapply(data1[factor_columns1], as.factor)

# Drop rows with missing values
data1 <- na.omit(data1)

# Split the data into training and testing sets for data1 (80% training, 20% testing)
set.seed(123)  # For reproducibility
train_index1 <- createDataPartition(data1$final_result, p = 0.8, list = FALSE)

# Train and test datasets for data1
train_data1 <- data1[train_index1, ]
test_data1 <- data1[-train_index1, ]

# Step 2: Train classification and regression models for data1
# Train the classification model for data1 (logistic regression)
classification_model1 <- glm(final_result ~ ., data = train_data1, family = "binomial")

# Train the regression model for data1 (linear regression)
regression_model1 <- lm(studied_credits ~ ., data = train_data1)

# Step 3: Make predictions on the test data for data1
# Make predictions on the test data for classification for data1 (probabilities)
classification_predictions1 <- predict(classification_model1, newdata = test_data1, type = "response")

# Convert predicted probabilities to class labels for classification for data1
predicted_classes1 <- ifelse(classification_predictions1 > 0.5, "Pass", "Fail")  # Adjust the threshold as needed

# Make sure predicted_classes1 is a factor with the same levels as test_data1$final_result
predicted_classes1 <- factor(predicted_classes1, levels = levels(test_data1$final_result))

# Make predictions on the test data for regression for data1
regression_predictions1 <- predict(regression_model1, newdata = test_data1)

# Step 4: Evaluate the classification and regression models for data1
# Evaluate the classification model for data1
classification_confusion_matrix1 <- confusionMatrix(data = predicted_classes1, reference = test_data1$final_result)

# Calculate regression model metrics for data1
MAE1 <- mean(abs(regression_predictions1 - test_data1$studied_credits))
MSE1 <- mean((regression_predictions1 - test_data1$studied_credits)^2)
RMSE1 <- sqrt(mean((regression_predictions1 - test_data1$studied_credits)^2))

# Print classification model evaluation for data1
print("Classification Model Evaluation for data1:")
print(classification_confusion_matrix1)

# Print regression model metrics for data1
cat("MAE for regression model (data1):", MAE1, "\n")
cat("MSE for regression model (data1):", MSE1, "\n")
cat("RMSE for regression model (data1):", RMSE1, "\n")

# Plot actual vs predicted values for regression
plot(test_data1$studied_credits, regression_predictions1, 
     xlab = "Actual Studied Credits", ylab = "Predicted Studied Credits",
     main = "Actual vs Predicted Values for Regression Model (data1)")
abline(0, 1, col = "orange")
