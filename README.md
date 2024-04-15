
library(dplyr)  # For data manipulation
library(caret)  # For machine learning model training and evaluation
data1 <- read.csv("C:/Users/Admin/Downloads/oulad-students.csv")
data1 <- data1[, c("code_module", "code_presentation", "id_student", "gender", "region",
                   "highest_education", "imd_band", "age_band", "num_of_prev_attempts",
                   "studied_credits", "disability", "final_result",
                   "module_presentation_length", "date_registration", "date_unregistration")]

factor_columns1 <- c("code_module", "code_presentation", "gender", "region",
                     "highest_education", "imd_band", "age_band", "num_of_prev_attempts",
                     "disability", "final_result")

data1[factor_columns1] <- lapply(data1[factor_columns1], as.factor)

data1 <- na.omit(data1)

set.seed(123)  # For reproducibility
train_index1 <- createDataPartition(data1$final_result, p = 0.8, list = FALSE)

train_data1 <- data1[train_index1, ]
test_data1 <- data1[-train_index1, ]

classification_model1 <- glm(final_result ~ ., data = train_data1, family = "binomial")

regression_model1 <- lm(studied_credits ~ ., data = train_data1)

classification_predictions1 <- predict(classification_model1, newdata = test_data1, type = "response")

predicted_classes1 <- ifelse(classification_predictions1 > 0.5, "Pass", "Fail")  # Adjust the threshold as needed
predicted_classes1 <- factor(predicted_classes1, levels = levels(test_data1$final_result))

regression_predictions1 <- predict(regression_model1, newdata = test_data1)


classification_confusion_matrix1 <- confusionMatrix(data = predicted_classes1, reference = test_data1$final_result)

MAE1 <- mean(abs(regression_predictions1 - test_data1$studied_credits))
MSE1 <- mean((regression_predictions1 - test_data1$studied_credits)^2)
RMSE1 <- sqrt(mean((regression_predictions1 - test_data1$studied_credits)^2))

print("Classification Model Evaluation for data1:")
print(classification_confusion_matrix1)

cat("MAE for regression model (data1):", MAE1, "\n")
cat("MSE for regression model (data1):", MSE1, "\n")
cat("RMSE for regression model (data1):", RMSE1, "\n")

plot(test_data1$studied_credits, regression_predictions1, 
     xlab = "Actual Studied Credits", ylab = "Predicted Studied Credits",
     main = "Actual vs Predicted Values for Regression Model (data1)")
abline(0, 1, col = "orange")
