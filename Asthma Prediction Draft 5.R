if (!require(readxl)) install.packages("readxl")
if (!require(glmnet)) install.packages("glmnet")
if (!require(naniar)) install.packages("naniar")
if (!require(caTools)) install.packages("caTools")
if (!require(car)) install.packages("car")
if (!require(caret)) install.packages("caret")
if (!require(pROC)) install.packages("pROC")
if (!require(dplyr)) install.packages("dplyr")
if (!require(kernlab)) install.packages("kernlab")
if (!require(e1071)) install.packages("e1071")
if (!require(rpart)) install.packages("rpart")
if (!require(rpart.plot)) install.packages("rpart")
if (!require(DMwR2)) install.packages("DMwR2")
if (!require(smotefamily)) install.packages("smotefamily")




library(readxl)
library(glmnet)
library(naniar)
library(caTools)
library(car)
library(caret)
library(pROC)
library(dplyr)
library(kernlab)
library(e1071)
library(rpart)
library(rpart.plot)
library(DMwR2)
library(smotefamily)


#SCALING AND IMPUTOING SHOULD BE DONE SEPARATELY, (split test and train data)


file_path <- "/Users/justi/Downloads/doi_10_5061_dryad_8bv8p__v20130809/CAMpaper_fulldata.xlsx"
data <- read_excel(file_path)

# Converts Asthma_ER_or_Hospitalization into a category and not numerical


data$Asthma_ER_or_Hospitalization <- as.factor(data$Asthma_ER_or_Hospitalization)


# Drop unecessary columns:

data <- data %>% select(-Control_with_missing_data)
data <- data %>% select(-true_other_cam)
data <- data %>% select(-cam_types)
data <- data %>% select(-cam_use)
data <- data %>% select(-ID)


# Imputing missing data:

data$fev1_to_fvc[is.na(data$fev1_to_fvc)] <- mean(data$fev1_to_fvc, na.rm = TRUE)  # Replace with median if preferred
data$fvc[is.na(data$fvc)] <- mean(data$fvc, na.rm = TRUE)
data$fev1[is.na(data$fev1)] <- mean(data$fev1, na.rm = TRUE)
data$fev1_over_pred[is.na(data$fev1_over_pred)] <- mean(data$fev1_over_pred, na.rm = TRUE)
data$predicted_fev1[is.na(data$predicted_fev1)] <- mean(data$predicted_fev1, na.rm = TRUE)


columns_to_impute <- c("fev1_to_fvc", "fvc", "fev1", "predicted_fev1", "fev1_over_pred")

for (col in columns_to_impute) {
  
  Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 2 * IQR
  upper_bound <- Q3 + 2 * IQR
  
  outliers <- data[[col]][data[[col]] < lower_bound | data[[col]] > upper_bound]
  outliers
  
  data[[col]][data[[col]] < lower_bound] <- lower_bound
  data[[col]][data[[col]] > upper_bound] <- upper_bound
  
}

columns_to_scale <- c("fev1_to_fvc", "fvc", "fev1", "predicted_fev1", "fev1_over_pred", "resuse", "age")

for (col in columns_to_scale) {
  
  data[[col]] <- as.vector(scale(data[[col]]))
  
}

data <- cbind(data, model.matrix(~ controller_medicine_use - 1, data))
data$controller_medicine_use <- NULL  # Drop the original categorical column

# One-hot encode a multi-class column (e.g., 'am_chiropractic')
data <- cbind(data, model.matrix(~ ethnicity - 1, data))
data$ethnicity <- NULL  # Drop the original categorical column

# For 'sex' (e.g., Male = 0, Female = 1)
data$sex <- ifelse(data$sex == "Male", 0, 1)

# For 'education' (e.g., High School = 0, College = 1)
data$education <- ifelse(data$education == "Less than college", 0, 1)

# For 'education' (e.g., High School = 0, College = 1)
data$household_income <- ifelse(data$household_income == "<$6,000", 0, 1)


train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)


X <- data[ , !(names(data) %in% "Asthma_ER_or_Hospitalization")]
y <- data$Asthma_ER_or_Hospitalization

# Create a train-test split
set.seed(42)
trainIndex <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[trainIndex, ]
y_train <- y[trainIndex]
X_test <- X[-trainIndex, ]
y_test <- y[-trainIndex]

# Combine training data for caret
training_data <- cbind(X_train, Asthma_ER_or_Hospitalization = y_train)
testing_data <- cbind(X_test, Asthma_ER_or_Hospitalization = y_test)

training_data

# SMOTE:

smoteData <- SMOTE(X = X_train, target = training_data$Asthma_ER_or_Hospitalization, K = 5, dup_size = 2)
smoteData$data$class <- ifelse(smoteData$data$class == "1", 1, 0)



smoteData <- smoteData$data

# May or may not have to convert target class to num from chr

smoteData <- na.omit(smoteData)

smoteData <- data.frame(smoteData)

smoteData

smoteData$class <- as.factor(smoteData$class)


tuneGrid=data.frame(C=c(0.25, 0.5, 1,5,8,12,100))

model <- train(class ~ ., data = smoteData, method = "svmLinear", tuneGrid = tuneGrid, trControl = trainControl(method = "cv", number = 5))

smote_predictions <- predict(model, newdata = smoteData)

confusionMatrix(smote_predictions, smoteData$class)

# Renaming columns because the smote process changes the column names

colnames(testing_data)[which(names(testing_data) == "controller_medicine_use0.5<=PDC<0.8")] <- "controller_medicine_use0.5..PDC.0.8"
colnames(testing_data)[which(names(testing_data) == "controller_medicine_usePDC<0.5")] <- "controller_medicine_usePDC.0.5"
colnames(testing_data)[which(names(testing_data) == "controller_medicine_usePDC>=0.8")] <- "controller_medicine_usePDC..0.8"

colnames(testing_data)[which(names(testing_data) == "Asthma_ER_or_Hospitalization")] <- "class"

smote_predictions

test_predictions <- predict(model, newdata = testing_data)
str(testing_data)
str(test_predictions)

test_predictions

numeric_test_predictions <- as.numeric(test_predictions)
numeric_test_data <- as.numeric(testing_data$class)


confusionMatrix(test_predictions, testing_data$class)

roc_object <- roc(testing_data$class, numeric_test_predictions)

# calculate area under curve
auc(roc_object)

