library(tidyverse)
library(caret)
library(e1071)
library(xgboost)
library(randomForest)
library(pROC)
library(smotefamily)

data <- read.csv("C:/d1p1/data/diabetes.csv")
data$Outcome <- factor(data$Outcome, levels = c(0, 1))

na_columns <- c("Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI")
data[na_columns] <- lapply(data[na_columns], function(x) ifelse(x == 0, NA, x))
data[na_columns] <- lapply(data[na_columns], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))

data$BMI_Age <- data$BMI * data$Age
data$Glucose_Insulin <- data$Glucose / (data$Insulin + 1)

set.seed(123)
trainIndex <- createDataPartition(data$Outcome, p = 0.8, list = FALSE)
train_data <- data[trainIndex, ]
test_data  <- data[-trainIndex, ]

smote_input <- train_data
smote_input$Outcome <- as.numeric(as.character(smote_input$Outcome))
smote_result <- SMOTE(X = smote_input[, -which(names(smote_input) == "Outcome")],
                      target = smote_input$Outcome,
                      K = 5)
train_data_smote <- smote_result$data
train_data_smote$class <- factor(train_data_smote$class, levels = c(0, 1))
colnames(train_data_smote)[ncol(train_data_smote)] <- "Outcome"

rf_model <- randomForest(Outcome ~ ., data = train_data_smote, ntree = 500, importance = TRUE)
rf_preds <- predict(rf_model, newdata = test_data)
print(confusionMatrix(rf_preds, test_data$Outcome))

train_matrix <- model.matrix(Outcome ~ . -1, data = train_data_smote)
train_label <- as.numeric(as.character(train_data_smote$Outcome))
test_matrix  <- model.matrix(Outcome ~ . -1, data = test_data)
test_label   <- as.numeric(as.character(test_data$Outcome))

xgb_grid <- expand.grid(
  nrounds = 200,
  eta = c(0.01, 0.1),
  max_depth = c(3, 6),
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.8
)

train_control <- trainControl(method = "cv", number = 5, verboseIter = FALSE)

xgb_train <- train(
  x = train_matrix,
  y = as.factor(train_label),
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = xgb_grid,
  metric = "Accuracy"
)

xgb_preds <- predict(xgb_train, newdata = test_matrix)
confusionMatrix(xgb_preds, as.factor(test_label))

saveRDS(rf_model, "models/random_forest_model.rds")
saveRDS(xgb_train, "models/xgboost_model_tuned.rds")

varImpPlot(rf_model)
xgb.plot.importance(xgb.importance(model = xgb_train$finalModel))
