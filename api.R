library(plumber)


xgb_model <- readRDS("C:/d1p1/models/xgboost_model_tuned.rds")
rf_model <- readRDS("C:/d1p1/models/random_forest_model.rds")


cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  plumber::forward()
}

#* @apiTitle Diabetes Prediction API

#* @param Pregnancies
#* @param Glucose
#* @param BloodPressure
#* @param SkinThickness
#* @param Insulin
#* @param BMI
#* @param DiabetesPedigreeFunction
#* @param Age
#* @post /predict
#* @serializer unboxedJSON
function(Pregnancies, Glucose, BloodPressure, SkinThickness, Insulin, BMI, DiabetesPedigreeFunction, Age) {
  input_data <- data.frame(
    Pregnancies = as.numeric(Pregnancies),
    Glucose = as.numeric(Glucose),
    BloodPressure = as.numeric(BloodPressure),
    SkinThickness = as.numeric(SkinThickness),
    Insulin = as.numeric(Insulin),
    BMI = as.numeric(BMI),
    DiabetesPedigreeFunction = as.numeric(DiabetesPedigreeFunction),
    Age = as.numeric(Age)
  )
  
  
  input_data$BMI_Age <- input_data$BMI * input_data$Age
  input_data$Glucose_Insulin <- input_data$Glucose / (input_data$Insulin + 1)
  
 
  input_matrix <- model.matrix(~ . -1, data = input_data)
 
  xgb_pred <- predict(xgb_model, input_matrix)
  xgb_class <- ifelse(as.numeric(xgb_pred) > 0.5, 1, 0)
  rf_pred <- as.character(predict(rf_model, input_data))
  

  list(
    random_forest_prediction = rf_pred,
    xgboost_prediction = xgb_class,
    xgboost_probability = round(as.numeric(xgb_pred), 3)
  )
}
