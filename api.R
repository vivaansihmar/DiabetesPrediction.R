library(plumber)


xgb_model <- readRDS("model/xgboost_model_tuned.rds")


#* @apiTitle Diabetes Prediction API

#* Predict diabetes using xgboost
#* @param Pregnancies:number
#* @param Glucose:number
#* @param BloodPressure:number
#* @param SkinThickness:number
#* @param Insulin:number
#* @param BMI:number
#* @param DiabetesPedigreeFunction:number
#* @param Age:number
#* @post /predict
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
  
  
  input_matrix <- model.matrix(~ . -1, data = input_data)
  pred <- predict(xgb_model, input_matrix)
  result <- ifelse(pred > 0.5, 1, 0)
  
  list(predicted_class = result, probability = round(pred, 3))
}
