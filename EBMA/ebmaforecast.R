#install.packages("pacman")
if(!require(pacman)) install.packages('pacman', dependencies = T)

p_load(
  dplyr,
  EBMAforecast,
  here,
  Hmisc,
  openxlsx,
  tidyr
)

common_pred <- read.xlsx("data/bma_db.xlsx") #file with model predictions and observed outcome
                
all_col_names <- colnames(common_pred)
no_pred_names <- c("sample_id", "outcome")
model_names <- all_col_names[!all_col_names %in% no_pred_names]

set.seed(123)  # random splitting, seed for reproducibility

n <- nrow(common_pred)
train_indices <- sample(seq_len(n), size = floor(2/3 * n))

training_set <- common_pred[train_indices, ]
test_set <- common_pred[-train_indices, ]

#EBMA framework

this.ForecastData <- makeForecastData(.predCalibration=training_set[,!names(common_pred) %in% c("sample_id", "outcome")],
                                      .outcomeCalibration=training_set[,"outcome"],
                                      .predTest=test_set[,!names(common_pred) %in% c("sample_id", "outcome")],
                                      .outcomeTest=test_set[,"outcome"],
                                      .modelNames=model_names)                                      

this.ensemble <- calibrateEnsemble(this.ForecastData, model="logit", method = "gibbs") 

summary(this.ensemble, period="calibration")
plot(this.ensemble, period="calibration")
summary(this.ensemble, period="test", showCoefs=FALSE)
plot(this.ensemble, period="test")

#save(this.ensemble, file = "ebma.RData")
