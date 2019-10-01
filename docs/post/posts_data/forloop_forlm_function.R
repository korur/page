
```{r Summary of the model performances}
models <- c("lm", "glm", "glmnet", "rf", "gbm", "naive")
lm <- round(mean(c(variance_1000_1, variance_1000_2, variance_1000_3)), 3)
glm <- max(model_glm$results$ROC)
glmnet <- max(model_glmnet$results$ROC)
rf <- max(model_rf$results$ROC)
gbm <- max(model_gbm$results$ROC)
nb <- max(model_nb$results$ROC)
model_vector <- c( lm, glm, glmnet, rf, gbm, naive
)
df <- data.frame(model_vector, row.names=models)
```



sample_rows <- list()
dia_train <- list()
dia_test <- list()
lm <- list()
p <-  list()
roc_auc <- list()
for(i in 1:1000){
sample_rows[[i]] <- sample(nrow(diabetes), nrow(diabetes) * 0.8)
# Create the training dataset
dia_train[[i]] <- diabetes[sample_rows[[i]], ]
# Create the test dataset
dia_test[[i]] <- diabetes[-sample_rows[[i]], ]
lm[[i]] <- lm(Outcome ~ .,data = dia_train[[i]])
p[[i]] <- predict(lm[[i]], dia_test[[i]])

# Calculate AUC
roc_auc[[i]] <- roc(dia_test[[i]]$Outcome, p[[i]])$auc[1]
lm_mean <- mean(unlist(roc_auc))
}
print(paste("After 1000 train/test split iterations, average accuracy of your model is", lm_mean))

# MY FIRST FUNCTION
# Try in a function:
multi_split <- function(x){
sample_rows <- list()
dia_train <- list()
dia_test <- list()
lm <- list()
p <-  list()
roc_auc <- list()
for(i in 1:x){
  sample_rows[[i]] <- sample(nrow(diabetes), nrow(diabetes) * 0.8)
  # Create the training dataset
  dia_train[[i]] <- diabetes[sample_rows[[i]], ]
  # Create the test dataset
  dia_test[[i]] <- diabetes[-sample_rows[[i]], ]
  lm[[i]] <- lm(Outcome ~ .,data = dia_train[[i]])
  p[[i]] <- predict(lm[[i]], dia_test[[i]])
  
  # Calculate AUC
  roc_auc[[i]] <- roc(dia_test[[i]]$Outcome, p[[i]])$auc[1]
  lm_mean <- mean(unlist(roc_auc))
}
print(paste("After", x, "train/test split iterations, average accuracy of your model is", lm_mean))
}


```{r Build a glm model}

# MyControl
myControl <- trainControl(
  method = "cv", 
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)
# Model
model_glm <- train(
  Outcome~.,
  method = "glm",
  data = diabetes,
  trControl = myControl
)
model_glm

```


```{r Option glmnet train function}
# Convert Outcome to a factor with two levels
diabetes$Outcome <- factor(diabetes$Outcome)
levels(diabetes$Outcome) <- c("H", "D")

# MyControl
myControl <- trainControl(
  method = "cv", 
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)
# Model
model <- train(
  Outcome~.,
  method = "glm",
  data = diabetes,
  trControl = myControl
)
model

```

```{r Build a Random forest model}

# MyControl
myControl <- trainControl(
  method = "cv", 
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)
# Model
model_rf <- train(
  Outcome~.,
  method = "ranger",
  data = diabetes,
  trControl = myControl
)
model_rf

```
