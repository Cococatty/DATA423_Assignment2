# https://dpmartin42.github.io/posts/r/imbalanced-classes-part-1

library(dplyr) # for data manipulation
library(caret) # for model-building
library(DMwR) # for smote implementation
library(purrr) # for functional programming (map)
library(pROC) # for AUC calculations

set.seed(2969)

imbal_train <- twoClassSim(100,
                           intercept = -25,
                           linearVars = 20,
                           noiseVars = 10)

imbal_test  <- twoClassSim(50,
                           intercept = -25,
                           linearVars = 20,
                           noiseVars = 10)

prop.table(table(imbal_train$Class))

# Set up control function for training

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     # summaryFunction = twoClassSummary,
                     classProbs = TRUE)

# Build a standard classifier using a gradient boosted machine

set.seed(5627)

orig_fit <- train(Class ~ .,
                  data = imbal_train,
                  method = "gbm",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl)

# Build custom AUC function to extract AUC
# from the caret model object
test_roc <- function(model, data) {
  roc(data$Class,
      predict(model, data, type = "prob")[, "Class2"])
  
}

orig_fit %>%
  test_roc(data = imbal_test) %>%
  auc()


# Build up-sampled model

ctrl$sampling <- "up"

up_fit <- train(Class ~ .,
                data = imbal_train,
                method = "gbm",
                verbose = FALSE,
                metric = "ROC",
                trControl = ctrl)
anyNA(modelTrainDT)
modelTrainDT <- na.omit(modelTrainDT)
up_fit1 <- train(Owner.size ~ .,
                data = modelTrainDT,
                method = "gbm",
                verbose = FALSE,
                metric = "ROC",
                trControl = ctrl)

# Build smote model

ctrl$sampling <- "smote"

smote_fit <- train(Owner.size ~ .,
                   data = modelTrainDT,
                   method = "gbm",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = ctrl)


# Examine results for test set

model_list <- list(original = orig_fit,
                   weighted = weighted_fit,
                   down = down_fit,
                   up = up_fit,
                   SMOTE = smote_fit)

model_list_roc <- model_list %>%
  map(test_roc, data = imbal_test)

model_list_roc %>%
  map(auc)


results_list_roc <- list(NA)
num_mod <- 1

for(the_roc in model_list_roc){
  
  results_list_roc[[num_mod]] <- 
    data_frame(tpr = the_roc$sensitivities,
               fpr = 1 - the_roc$specificities,
               model = names(model_list)[num_mod])
  
  num_mod <- num_mod + 1
  
}

results_df_roc <- bind_rows(results_list_roc)

# Plot ROC curve for all 5 models

custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_bw(base_size = 18)
