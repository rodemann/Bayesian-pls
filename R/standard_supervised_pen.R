library(dplyr)
library(checkmate,asserthat)
library(glmnet)
source("R/lognet.R")


standard_supervised_pen <- function(labeled_data,
                                unlabeled_data,
                                test_data,
                                target,
                                glm_formula) {
  
  
  # some input checking
  assert_data_frame(labeled_data)
  assert_data_frame(unlabeled_data)
  assert_data_frame(test_data)
  assert_formula(glm_formula)
  assert_character(target)
  
  
  y = labeled_data[,c(target)]
  x = labeled_data[,!(names(labeled_data) %in% c(target, "nr"))]
  
  logistic_model <- glmnet(y = y, x = x, 
                        family = "binomial",
                        nlambda = 1,
                        lambda = 10,
                        alpha = 0) # ridge 
  # predict on unlabeled
  
  newx = unlabeled_data[,!(names(labeled_data) %in% c(target, "nr"))] %>% as.matrix
  predicted_target <- predict(logistic_model, newx = newx, type = "response")
  new_labeled_obs <- unlabeled_data
  new_labeled_obs[c(target)] <- ifelse(predicted_target > 0.5, 1,0)  
  
  
  
  predicted_label <- new_labeled_obs[c(target)] %>% unlist()
  number <- new_labeled_obs$nr %>% unlist()
  results <- cbind(number, predicted_label)
  
  # return transductive results (labels) and final model
  list(results, logistic_model)
  
}


