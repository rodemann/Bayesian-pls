library(dplyr)
library(checkmate,asserthat)

source("R/utils_diff_marg_likelihood.R")
source("R/bnn_utils.R")



standard_supervised_glm <- function(labeled_data,
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
  

 
    logistic_model <- glm(formula = formula, 
                          data = labeled_data, 
                          family = "binomial")
    # predict on unlabeled
    
    predicted_target <- predict(logistic_model, newdata = unlabeled_data, type = "response")
    new_labeled_obs <- unlabeled_data
    new_labeled_obs[c(target)] <- ifelse(predicted_target > 0.5, 1,0)  
    
   
    
    predicted_label <- new_labeled_obs[c(target)] %>% unlist()
    number <- new_labeled_obs$nr %>% unlist()
    results <- cbind(number, predicted_label)
    
  # return transductive results (labels) and final model
  list(results, logistic_model)
  
}



standard_supervised <- function(labeled_data,
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
  
    # --- old glm stuff
    #logistic_model <- glm(formula = formula, 
    #                      data = labeled_data, 
    #                      family = "binomial")
    #predicted_target <- predict(logistic_model, newdata = unlabeled_data, type = "response")
    # --- end

    # Data wrangling for use in DL models
    x_l <- as.matrix(labeled_data[, 2:(length(labeled_data)-1)])
    y_l <- as.matrix(as.double(labeled_data[, 1])) - 1

    # to create the initial pre-trained DNN, using n=40000 in entry_bnn.R
    # instructions
    #  1. set n=40000 in entry_bnn.R
    #  2. uncomment line 77 in entry_bnn.R and the line below
    #  3. remove the file bnn_pretrained.h5 to do pretraining
    #  4. stop, undo, rerun
    #bnn <- get_model(x_l, y_l, save_fname = "bnn_pretrained.h5", validation_split = 0.2, epochs = 1500)

    bnn <- get_model(x_l, y_l, load_fname = "bnn_pretrained.h5", validation_split = NULL, epochs = 150, verbose = 0)
 
    # predict on unlabeled
    
    x_u <- as.matrix(unlabeled_data[, 2:(length(unlabeled_data)-1)])
    y_u <- as.matrix(as.double(unlabeled_data[, 1])) - 1
    p_u <- bnn(x_u)
    uncertainty <- as.array(p_u %>% tfd_stddev())[,1] 
    winner <- which.min(uncertainty)
    predicted_target <- as.array(p_u %>% tfd_mean())[winner,1]

    new_labeled_obs <- unlabeled_data
    new_labeled_obs[c(target)] <- ifelse(predicted_target > 0.5, 1,0)  
   
    
    predicted_label <- new_labeled_obs[c(target)] %>% unlist()
    number <- new_labeled_obs$nr %>% unlist()
    results <- cbind(number, predicted_label)
    
  # return transductive results (labels) and final model
  list(results, bnn)
  
}


