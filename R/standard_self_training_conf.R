library(dplyr)
library(checkmate,asserthat)
source("R/bnn_utils.R")



# standard_self_training_conf_glm <- function(labeled_data,
#                                    unlabeled_data,
#                                    test_data,
#                                    target,
#                                    glm_formula) {
  
#   # some input checking
#   assert_data_frame(labeled_data)
#   assert_data_frame(unlabeled_data)
#   assert_data_frame(test_data)
#   assert_formula(glm_formula)
#   assert_character(target)
  
#   n_imp = nrow(unlabeled_data)
#   results = matrix(nrow = n_imp, ncol = 3)
#   which_flip = seq(n_imp)

#   for (i in seq(n_imp)) {
    
    
#     logistic_model <- glm(formula = formula, 
#                           data = labeled_data, 
#                           family = "binomial")
#     #choose instance whose prediction has most CONFIDENCE (as opposed to certainty)
#     response_preds <- predict(logistic_model, newdata= unlabeled_data, type = "response") 
#     abs_confidence <- ifelse(response_preds > 0.5, abs(1 - response_preds), abs(0 - response_preds))
#     winner <- which.min(abs_confidence)
#     # min.col <- function(m, ...) max.col(-m, ...)
#     # winner <- min.col(matrix(abs_confidence, nrow = 1), ties.method = "random") # make sure indices are returned randomly in case of ties
#     # if(length(winner) >= 2)
    
    
#     # predict it
#     predicted_target <- predict(logistic_model, newdata= unlabeled_data[winner,], type = "response")
    
#     new_labeled_obs <- unlabeled_data[winner,]
#     new_labeled_obs[c(target)] <- ifelse(predicted_target > 0.5, 1,0)  
    

#     # update labeled data
#     labeled_data<- rbind(labeled_data, new_labeled_obs)
#     # evaluate test error (on-the-fly inductive learning results)
#     logistic_model <- glm(formula = formula, data = labeled_data, family = "binomial") # refit model with added label
#     scores = predict(logistic_model, newdata = test_data, type = "response") 
#     prediction_test <- ifelse(scores > 0.5, 1, 0)
#     test_acc <- sum(prediction_test == test_data[c(target)])/nrow(test_data)
    
#     # store results
#     results[i,] <- c(unlabeled_data[winner,]$nr, new_labeled_obs[c(target)], test_acc) %>% unlist()
#     unlabeled_data <- unlabeled_data[-winner,]
#   }
  
#   # return transductive results (labels) and final model
#   list(results, logistic_model)
  
# }




standard_self_training_conf <- function(labeled_data,
                                      unlabeled_data,
                                      test_data,
                                      target,
                                      glm_formula) {
  # n_feats <- 5   # 20 also fine but much slower
  # labeled_data <- labeled_data[,c(1:n_feats, ncol(labeled_data))]
  # unlabeled_data <- unlabeled_data[,c(1:n_feats,ncol(unlabeled_data))]
  # test_data <- test_data[,c(1:n_feats,ncol(test_data))]

  # some input checking
  assert_data_frame(labeled_data)
  assert_data_frame(unlabeled_data)
  assert_data_frame(test_data)
  assert_formula(glm_formula)
  assert_character(target)
  
  n_imp = nrow(unlabeled_data)
  results = matrix(nrow = n_imp, ncol = 3)
  which_flip = seq(n_imp)
  for (i in seq(as.integer(n_imp/10))) {
    # fit model to labeled data
    # labeled data: 1 = label, 2-61 = features, 62 = "nr"
    # https://blogs.rstudio.com/ai/posts/2019-06-05-uncertainty-estimates-tfprobability/

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

    cat("called with", nrow(x_l), "rows\n1")
    bnn <- get_model(x_l, y_l, load_fname = "bnn_pretrained.h5", validation_split = NULL, epochs = 150, verbose = 0)

    x_u <- as.matrix(unlabeled_data[, 2:(length(unlabeled_data)-1)])
    y_u <- as.matrix(as.double(unlabeled_data[, 1])) - 1
    p_u <- bnn(x_u)
    prob_score <- as.array(p_u %>% tfd_mean())[,1] 
    #winner <- which.max(abs(0.5 - prob_score))
    ord <- order(unlist(prob_score), decreasing = TRUE)
    winner <- ord[1:10]
    predicted_target <- prob_score[winner]

    new_labeled_obs <- unlabeled_data[winner,]
    new_labeled_obs[c(target)] <- ifelse(predicted_target > 0.5, 1,0)  

    # evaluate test error (on-the-fly inductive learning results)
    x_l <- as.matrix(test_data[, 2:(length(test_data))])
    y_l <- as.matrix(as.double(test_data$target_var)) - 1
    scores <- as.array(bnn(x_l) %>% tfd_mean())
    prediction_test <- ifelse(scores > 0.5, 1, 0)
    test_acc <- sum(prediction_test == test_data[c(target)])/nrow(test_data)
    cat("TEST ACCURACY", test_acc, "\n")
    
    # update labeled data
    #labeled_data<- rbind(labeled_data, new_labeled_obs)
    # store results
    #results[i,] <- c(unlabeled_data[winner,]$nr, new_labeled_obs[c(target)], test_acc) %>% unlist()
    #unlabeled_data <- unlabeled_data[-winner,]

    # update labeled data and store results
    labeled_data<- rbind(labeled_data, new_labeled_obs)
    results[((i-1)*10+1):(i*10), 1] <- c(unlabeled_data[winner,]$nr) %>% unlist()
    results[((i-1)*10+1):(i*10), 2] <- c(new_labeled_obs[c(target)])%>% unlist()
    results[((i-1)*10+1):(i*10), 3] <- c(test_acc)
    unlabeled_data <- unlabeled_data[-winner,]
  }

  # get final model
  x_l <- as.matrix(labeled_data[, 2:(length(labeled_data)-1)])
  y_l <- as.matrix(as.double(labeled_data$target_var)) - 1
  bnn <- get_model(x_l, y_l, load_fname = "bnn_pretrained.h5", validation_split = NULL, epochs = 150, verbose = 0)

  # return transductive results (labels) and final model
  list(results, bnn)
}


