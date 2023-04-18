library(dplyr)
library(checkmate,asserthat)
source("R/utils_diff_marg_likelihood.R")
source("R/bnn_utils.R")

# install keras and tfproba from within, make sure version >= 2.10
# this approach works opposed to reticulate + pip/ conda install
# install_tensorflow(
#   extra_packages = c("keras", "tensorflow-hub", "tensorflow-probability"),
#   version = "2.10"
# )


diff_marg_likelihood_pred_ext <- function(labeled_data,
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
  #assert_formula(glm_formula)
  assert_character(target)

  n_imp = nrow(unlabeled_data) 
  results = matrix(nrow = nrow(unlabeled_data), ncol = 3)
  which_flip = seq(n_imp)
  for (i in seq(as.integer(n_imp / 10))) {
    # fit model to labeled data
    # labeled data: 1 = label, 2-61 = features, 62 = "nr"
    # https://blogs.rstudio.com/ai/posts/2019-06-05-uncertainty-estimates-tfprobability/

    # data wrangling for use in dl models
    x_l <- as.matrix(labeled_data[, 2:(length(labeled_data)-1)])
    y_l <- as.matrix(as.double(labeled_data[, 1])) - 1
    x_u <- as.matrix(unlabeled_data[, 2:(length(unlabeled_data)-1)])
    y_u <- as.matrix(as.double(unlabeled_data[, 1])) - 1

    # to create the initial pre-trained dnn, using n=40000 in entry_bnn.r
    # instructions
    #  1. set n=40000 in entry_bnn.r
    #  2. uncomment line 77 in entry_bnn.r and the line below
    #  3. remove the file bnn_pretrained.h5 to do pretraining
    #  4. stop, undo, rerun
    #bnn <- get_model(x_l, y_l, save_fname = "bnn_pretrained.h5", validation_split = 0.2, epochs = 1500)

    bnn <- get_model(x_l, y_l, load_fname = "bnn_pretrained.h5", validation_split = NULL, epochs = 150, verbose = 0)

    # predict on unlabeled data
    predicted_target <- as.array(bnn(x_u) %>% tfd_mean())[,1] 

    # assign predicted (pseudo) labels to unlabeled data
    unlabeled_data[c(target)] <- ifelse(predicted_target > 0.5, 1,0)  
    
    # create datasets that contain labeled data and one predicted instance each
    if(i >= 2){
      which_flip <- which_flip[-(winner)]
    }
    data_sets_pred_init = as.list(seq_along(which_flip)) 
    data_sets_pred = lapply(data_sets_pred_init, function(flip){
      new_data = rbind(labeled_data, unlabeled_data[flip,])
      new_data 
    })
    
    # now approximate marginal likelihood for each of the so-created data sets
    marg_l_pseudo <- lapply(data_sets_pred, function(data) {
      x_l <- as.matrix(data[, 2:(length(data)-1)])
      y_l <- as.matrix(as.double(data[, 1])) - 1
      bnn <- get_model(x_l, y_l, load_fname = "bnn_pretrained.h5", validation_split = NULL, epochs = 150, verbose = 0)
      all_vars <- c(extract_bnn_posterior(bnn, 1)$scale, extract_bnn_posterior(bnn, 2)$scale)
      log_determinant <- sum(log(all_vars))
      loglik <- sum(as.array(bnn(x_l) %>% tfd_log_prob(y_l)))

      # compute difference with prior
      prior_diff <- sum(lapply(bnn$weights, function(w) 
        ifelse(grepl("posterior", w$name), 0, sum(as.array(w)**2 / 2))
      ) %>% unlist())

      #return(loglik - log_determinant / 2)
      return(prior_diff)
    })
  
    #print(marg_l_pseudo)
    ord <- order(unlist(marg_l_pseudo), decreasing = TRUE)
    winner <- ord[1:10]

    # predict on it again and add to labeled data
    data <- unlabeled_data[winner,]
    x_l <- as.matrix(data[, 2:(length(data)-1)])
    y_l <- as.matrix(as.double(data[, 1])) - 1
    predicted_target <- as.array(bnn(x_l) %>% tfd_mean())
    new_labeled_obs <- unlabeled_data[winner,]
    new_labeled_obs[c(target)] <- ifelse(predicted_target > 0.5, 1,0)  

    # evaluate test error (on-the-fly inductive learning results)
    x_l <- as.matrix(test_data[, 2:(length(test_data))])
    y_l <- as.matrix(as.double(test_data$target_var)) - 1
    scores <- as.array(bnn(x_l) %>% tfd_mean())
    prediction_test <- ifelse(scores > 0.5, 1, 0)
    test_acc <- sum(prediction_test == test_data[c(target)])/nrow(test_data)
    cat("test accuracy", test_acc, "\n")
    
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


