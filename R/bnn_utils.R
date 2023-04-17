




get_model <- function(x_l, y_l, save_fname = NULL, load_fname = NULL, validation_split = 0.2, epochs = 1500, verbose = 2) {
  num_classes <- 2
  dim <- 4
  klw <- ifelse(is.null(x_l), 1, 1/nrow(x_l))
  bnn <- keras::keras_model_sequential() %>% 
    tfprobability::layer_dense_variational(
      units = 128,
      batch_input_shape=list(NULL, as.integer(dim)), # input shape for first layer
      make_posterior_fn = posterior_mean_field,
      make_prior_fn = prior_trainable,
      activation = "tanh",
      kl_weight=klw,
    ) %>% tfprobability::layer_dense_variational(
      units = num_classes - 1, # binary classification case
      make_posterior_fn = posterior_mean_field,
      make_prior_fn = prior_trainable,
      kl_weight=klw,
    )  %>% tfprobability::layer_independent_bernoulli(
      event_shape = num_classes - 1
    )

  negloglik <- function(y_true, y_pred) {
    # y_pred is the distribution returned by last layer_independent_bernoulli
    -(y_pred %>% tfd_log_prob(y_true))
  }

  if(!is.null(load_fname) && file.exists(load_fname)) {
    bnn$load_weights(load_fname);
    stopifnot(load_fname != save_fname);
  }

  if((is.null(save_fname) || !file.exists(save_fname))) {
    # train model
    bnn %>% keras::compile(
      optimizer = keras::optimizer_adam(learning_rate = 0.001),
      loss = negloglik, metrics = c("accuracy")
    )
    bnn %>% keras::fit(
      x_l, y_l,
      validation_split = validation_split,
      verbose = verbose, batch_size = 128, epochs = epochs
    )

    if(!is.null(save_fname)) {
      bnn$save_weights(save_fname);
    }
  }
  else {
    bnn$load_weights(save_fname)
  }

  return(bnn)
}


# import tf, tfproba, keras
library(tensorflow)
library(tfprobability)
library(keras)

###### 
# jann BNN helpers
###### 
extract_bnn_posterior <- function(network, layer) {
  ws <- network$layers[[layer]][["_posterior"]]$weights[[1]]
  mean <- as.array(ws[1:(length(ws)/2)])
  scale <- as.array(1e-5 + tf$nn$softplus(log(expm1(1)) + ws[(length(ws) / 2 + 1):length(ws)]))
  list(mean=mean,scale=scale)
}

posterior_mean_field <- function(kernel_size, bias_size = 0, dtype = NULL) {
    n <- kernel_size + bias_size
    c <- log(expm1(1))
    keras_model_sequential(list(
      layer_variable(shape = 2 * n, dtype = dtype, trainable = TRUE),
      layer_distribution_lambda(
        make_distribution_fn = function(t) {
          tfd_independent(tfd_normal(
            loc = t[1:n],
            scale = 1e-5 + tf$nn$softplus(1e-5 + t[(n + 1):(2 * n)])
            ), reinterpreted_batch_ndims = 1)
        }
      )
    ))
  }

prior_trainable <- function(kernel_size, bias_size = 0, dtype = NULL) {
  n <- kernel_size + bias_size
  keras_model_sequential() %>%
    layer_variable(n, dtype = dtype, trainable = TRUE) %>%
    layer_distribution_lambda(function(t) {
      tfd_independent(tfd_normal(loc = t, scale = 1),
                      reinterpreted_batch_ndims = 1)
    })
}
