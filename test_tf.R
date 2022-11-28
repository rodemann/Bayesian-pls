library(tensorflow)
library(tfprobability)
library(keras)
# install keras and tfproba from within, make sure version >= 2.10
# this approach works opposed to reticulate + pip/ conda install
# install_tensorflow(
#   extra_packages = c("keras", "tensorflow-hub", "tensorflow-probability"),
#   version = "2.10"
# )

foo <- tf$constant("Hello Tensorflow!")
tfp <- import("tensorflow_probability")
tfd <- tfp$distributions
n <- tfd$Normal(loc = 0, scale = 1)
n$sample(6L)

bn = tfd$Binomial(total_count = 7, probs = 0.3)
bn$sample(8L)

# exit Q
test = function(a){
  print(a)
  b = "iris"
  browser()
  for (i in seq_len(a)){
    print(i)
  }
  c = "dani"
}

test(2)

for (a in c(0, 1, 2)){
  test(a=a)
}


#### Keras tf proba 

encoded_size <- 2
input_shape <- c(2L, 2L, 1L)
train_size <- 100
x_train <- array(runif(train_size * Reduce(`*`, input_shape)), dim = c(train_size, input_shape))

# encoder is a keras sequential model
encoder_model <- keras_model_sequential() %>%
  layer_flatten(input_shape = input_shape) %>%
  layer_dense(units = 10, activation = "relu") %>%
  layer_dense(units = params_size_multivariate_normal_tri_l(encoded_size)) %>%
  layer_multivariate_normal_tri_l(event_size = encoded_size) %>%
  # last layer adds KL divergence loss
  tfprobability::layer_kl_divergence_add_loss(
      distribution = tfd_independent(
        tfd_normal(loc = c(0, 0), scale = 1),
        reinterpreted_batch_ndims = 1
      ),
      weight = train_size)

# decoder is a keras sequential model
decoder_model <- keras_model_sequential() %>%
  layer_dense(units = 10,
              activation = 'relu',
              input_shape = encoded_size) %>%
  layer_dense(params_size_independent_bernoulli(input_shape)) %>%
  layer_independent_bernoulli(event_shape = input_shape,
                              convert_to_tensor_fn = tfp$distributions$Bernoulli$logits)

# keras functional model uniting them both
vae_model <- keras_model(inputs = encoder_model$inputs,
                         outputs = decoder_model(encoder_model$outputs[1]))

# VAE loss now is just log probability of the data
vae_loss <- function (x, rv_x)
    - (rv_x %>% tfd_log_prob(x))

vae_model %>% compile(
  optimizer = "adam",
  loss = vae_loss
)

vae_model %>% fit(x_train, x_train, batch_size = 25, epochs = 10)



##### ARXIVE OLD CRAP
# install.packages("tensorflow")
# install.packages("keras")
# install.packages("tfprobability")

#library("renv")
#library("reticulate")
#library("tfprobability")
#library("keras")
#library("tensorflow")
# allows us to use the conda env with the correct tf python installs
# reticulate::conda_python(envname="/home/ubuntu/miniconda3/envs/bnn_env/")
# tensorflow::install_tensorflow(version = "2.10.0", envname="/home/ubuntu/miniconda3/envs/bnn_env/")
# keras::install_keras(version = "2.10.0", envname="/home/ubuntu/miniconda3/envs/bnn_env/")
# tfprobability::install_tfprobability(version = "0.16", envname="/home/ubuntu/miniconda3/envs/bnn_env/")

# tensorflow::install_tensorflow(version = "2.10.0", envname="bnn_env2", method="conda")
# keras::install_keras(version = "2.10.0", envname="bnn_env2")
# tfprobability::install_tfprobability(version = "0.16", envname="bnn_env2")