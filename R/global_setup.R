###############
## global setup
###############
source("R/bnn_utils.R")
library(dplyr)
share_unlabeled = 0.9
set.seed(2138720)

# half of this is test
n = 400
N = 6

# simulate data
p = 60 
  
feature_1 <- rnorm(n, mean = 0.2)
feature_2 <- rnorm(n, mean = 2)
feature_3 <- rnorm(n, mean = 1.8)
feature_4 <- rnorm(n, mean = -1)
feature_5 <- rnorm(n, mean = 1.5, sd = 4)
feature_6 <- rnorm(n, mean = -1, sd = 8)
feature_7 <- rnorm(n, mean = -2, sd = 1)
feature_8 <- rnorm(n, mean = 0, sd = 4)
feature_9 <- rnorm(n, mean = 4, sd = 8)
feature_10 <- rnorm(n, mean = 2, sd = 4)
feature_11 <- rnorm(n, mean = 3, sd = 2)
feature_12 <- rnorm(n, mean = -2, sd = 18)
feature_13 <- rnorm(n, mean = -1, sd = 30)
feature_14 <- rnorm(n, mean = -1, sd = 1)
feature_15 <- rnorm(n, mean = -1, sd = 2)
feature_16 <- rnorm(n, mean = 20, sd = 4)
feature_17 <- rnorm(n, mean = 1, sd = 2)
feature_18 <- rnorm(n, mean = -5, sd = 18)
feature_19 <- rnorm(n, mean = -9, sd = 30)
feature_20 <- rnorm(n, mean = -1, sd = 1)
feature_21 <- rnorm(n, mean = 23, sd = 2)
feature_22 <- rnorm(n, mean = -21, sd = 18)
feature_23 <- rnorm(n, mean = -14, sd = 30)
feature_24 <- rnorm(n, mean = -2, sd = 11)
feature_25 <- rnorm(n, mean = 0, sd = 24)
feature_26 <- rnorm(n, mean = 2, sd = 40)
feature_27 <- rnorm(n, mean = 60, sd = 2)
feature_28 <- rnorm(n, mean = 41, sd = 18)
feature_29 <- rnorm(n, mean = 64, sd = 30)
feature_30 <- rnorm(n, mean = -13, sd = 11)
feature_31 <- rnorm(n, mean = 0, sd = 2)
feature_32 <- rnorm(n, mean = -22, sd = 18)
feature_33 <- rnorm(n, mean = -19, sd = 20)
feature_34 <- rnorm(n, mean = -21, sd = 1)
feature_35 <- rnorm(n, mean = 0.22, sd = 24.5)
feature_36 <- rnorm(n, mean = 2.46, sd = 40)
feature_37 <- rnorm(n, mean = 0.46, sd = 2)
feature_38 <- rnorm(n, mean = -42, sd = 18)
feature_39 <- rnorm(n, mean = -67, sd = 30)
feature_40 <- rnorm(n, mean = -12, sd = 11)
feature_41 <- rnorm(n, mean = 24, sd = 0.2)
feature_42 <- rnorm(n, mean = -47, sd = 18)
feature_43 <- rnorm(n, mean = -44, sd = 30)
feature_44 <- rnorm(n, mean = -4.67, sd = 11)
feature_45 <- rnorm(n, mean = 3.48, sd = 24)
feature_46 <- rnorm(n, mean = 2.576, sd = 0.1)
feature_47 <- rnorm(n, mean = 0, sd = 0.2)
feature_48 <- rnorm(n, mean = 74, sd = 0.18)
feature_49 <- rnorm(n, mean = -87, sd = 3)
feature_50 <- rnorm(n, mean = -1, sd = 0.11)
feature_51 <- rnorm(n, mean = 98, sd = 2)
feature_52 <- rnorm(n, mean = -41, sd = 18)
feature_53 <- rnorm(n, mean = -14, sd = 30)
feature_54 <- rnorm(n, mean = -45, sd = 11)
feature_55 <- rnorm(n, mean = 0, sd = 24)
feature_56 <- rnorm(n, mean = 20, sd = 40)
feature_57 <- rnorm(n, mean = 3, sd = 2)
feature_58 <- rnorm(n, mean = 4, sd = 18)
feature_59 <- rnorm(n, mean = 6, sd = 30)
feature_60 <- rnorm(n, mean = -6, sd = 11)

lin_comb <- 2.4- 7.9*feature_1-4*feature_2-4*feature_3
# only for pretraining
#lin_comb <- lin_comb + rnorm(n, sd=2)

formula <- target_var ~ .

prob = 1/(1+exp(-lin_comb))
target_var <-rbinom(n, 1, prob = prob)
cat("--- data target average", mean(target_var))

data_frame <- data_frame(target_var = target_var, feature_1 = feature_1, feature_2 = feature_2, feature_3 = feature_3, feature_4 = feature_4)

bnn <- keras::keras_model_sequential() %>% 
  tfprobability::layer_dense_variational(
    units = 128,
    batch_input_shape=list(NULL, as.integer(4)),
    make_posterior_fn = posterior_mean_field,
    make_prior_fn = prior_trainable,
    activation = "tanh"
  ) %>% tfprobability::layer_dense_variational(
    units = 1,
    make_posterior_fn = posterior_mean_field,
    make_prior_fn = prior_trainable,
  )  %>% tfprobability::layer_independent_bernoulli(
    event_shape = 1
  )

# use the prior bnn to make the target (balanced)
ps <- as.array(bnn(as.matrix(data_frame[2:5])) %>% tfd_mean())[,1]
data_frame$target_var <- ifelse(ps > 0.5, 1, 0)  #ifelse(ps > mean(ps), 1, 0)
mean(data_frame$target_var)



target = "target_var" 
data_frame[c(target)] <- data_frame[c(target)] %>% unlist() %>% as.factor()
levels_present <- levels(data_frame[c(target)] %>% unlist())
# check whether labels are suited for replacement by 0,1
levels_present
levels(data_frame[, which(names(data_frame) %in% target)]) <- c(0,1)



#train test splict
n_test = nrow(data_frame)*0.5
n_test = round(n_test)

test_rows = sample(nrow(data_frame), size = n_test)
test_data = data_frame[test_rows,]

# data frame for SSL
data = data_frame[-test_rows,]

# share of unlabeled obs
n_imp = (nrow(data) * share_unlabeled) %>% round()


# create data setup by randomly unlabelling data points
unlabeled_data_inst <- sample(nrow(data), n_imp)
labeled_data <- data[-unlabeled_data_inst,]
labeled_data <- cbind(labeled_data,nr = 0)
unlabeled_data <- data[unlabeled_data_inst,]
# add number
unlabeled_data <- cbind(unlabeled_data, nr = unlabeled_data_inst)
true_labels = cbind(unlabeled_data$nr, unlabeled_data[c(target)])


data_frame = data_frame %>% as.data.frame()
name_df = "simulated_easier" # for results 
data = "simulated_easier" # for results 