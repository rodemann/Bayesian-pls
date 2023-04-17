
source("R/global_setup.R")
source("R/bnn_utils.R")



set.seed(3405934)

n_imp = ((nrow(data_frame) - n_test) * share_unlabeled) %>% round()
ind_res_on_the_fly = matrix(nrow = n_imp, ncol = N)

test_rows = sample(nrow(data_frame), size = n_test)
test_data = data_frame[test_rows,]
data = data_frame[-test_rows,]
unlabeled_data_inst <- sample(nrow(data), n_imp)
labeled_data <- data[-unlabeled_data_inst,]
labeled_data <- cbind(labeled_data,nr = 0)
unlabeled_data <- data[unlabeled_data_inst,]
unlabeled_data <- cbind(unlabeled_data, nr = unlabeled_data_inst)
true_labels = cbind(unlabeled_data$nr, unlabeled_data[c(target)])
  

x_l <- as.matrix(labeled_data[, 2:(length(labeled_data)-1)])
y_l <- as.matrix(as.double(labeled_data[, 1])) - 1


# get a random bnn as a prior
get_model(NULL, NULL)$save_weights("bnn_prior.h5")

# to create the initial pre-trained DNN, using n=40000 in entry_bnn.R
# instructions
#  1. set n=4000 in global_setup.R
#  2. uncomment line 77 in global_setup.R
#  3. remove/rename the file bnn_pretrained.h5 to do pretraining
#  4. stop, undo, rerun
bnn <- get_model(x_l, y_l, save_fname = "bnn_pretrained.h5", validation_split = 0.2, epochs = 700)
