###############
## global setup
###############
library(dplyr)
#N = 10
#share_unlabeled = 0.8
set.seed(2138720)

# simulate data
#n = 70
feature_1 <- rnorm(n, mean = 0.2)
feature_2 <- rnorm(n, mean = -2)
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
feature_26 <- rnorm(n, mean = 20, sd = 40)
feature_27 <- rnorm(n, mean = 0, sd = 2)
feature_28 <- rnorm(n, mean = 4, sd = 18)
feature_29 <- rnorm(n, mean = 6, sd = 30)
feature_30 <- rnorm(n, mean = -1, sd = 11)

lin_comb <- 2.4- 7.9*feature_1 #+ 1.9*feature_2 + 0.5 * feature_3 + feature_4
prob = 1/(1+exp(-lin_comb))
target_var <-rbinom(n, 1, prob = prob)
sum(target_var)
data_frame <- data_frame(target_var = target_var, feature_1 = feature_1, feature_2 = feature_2,
                         feature_3 = feature_3, feature_4 = feature_4, feature_5 = feature_5,
                         feature_6 = feature_6, feature_7 = feature_7, feature_8 = feature_8,
                         feature_9 = feature_9, feature_10 = feature_10, feature_11 = feature_11,
                         feature_12 = feature_12, feature_13 = feature_13, feature_14 = feature_14,
                         feature_15 = feature_15, feature_16 = feature_16, feature_17 = feature_17,
                         feature_18 = feature_18, feature_19 = feature_19, feature_20 = feature_20,
                         feature_21 = feature_21, feature_22 = feature_22, feature_23 = feature_23,
                         feature_24 = feature_24, feature_25 = feature_25, feature_26 = feature_26,
                         feature_27 = feature_27, feature_28 = feature_28, feature_29 = feature_29,
                         feature_30 = feature_30)

formula <- target_var ~ .
glm(formula = formula, data = data_frame, family = "binomial") %>% summary()

data_frame = data_frame %>% as.data.frame()
name_df = "simulated_easier" # for results 
data = "simulated_easier" # for results 

# formula for glm
formula = target_var~ 1 + feature_1 + feature_2 + feature_3 + feature_4 + feature_5 + feature_6 +
  feature_7 + feature_8 + feature_9 + feature_10 + feature_11 + feature_12 + feature_13 +
feature_14 + feature_15 + feature_16 + feature_17 + feature_18 + feature_19 + feature_20 +
feature_21 + feature_22 + feature_23 + feature_24 + feature_25 + feature_26 + feature_27 +
feature_28 + feature_29 + feature_30

summary <- glm(formula = formula, data = data_frame, family = "binomial") %>% summary()
p <- summary$coefficients %>% nrow() - 1

target = "target_var" 
data_frame[c(target)] <- data_frame[c(target)] %>% unlist() %>% as.factor()
levels_present <- levels(data_frame[c(target)] %>% unlist())
# check whether labels are suited for replacement by 0,1
levels_present
levels(data_frame[, which(names(data_frame) %in% target)]) <- c(0,1)

##########################
# source experiments files
##########################
path_to_experiments = paste(getwd(),"/benchmarks/experiments", sep = "")
# sequential sourcing
# miceadds::source.all(path_to_experiments) 


# parallel sourcing
files_to_source = list.files(path_to_experiments, pattern=".R",
                             full.names = TRUE)
#files_to_source = files_to_source[-c(4)]

num_cores <- parallel::detectCores() - 1
comp_clusters <- parallel::makeCluster(num_cores) # parallelize experiments
doParallel::registerDoParallel(comp_clusters)
object_vec = c("N", "share_unlabeled", "data_frame", "name_df", "formula", "target", "p")
env <- environment()
parallel::clusterExport(cl=comp_clusters, varlist = object_vec, envir = env)
parallel::parSapply(comp_clusters, files_to_source, source)

parallel::stopCluster(comp_clusters)



