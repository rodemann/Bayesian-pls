###############
## global setup
###############
library(dplyr)
N = 400
#share_unlabeled = 0.8
n = nrow(mtcars)
p = 3
# read in data frame
data_frame = mtcars
name_df = "mtcars" # for results 
data = "mtcars"
# formula for glm
formula = vs ~1 + qsec + wt #+ mpg + drat + disp + hp

target = "vs" 
data_frame[c(target)] <- data_frame[c(target)] %>% unlist() %>% as.factor()
levels_present <- levels(data_frame[c(target)] %>% unlist())
# check whether labels are suited for replacement by 0,1
levels_present
levels(data_frame[, which(names(data_frame) %in% target)]) <- c(0,1)


#train test splict
n_test = nrow(data_frame)*0.5
n_test = round(n_test)



##########################
# source experiments files
##########################
path_to_experiments = paste(getwd(),"/benchmarks/experiments", sep = "")
# sequential sourcing
# miceadds::source.all(path_to_experiments) 


# parallel sourcing
files_to_source = list.files(path_to_experiments, pattern=".R",
                             full.names = TRUE)

#files_to_source = files_to_source[-c(3,4,5)]

num_cores <- parallel::detectCores() - 1
comp_clusters <- parallel::makeCluster(num_cores) # parallelize experiments
doParallel::registerDoParallel(comp_clusters)
object_vec = c("N", "share_unlabeled", "data_frame", "name_df", "formula", "target", "p", "n_test")
env <- environment()
parallel::clusterExport(cl=comp_clusters, varlist = object_vec, envir = env)
parallel::parSapply(comp_clusters, files_to_source, source)

parallel::stopCluster(comp_clusters)



