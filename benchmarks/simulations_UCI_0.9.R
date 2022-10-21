


print(Sys.time()) 
try(
  source(paste(getwd(),"/benchmarks/run_benchmarks_abalone_0.9.R", sep=""))
)
try(
  source(paste(getwd(),"/analyze/analyze.R", sep=""))
)


print(Sys.time()) 
try(
  source(paste(getwd(),"/benchmarks/run_benchmarks_banknote_0.9.R", sep=""))
)
try(
  source(paste(getwd(),"/analyze/analyze.R", sep=""))
)


print(Sys.time()) 
try(
  source(paste(getwd(),"/benchmarks/run_benchmarks_ionoshere_0.9.R", sep=""))
)
try(
  source(paste(getwd(),"/analyze/analyze.R", sep=""))
)


print(Sys.time()) 
try(
  source(paste(getwd(),"/benchmarks/run_benchmarks_breast_cancer_0.9.R", sep=""))
)
try(
  source(paste(getwd(),"/analyze/analyze.R", sep=""))
)

