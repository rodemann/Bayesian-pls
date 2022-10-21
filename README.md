
# *Bayesian(,) pl(ea)s(e)!*

## Bayesian Pseudo-Label Selection (Bayesian-pls) 



### Introduction, TOC
This anonymous repository contains code for Selecting Pseudo-Labels the Bayesian way, as introduced in the paper "Bayes Optimal Pseudo-Label Selection for Semi-Supervised Learning"

* [R](R) contains implementation of BPLS with PPP and alternative PLS methods to benchmark against
* [benchmarking](benchmarking) provides files for experiments (section 4), in order to reproduce results, see setup below
* [data](data) contains real-world data used in experiments
* experimental results and visualization thereof will be saved in [plots](plots) and [results](results) 




### Tested with

- R 4.2.0
- R 4.1.6
- R 4.0.3

on
- Linux Ubuntu 20.04
- Linux Debian 10
- Windows 11 Pro Build 22H2 




### Setup

First and foremost, please install all dependencies by sourcing [this file](_setup_session.R).

Then download the implementations of BPLS with PPP and concurring PLS methods and save in a folder named "R":

* [Supervised Baseline](R/standard_supervised.R)
* [Probability Score](R/standard_self_training_conf.R)
* [Predictive Variance](R/standard_self_training.R)
* [PPP (Bayes-optimal)](R/diff_marg_likelihood_pred_ext.R)
* [Likelihood (max-max)](R/diff_marg_likelihood_pred.R)
* [Utilities for PPP](R/utils_diff_marg_likelihood.R)



In order to reproduce the papers' key results (and visualizations thereof) further download these scripts and save in respective folder:

* [global setup of experiments](benchmarks/run_benchmarks_simulated_data_p=60.R)
* [analyze/analyze.R](analyze/analyze.R)
* [experiments with likelihood (max-max)](benchmarks/experiments/benchmark-dml-pred.R)
* [experiments with PPP (bayes-opt)](benchmarks/experiments/benchmark-dml-pred-ext.R)
* [experiments with supervised baseline](benchmarks/experiments/_benchmark-standard-supervised.R)
* [experiments with predictive variance](benchmarks/experiments/_benchmark-standard-self-training.R)
* [experiments with probability score](benchmarking/experiments/_benchmark-standard-self-training_conf.R)


Eventually, download and source [benchmarking/experiments_simulated_data.R](benchmarking/experiments_simulated_data.R) (estimated runtime: 30 CPU hours)

Both [results](results) and [plots](plots) will be stored automatically. In addition, you can access them as object after completion of the experiments.

### Data

Find data and files to read in data in folder [data](data). 



