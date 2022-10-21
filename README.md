
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
In order to reproduce the papers' key results (and visualizations thereof) 

* source [benchmarking/experiments_simulated_data.R](benchmarking/experiments_simulated_data.R)  

Please find optional (currently commented out) visualizations in lines 118-159 of this very file. In order to rerun all simulations described in section 4 (PROBO on graphene data), please 

* source [this file](benchmarking/main-PROBO-benchmarking-graphene.R) to kick off the simulation study (estimated time on 64-bit-core (linux gnu): 11h)
* results are saved automatically
* source [this file](benchmarking/viz-glcb-all-comparisons-new.R) to visualize the retrieved results


### Data

Find data and files to read in data in folder [data](data). 



