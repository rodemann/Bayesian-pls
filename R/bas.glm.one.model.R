
#' This is an edited version of bas.glm from {BAS}, see https://github.com/merliseclyde/BAS/blob/8c1576257d9ba81368ca920cac5ef331e603c6f9/R/bas_glm.R
#' Its edit allows to sample from one model only to approximate its log marginal likelihood
#' Bayesian Adaptive Sampling Without Replacement for Variable Selection in
#' Generalized Linear Models
#'
#' Sample with or without replacement from a posterior distribution on GLMs
#' 
#' 




bas.glm <- function(formula, family = binomial(link = "logit"),
                    data, weights, subset, contrasts=NULL, offset, na.action = "na.omit",
                    n.models = NULL,
                    betaprior = CCH(alpha = .5, beta = as.numeric(nrow(data)), s = 0),
                    modelprior = beta.binomial(1, 1),
                    initprobs = "Uniform",
                    include.always = ~1,
                    method = "MCMC",
                    update = NULL,
                    bestmodel = NULL,
                    prob.rw = 0.5,
                    MCMC.iterations = NULL, thin = 1,
                    control = glm.control(), laplace = FALSE, renormalize = FALSE,
                    force.heredity = FALSE,
                    bigmem = FALSE) {
  num.updates <- 10
  call <- match.call()
  
  if (is.character(family)) {
    family <- get(family, mode = "function", envir = parent.frame())
  }
  if (is.function(family)) {
    family <- family()
  }
  
  if (!(family$family %in% c("binomial", "poisson"))) {
    stop(paste("family ", family$family, "not implemented"))
  }
  if (missing(data)) {
    data <- environment(formula)
  }
  
  if (class(modelprior) != "prior") stop("modelprior should be an object of class prior,  uniform(),  beta.binomial(), etc")
  
  
  # browser()
  mfall <- match.call(expand.dots = FALSE)
  m <- match(c(
    "formula", "data", "subset", "weights", "na.action",
    "etastart", "mustart", "offset"
  ), names(mfall), 0L)
  mf <- mfall[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  n.NA <- length(attr(mf, "na.action"))
  
  if (n.NA > 0) {
    warning(paste(
      "dropping ", as.character(n.NA),
      "rows due to missing data"
    ))
  }
  
  Y <- model.response(mf, type = "any")
  mt <- attr(mf, "terms")
  X <- model.matrix(mt, mf, contrasts)
  # X = model.matrix(formula, mf)
  #    Y = glm.obj$y
  #    X = glm.obj$x
  namesx <- dimnames(X)[[2]]
  namesx[1] <- "Intercept"
  p <- dim(X)[2]
  nobs <- dim(X)[1]
  
  if (nobs == 0) {stop("Sample size is zero; check data and subset arguments")}
  #   weights = as.vector(model.weights(mf))
  
  weights <- as.vector(model.weights(mf))
  if (is.null(weights)) {
    weights <- rep(1, nobs)
  }
  
  offset <- model.offset(mf)
  if (is.null(offset)) offset <- rep(0, nobs)
  
  
  Y <-  glm(Y ~ 1, family = family, weights = weights,
            offset = offset, y = T)$y
  
  
  if (!is.numeric(initprobs)) {
    if (nobs <= p && initprobs == "eplogp") {
      stop(
        "Full model is not full rank so cannot use the eplogp bound to create starting sampling probabilities, perhpas use 'marg-eplogp' for fiting marginal models\n"
      )
    }
    initprobs <- switch(
      initprobs,
      "eplogp" = eplogprob(glm(Y ~ X - 1,
                               family = family, weights = weights,
                               offset = offset)),
      "marg-eplogp" = eplogprob.marg(Y, X),
      "uniform" = c(1.0, rep(.5, p - 1)),
      "Uniform" = c(1.0, rep(.5, p - 1))
    )
  }
  if (length(initprobs) == (p - 1)) {
    initprobs <- c(1.0, initprobs)
  }
  
  # set up variables to always include
  keep <- 1
  if ("include.always" %in% names(mfall)) {
    minc <- match(c("include.always", "data", "subset"), names(mfall), 0L)
    mfinc <- mfall[c(1L, minc)]
    mfinc$drop.unused.levels <- TRUE
    names(mfinc)[2] <- "formula"
    mfinc[[1L]] <- quote(stats::model.frame)
    mfinc <- eval(mfinc, parent.frame())
    mtinc <- attr(mfinc, "terms")
    X.always <- model.matrix(mtinc, mfinc, contrasts)
    
    keep <- c(1L, match(colnames(X.always)[-1], colnames(X)))
    initprobs[keep] <- 1.0
    if (ncol(X.always) == ncol(X)) {
      # just one model with all variables forced in
      # use method='BAS" as deterministic and MCMC fail in this context
      method <- "BAS"
    }
  }
  
  parents <- matrix(1, 1, 1)
  if (method == "deterministic" | method == "MCMC+BAS" ) force.heredity <- FALSE # not working yet
  if (force.heredity) {
    parents <- make.parents.of.interactions(mf, data)
    
    # check to see if really necessary
    if (sum(parents) == nrow(parents)) {
      parents <- matrix(1, 1, 1)
      force.heredity <- FALSE
    }
  }
  
  prob <- normalize.initprobs.lm(initprobs, p)
  
  if (is.null(bestmodel)) {
    #    bestmodel = as.integer(initprobs)
    bestmodel <- c(1, rep(0, p - 1))
  }
  bestmodel[keep] <- 1
  if (force.heredity) {
    update <- NULL # do not update tree  FIXME LATER
    if (prob.heredity(bestmodel, parents) == 0) {
      warning("bestmodel violates heredity conditions; resetting to null model.  Please check  include.always and bestmodel")
      bestmodel <- c(1, rep(0, p - 1))
    }
    #    initprobs <- c(1, seq(.95, .55, length = (p - 1))) # keep same order
  }
  
  bestmodel <- as.integer(bestmodel)
  
  
  if (is.null(n.models)) {
    n.models <- as.integer(min(2^p, 2^19))
  }
  
  n.models <- as.integer(normalize.n.models(n.models, p, prob, method, bigmem))
  
  modelprior <- normalize.modelprior(modelprior, p)
  
  
  if (is.null(MCMC.iterations)) {
    MCMC.iterations <- max(10000, (n.models * 10))
  }
  
  MCMC.iterations = as.integer(MCMC.iterations)
  Burnin.iterations <- as.integer(MCMC.iterations)
  
  
  modeldim <- as.integer(rep(0, n.models))
  
  
  
  
  
  #print(MCMC.iterations)
  
  
  if (is.null(update)) {
    if (force.heredity) {  # do not update tree for BAS
      update <- n.models + 1}
    else {
      if (n.models == 2^(p - 1)) {
        update <- n.models + 1
      } else {
        (update <- n.models / num.updates)
      }}
  }
  
  
  Yvec <- as.numeric(Y)
  
  
  
  #  check on priors
  
  if (class(betaprior) !="prior") stop("prior on coeeficients must be an object of type 'prior'")
  
  loglik_null <- as.numeric(-0.5 * glm(Y ~ 1,
                                       weights = weights,
                                       offset = offset,
                                       family = eval(call$family)
  )$null.deviance)
  
  betaprior$hyper.parameters$loglik_null <- loglik_null
  #  	browser()
  
  if (betaprior$family == "BIC" & is.null(betaprior$n)) {
    betaprior <- bic.prior(as.numeric(nobs))
  }
  
  
  if (betaprior$family == "hyper-g/n" & is.null(betaprior$n)) {
    betaprior$hyper.parameters$theta <- 1 / nobs
    betaprior$n <- nobs
  }
  if (betaprior$family == "robust" & is.null(betaprior$n)) betaprior <- robust(as.numeric(nobs))
  
  if (betaprior$family == "intrinsic" & is.null(betaprior$n)) {
    betaprior$hyper.parameters$n <- as.numeric(nobs)
  }
  
  if (betaprior$family == "betaprime" & is.null(betaprior$hyper.parameters$n)) {
    betaprior$hyper.parameters$n <- as.numeric(nobs)
  }
  
  #print(MCMC.iterations)
  result <- switch(method,
                   "MCMC" = .Call(C_glm_mcmc,
                                  Y = Yvec, X = X,
                                  Roffset = as.numeric(offset),
                                  Rweights = as.numeric(weights),
                                  Rprobinit = prob,
                                  Rmodeldim = modeldim,
                                  modelprior = modelprior,
                                  betaprior = betaprior,
                                  Rbestmodel = bestmodel,
                                  plocal = as.numeric(1.0 - prob.rw),
                                  BURNIN_Iterations = as.integer(MCMC.iterations),
                                  Rthin = as.integer(thin),
                                  family = family, Rcontrol = control,
                                  Rlaplace = as.integer(laplace),
                                  Rparents = parents
                   ),
                   "BAS" = .Call(C_glm_sampleworep,
                                 Y = Yvec, X = X,
                                 Roffset = as.numeric(offset),
                                 Rweights = as.numeric(weights),
                                 Rprobinit = prob,
                                 Rmodeldim = modeldim,
                                 modelprior = modelprior,
                                 betaprior = betaprior,
                                 Rbestmodel = bestmodel,
                                 plocal = as.numeric(1.0 - prob.rw),
                                 family = family, Rcontrol = control,
                                 Rupdate = as.integer(update),
                                 Rlaplace = as.integer(laplace),
                                 Rparents = parents
                   ),
                   "MCMC+BAS" = .Call(C_glm_mcmcbas,
                                      Y = Yvec,
                                      X = X,
                                      Roffset = as.numeric(offset),
                                      Rweights = as.numeric(weights),
                                      Rprobinit = prob,
                                      Rmodeldim = modeldim,
                                      modelprior = modelprior,
                                      betaprior = betaprior,
                                      Rbestmodel = bestmodel,
                                      plocal = as.numeric(1.0 - prob.rw),
                                      BURNIN_Iterations = as.integer(MCMC.iterations),
                                      family = family, Rcontrol = control,
                                      Rupdate = as.integer(update), Rlaplace = as.integer(laplace),
                                      Rparents = parents
                   ),
                   "deterministic" = .Call(C_glm_deterministic,
                                           Y = Yvec, X = X,
                                           Roffset = as.numeric(offset),
                                           Rweights = as.numeric(weights),
                                           Rprobinit = prob,
                                           Rmodeldim = modeldim,
                                           modelprior = modelprior,
                                           betaprior = betaprior,
                                           family = family,
                                           Rcontrol = control,
                                           Rlaplace = as.integer(laplace)
                   )
  )
  
  
  result$namesx <- namesx
  result$n <- length(Yvec)
  result$modelprior <- modelprior
  result$probne0[keep]  <- 1.0
  result$probne0.RN <- result$probne0
  result$postprobs.RN <- result$postprobs
  result$family <- family
  result$betaprior <- betaprior
  result$modelprior <- modelprior
  
  result$n.models <- length(result$postprobs)
  result$include.always <- keep
  
  # 	if (method == "MCMC") result$n.models = result$n.Unique
  
  
  df <- rep(nobs - 1, result$n.models)
  
  if (betaprior$class == "IC") df <- df - result$size + 1
  result$df <- df
  result$R2 <- .R2.glm.bas(result$deviance, result$size, call)
  result$n.vars <- p
  result$Y <- Yvec
  result$X <- X
  result$call <- call
  result$terms <- mt
  result$contrasts <- attr(X, "contrasts")
  result$xlevels <- .getXlevels(mt, mf)
  result$model <- mf
  
  # drop null model
  if (betaprior$family == "Jeffreys" && include.always == ~1) result <- .drop.null.bas(result)
  
  if (method == "MCMC") {
    result$postprobs.MCMC <- result$freq / sum(result$freq)
    if (!renormalize) {
      result$probne0 <- result$probne0.MCMC
      result$postprobs <- result$postprobs.MCMC
    }
  }
  
  class(result) <- c("basglm", "bas")
  return(result)
}

# Drop the null model from Jeffrey's prior

.drop.null.bas <- function(object) {
  n.models <- object$n.models
  
  
  p <- object$size
  drop <- (1:n.models)[p == 1]
  logmarg <- object$logmarg[-drop]
  prior <- object$priorprobs[-drop]
  
  postprobs <- .renormalize.postprobs(logmarg, log(prior))
  which <- which.matrix(object$which[-drop], object$n.var)
  
  object$probne0 <- as.vector(postprobs %*% which)
  object$postprobs <- postprobs
  
  method <- eval(object$call$method)
  if (method == "MCMC+BAS" | method == "MCMC") {
    object$freq <- object$freq[-drop]
    object$probne0.MCMC <- as.vector(object$freq %*% which)/sum(object$freq)
  }
  
  object$priorprobs <- prior
  if (!is.null(object$sampleprobs)) object$sampleprobs <- object$sampleprobs[-drop]
  object$which <- object$which[-drop]
  object$logmarg <- logmarg
  object$deviance <- object$deviance[-drop]
  object$intercept <- object$intercept[-drop]
  object$size <- object$size[-drop]
  object$Q <- object$Q[-drop]
  object$R2 <- object$R2[-drop]
  object$mle <- object$mle[-drop]
  object$mle.se <- object$mle.se[-drop]
  object$shrinkage <- object$shrinkage[-drop]
  object$n.models <- n.models - 1
  object$df <- object$df[-drop]
  return(object)
}

.renormalize.postprobs <- function(logmarg, logprior) {
  probs <- logmarg + logprior
  probs <- exp(probs - max(probs))
  probs <- probs / sum(probs)
  return(probs)
}

.R2.glm.bas <- function(deviance, size, call) {
  n.models <- length(deviance)
  null.model <- (1:n.models)[size == 1]
  if (is.null(null.model)) {
    null.deviance <- glm(eval(call$formula),
                         data = eval(call$data),
                         family = eval(call$family)
    )$null.deviance
  }
  else {
    null.deviance <- deviance[null.model]
  }
  
  R2 <- 1 - deviance / null.deviance
  return(R2)
}


normalize.initprobs.lm <- function (initprobs, p) 
{
  if (length(initprobs) != p) {
    stop(paste("length of initprobs is", length(initprobs), 
               "is not same as dimensions of X", p))
  }
  if (initprobs[1] < 1 | initprobs[1] > 1) 
    initprobs[1] <- 1
  prob <- as.numeric(initprobs)
  prob[initprobs < 0] = 0
  prob[initprobs > 1] = 1
  return(prob)
}



normalize.initprobs.glm <- function(initprobs, glm.obj) {
  p <- dim(glm.obj$x)[2]
  if (!is.numeric(initprobs)) {
    initprobs <- switch(initprobs,
                        "eplogp" = eplogprob(glm.obj),
                        "uniform" = c(1.0, rep(.5, p - 1)),
                        "Uniform" = c(1.0, rep(.5, p - 1))
    )
  }
  if (length(initprobs) == (p - 1)) {
    initprobs <- c(1.0, initprobs)
  }
  if (length(initprobs) != p) {
    stop(paste("length of initprobs is not", p))
  }
  if (initprobs[1] < 1.0 | initprobs[1] > 1.0) initprobs[1] <- 1.0
  # intercept is always included otherwise we get a segmentation
  # fault (relax later)
  prob <- as.numeric(initprobs)
  
  pval <- summary(glm.obj)$coefficients[, 4]
  if (any(is.na(pval))) {
    warning(paste("warning full model is rank deficient; use caution when interpreting restults."))
    #   prob[is.na(pval)] <- 0.0
  }
  
  return(prob)
}


# assure that the function will be able to call other hidden functions from the package. 
assignInNamespace("bas.glm", bas.glm, ns = "BAS")
environment(bas.glm) <- asNamespace('BAS')




