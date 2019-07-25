library(PsyFuns)
library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(DEoptim)
#library(parallel)
#library(foreach)
#library(doParallel)

### Fitting functions
fitters <- list(
  list("gauss", "ab", c(0.5))
)

###
### dataLoading
###
file <- file.choose()
data <- read.csv(file)
data$X <- NULL

#needs additional yes-column


#spliting for every simulation
cat("Start of computation at:", " ", date(), "\n" , sep="")
data_list <- split(data, data$id_sim)

###
### processing Functions
###
fitMultiple <- function(func, dataList){
  fitdef <- function(d){
    pfm <- fitPFm(c(PC_gen, observations)~predictor, data = d, sigmoid = func[[1]], core = func[[2]],
                  control=list(maxit=10000), type="PC", gamma = func[[3]], algorithm = def_fixed_gamma)
    pfm$data$algorithm <- "def"
    return(pfm)
  }

  fit_heu <- function(d){
    pfm <- fitPFm(c(PC_gen, observations)~predictor, data = d, sigmoid = func[[1]], core = func[[2]],
                  control=list(maxit=10000), type="PC", gamma = func[[3]], algorithm = heu_fixed_gamma)
    pfm$data$algorithm <- "heu"
    return(pfm)
  }

  fit_deoptim <- function(d){
    pfm <- fitPFm(c(PC_gen, observations)~predictor, data = d, sigmoid = func[[1]], core = func[[2]],
                  control=DEoptim.control(itermax = 500, trace=FALSE), type="PC", gamma = func[[3]], algorithm = deoptim_fixed_gamma)
    pfm$data$algorithm <- "PsyFuns_deoptim"
    return(pfm)
  }

  cat("default algorighm fitting starts at:", " ", date(), "\n", sep="" )
  res <- lapply(dataList, fitdef)
  cat("algorighm with heuristics fitting starts at:", " ", date(), "\n", sep="" )
  res2 <- lapply(dataList, fit_heu)
  cat("deoptim algorithm starts at:", " ", date(), "\n", sep="" )
  res3 <- lapply(dataList, fit_deoptim)
  cat("deoptim end at:", " ", date(), "\n", sep="" )
  #
  return(c(res,res2,res3))
}

addFitID <- function(pfm, id){
  pfm$id_fit <- id
  pfm$data$id_fit <- pfm$id_fit
  pfm$data$sigmoid_fit <- pfm$sigmoid
  pfm$data$core_fit <- pfm$core
  pfm$data$gamma_fit <- pfm$gamma
  pfm$data$lambda_fit <- pfm$lambda
  pfm$data$params.1_fit <- pfm$params[1]
  pfm$data$params.2_fit <- pfm$params[2]
  pfm$data$lh_value <- pfm$value
  pfm$data$PC_fit <- predict(pfm, pfm$data$predictor)
  pfm$data$perf_th_fit <- pfm$perf_th
  pfm$data$imp_th_fit <- pfm$imp_th
  pfm$data$d_th_fit <- pfm$d_th
  pfm$data$iqr_fit <- pfm$iqr
  pfm$data$width_fit <- pfm$w
  pfm$data$log_likelihood_fit <- pfm$log_likelihood
  pfm$data$log_likelihood_ratio_fit <- pfm$log_likelihood_ratio
  pfm$data$pearson_x_fit <- pfm$pearson_x
  pfm$data$mse_fit <- pfm$mse

  return(pfm$data)
}


### Fitting
#stores number of fitted Data, so ID are unique
idFits <- 1
#names of files the data will be temporarily stored in
names <- c()
#registerDoParallel(detectCores()-1)
cat("Start of fitting at:", " ", date(), "\n" , sep="")

set.seed(2900)

fitted <- unlist(mapply(fitMultiple, fitters, MoreArgs=list(dataList=data_list), SIMPLIFY = FALSE), recursive = FALSE)

#creating IDs
ids <- as.list(idFits:(length(fitted)+idFits-1))
idFits <- length(fitted)+idFits

fitted_data.frame <- mapply(addFitID, fitted, ids, SIMPLIFY = FALSE)
fitted_data.frame <-bind_rows(fitted_data.frame)
cat("End of data fitting at:", " ", date(), "\n", sep="")

write.csv(fitted_data.frame, file="fit_sc_obs.data.csv")
cat("End of computation at:", " ", date(), "\n", sep="")

rm(fitted, fitters, file, data, data_list)
rm(fitMultiple, addFitID)
rm(names, idFits, ids)
#rm(fitted_data.frame)
