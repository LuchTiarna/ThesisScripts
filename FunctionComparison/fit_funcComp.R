library(PsyFuns)
library(dplyr)
library(ggplot2)
library(tidyr)
#library(parallel)
#library(foreach)
#library(doParallel)

### Fitting functions
fitters <- list(
  list("exponential", "polynom", 0.5),
  list("gauss", "ab", 0.5),
  list("logistic", "ab", 0.5),
  list("cauchy", "al", 0.5),
  list("gumbel_l", "al", 0.5)
)

###
### dataLoading
###
file <- file.choose()
data <- read.csv(file)
data$X <- NULL

#spliting for every simulation
cat("Start of computation at:", " ", date(), "\n" , sep="")
data_list <- split(data, data$id_sim)

#spliting for to smaller chuncs for more stable computation
cap <- 2000
data_list <- split(data_list, trunc((1:length(data_list))/cap))


###
### processing Functions
###
fitMultiple <- function(func, dataList){
  fit <- function(d){
    return(fitPFm(c(PC_gen, observations)~predictor, data = d ,sigmoid = func[[1]], core = func[[2]], 
                  control=list(maxit=10000), type="PC", gamma = func[[3]], algorithm = heu_fixed_gamma))
  }
  res <- lapply(dataList, fit)
  
  return(res)
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
for(d in 1:length(data_list)){
  cat("beginning processing of data chunk", " ", d, " ", "at:", " ", date(), "\n", sep="" )  
  set.seed(2900)  
  dd <- data_list[[d]]
  
  fitted <- unlist(mapply(fitMultiple, fitters, MoreArgs=list(dataList=dd), SIMPLIFY = FALSE), recursive = FALSE)
  
  #creating IDs
  ids <- as.list(idFits:(length(fitted)+idFits-1))
  idFits <- length(fitted)+idFits
  
  fitted_data.frame <- mapply(addFitID, fitted, ids, SIMPLIFY = FALSE)
  fitted_data.frame <-bind_rows(fitted_data.frame)
  
  name <- paste("chunk_", d, ".data.csv",sep ="")
  names <- c(names, name)
  write.csv(fitted_data.frame, file=name)
  cat("Data chunk", " ", d, " ", "written  to file at:", " ", date(), "\n", sep="" )
}
cat("End of data fitting at:", " ", date(), "\n", sep="")

#Rewriting data to one file
fitted_data.frame <- read.csv(names[1])
for(name in names[-1]){
  fitted_data.frame_chunk <- read.csv(name)
  fitted_data.frame <- bind_rows(fitted_data.frame, fitted_data.frame_chunk)
}
write.csv(fitted_data.frame, file="fit_funcComp.data.csv")
cat("End of computation at:", " ", date(), "\n", sep="")