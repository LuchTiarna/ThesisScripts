library(PsyFuns)
library(dplyr)
library(ggplot2)
library(tidyr)

#Setting random seed
set.seed(1234)

###
### Simulation parameters
###
setsOfPredictors <- list(
  data.frame(predictor=seq(0.5,6.5,length.out = 320),   observations= 80,      id=4, distr=4),
  data.frame(predictor=seq(0.5,6.5,length.out = 320),   observations= 160,     id=5, distr=5)
)

#Data generating functions + inner parameters a,b
functions <- list(
  list("gauss", "ab", c(3.5,3/2.326348))
)
## Guessing and lapse rate
Params_Guess_Lapse_rate <- list(
  c(0.5,0.075),
  c(0.5,0.05),
  c(0.5,0.025),
  c(0.5,0.01),
  c(0.5,0)
)


#noise type and parameters
noise <- list(
  list("normal", 0, 0.01),
  list("normal", 0, 0.02),
  list("normal", 0, 0.03),
  list("normal", 0, 0.04),
  list("normal", 0, 0.05)
)

# NOS (nuber of simulations) per one generating function + predicting stimulus + noise properties
NOS <- 5
### --- ###

###
### Creating generating function representations
###

cat("Preparation of function representations beginning:", " ", date(), "\n", sep="")

#combining all parameters needet to specify function properties
generators <- expand.grid(functions, Params_Guess_Lapse_rate, setsOfPredictors) 

#Constructiong representation of PFm model based on given parameters
createPFmodel <- function(pf, gl, gen){
  pf <- PF(sigmoid=pf[[1]], core=pf[[2]], gamma=gl[1],lambda=gl[2], params=pf[[3]])
  pfm <- PFm(pf, gen$predictor, gen$observations, type="PC")
  pfm$id_pred <- gen$id
  pfm$id_predDistr <- gen$distr
  return(pfm)
}


generators <- mapply(createPFmodel, generators[,1], generators[,2], generators[,3], SIMPLIFY = FALSE)

#Preparing data for later transfer, adding ids for better identification
ids <- 1:length(generators)
ids <- as.list(ids)
PFmToPFmTable <- function(pfm, id){
  pfm$id_gen <- id
  
  pfm$data$id_gen <- pfm$id_gen
  pfm$data$sigmoid_gen <- rep(pfm$sigmoid, nrow(pfm$data))
  pfm$data$core_gen <- rep(pfm$core, nrow(pfm$data))
  pfm$data$gamma_gen <- rep(pfm$gamma, nrow(pfm$data))
  pfm$data$lambda_gen <- rep(pfm$lambda, nrow(pfm$data))
  pfm$data$params.1_gen <- rep(pfm$params[1], nrow(pfm$data))
  pfm$data$params.2_gen <- rep(pfm$params[2], nrow(pfm$data))
  pfm$data$id_predDistr <- pfm$id_predDistr
  pfm$data$id_pred <- pfm$id_pred
  pfm$data$perf_th <- pfm$perf_th
  pfm$data$imp_th <- pfm$imp_th
  pfm$data$d_th <- pfm$d_th
  pfm$data$iqr <- pfm$iqr
  pfm$data$width <- pfm$w
  pfm$data$log_lik_orig <- pfm$log_likelihood
  return(pfm)
  
}

generators <- mapply(PFmToPFmTable, generators, ids, SIMPLIFY = FALSE)
### --- ###

###
### Noised simulations 
###
cat("Simulation start at:", " ", date(), "\n", sep="")

#adding noise parameters
gensnoise <- expand.grid(noise,generators)

adding_noise <- function(pfm, noise){
  pfm$noise <- TRUE
  pfm$noiseParams <- noise
  return(pfm)
}


generated <- mapply(adding_noise, gensnoise[,2], gensnoise[,1], SIMPLIFY = FALSE)

#Simulating data + preparing data for later use
generating_simulations <- function(pfm){
  list <- rep(list(pfm), NOS)
  
  gen <- function(pfm){
    pfm$data$PC_gen <- PsyFuns::noisedPredict(pfm, pfm$data$predictor,   unlist(pfm$noiseParams[1]), unlist(pfm$noiseParams[2]), unlist(pfm$noiseParams[3])) 
    pfm$data$PC_gen <- round(pfm$data$PC_gen * pfm$data$observations) / pfm$data$observations #rounding
    pfm$formula <- as.formula(c(PC_gen, observations) ~ predictor)
    pfm$data$noiseType <- unlist(pfm$noiseParams[1])
    pfm$data$noise_mean <- as.factor(unlist(pfm$noiseParams[2]))
    pfm$data$noise_sd <- as.factor(unlist(pfm$noiseParams[3]))
    pfm$data$log_likelihood_gen_noised <- PsyFuns:::log_likelihood(pfm)
    pfm$data$log_likelihood_ratio_gen_noised <- PsyFuns:::log_likRatio(pfm)
    pfm$data$pearson_x_gen_noised <- PsyFuns:::pearsonx(pfm)
    pfm$data$mse_gen_noised <- PsyFuns:::mse(pfm)
    
    return(pfm)
  }
  
  list <- lapply(list, gen)
  
  return(list)
}

generated <- unlist(lapply(generated, generating_simulations), recursive = FALSE)


### --- ###

###
### Final data processing
###
#Adding original id to every simulation for later manipulation
ids <- 1:length(generated)
ids <- as.list(ids) 

addSimultationID <- function(pfm, id){
  pfm$id_sim <- id
  pfm$data$id_sim <- pfm$id_sim
  return(pfm)
}

generated <- mapply(addSimultationID, generated, ids, SIMPLIFY = FALSE)

#Converting to data.frame
PFmTableToData.Frame <- function(pfm){
  return(pfm$data)
}

generated_data.frame <- lapply(generated, PFmTableToData.Frame)
generated_data.frame <- bind_rows(generated_data.frame)


###saving data to file
write.csv(generated_data.frame, "gen_sc_noise.data.csv")
cat("End of computation at:", " ", date(), "\n", sep="")

rm(setsOfPredictors, functions, Params_Guess_Lapse_rate, noise, NOS)
rm(PFmToPFmTable, addSimultationID, PFmTableToData.Frame, generating_simulations, adding_noise, createPFmodel)
rm(generators,ids,generated, gensnoise)
#rm(generated_data.frame)
