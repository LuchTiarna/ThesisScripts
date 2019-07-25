library(dplyr)
library(ggplot2)

###
### load file 
### fit_funcCom.data.csv
file <- file.choose()
data.file <- read.csv(file)
data.file$X <- NULL

#fitering individual experiments
data <- data.file %>% select(sigmoid_gen, core_gen, sigmoid_fit, core_fit,
                                  gamma_gen, lambda_gen, params.1_gen, params.2_gen,
                                  gamma_fit, lambda_fit, params.1_fit, params.2_fit,
                                  noiseType, noise_sd, noise_mean, observations,
                                  perf_th, perf_th_fit,
                                  iqr, iqr_fit, width, width_fit,
                                  log_lik_orig,
                                  log_likelihood_gen_noised, log_likelihood_fit,
                                  log_likelihood_ratio_gen_noised, log_likelihood_ratio_fit,
                                  pearson_x_gen_noised, pearson_x_fit, mse_gen_noised, mse_fit,
                                  id_gen, id_pred, id_sim, id_predDistr, id_fit) %>% unique()


data <- data %>% mutate(iqr_diff=abs(iqr_fit - iqr))
data <- data %>% mutate(perf_th_err=abs(perf_th_fit - perf_th))
data <- data %>% mutate(lambda_err=abs(lambda_fit - lambda_gen))
data <- data %>% mutate(difftype=paste(sigmoid_gen, sigmoid_fit, sep="_"))

#renaming functions 
data <- data %>% mutate(sigmoid_gen=as.character(sigmoid_gen),sigmoid_fit=as.character(sigmoid_fit))
data <- data %>% mutate(sigmoid_gen=ifelse(sigmoid_gen=="exponential", "weibull", sigmoid_gen))
data <- data %>% mutate(sigmoid_gen=ifelse(sigmoid_gen=="gumbel_l", "gumbel", sigmoid_gen))
data <- data %>% mutate(sigmoid_fit=ifelse(sigmoid_fit=="exponential", "weibull", sigmoid_fit))
data <- data %>% mutate(sigmoid_fit=ifelse(sigmoid_fit=="gumbel_l", "gumbel", sigmoid_fit))
write.csv(data, "res_func.data.csv")

#ANOVA for MSE to check if there is any difference in ability to fit among different function types
#ANOVA 
data_anova_fun_noise <- data %>% group_by(sigmoid_fit) %>% aov(mse_fit ~sigmoid_fit, .)
data_anova_fun_noise %>% summary()

data_anova_fun_distr <- data %>% group_by(sigmoid_fit, id_predDistr) %>% aov(mse_fit ~noise_sd + id_predDistr, .) 
data_anova_fun_distr %>% summary()

#ANOVA for performance threshold distance
#ANOVA 
data_anova_noise <- data %>% group_by(sigmoid_fit) %>% aov(perf_th_err ~sigmoid_fit, .)
data_anova_noise %>% summary()

anova_perf_th <- data %>% group_by(sigmoid_gen) %>% do(aov(perf_th_err ~ sigmoid_fit*noise_sd, .) %>% car::Anova() )
anova_perf_th <- data %>% group_by(sigmoid_gen) %>% do(aov(perf_th_err ~ sigmoid_fit*id_predDistr, .) %>% car::Anova() )

#multifactor ANOVA iqr
anova_iqr <- data %>% group_by(sigmoid_gen) %>% do(aov(iqr_fit ~ sigmoid_fit*noise_sd, .) %>% car::Anova() )
anova_iqr <- data %>% group_by(sigmoid_gen) %>% do(aov(iqr_diff ~ sigmoid_fit*noise_sd, .) %>% car::Anova())

anova_iqr <- data %>% filter(0.04 > noise_sd) %>% group_by(sigmoid_gen) %>% do(aov(iqr_fit ~ sigmoid_fit*noise_sd, .) %>% car::Anova() )
anova_iqr <- data %>% filter(0.04 > noise_sd) %>% group_by(sigmoid_gen) %>% do(aov(iqr_diff ~ sigmoid_fit*id_predDistr, .) %>% car::Anova())

