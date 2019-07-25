library(dplyr)
library(ggplot2)
library(gridExtra)

###
### dataLoading
### fit_sc_noise.data.csv
file <- file.choose()
dataFile <- read.csv(file)
dataFile$X <- NULL

#fitering individual experiments
data <- dataFile %>% select(sigmoid_gen, core_gen, sigmoid_fit, core_fit,
                            gamma_gen, lambda_gen, params.1_gen, params.2_gen,
                            gamma_fit, lambda_fit, params.1_fit, params.2_fit,
                            noiseType, noise_sd, noise_mean, observations,
                            perf_th, perf_th_fit,
                            iqr, iqr_fit, width, width_fit,
                            log_lik_orig,
                            log_likelihood_gen_noised, log_likelihood_fit,
                            log_likelihood_ratio_gen_noised, log_likelihood_ratio_fit,
                            pearson_x_gen_noised, pearson_x_fit, mse_gen_noised, mse_fit,
                            id_gen, id_pred, id_sim, id_predDistr, id_fit, algorithm) %>% unique()


write.csv(data,"res_sc_noise.data.csv")

data_anova_noise <- data  %>% aov(mse_fit ~noise_sd, .) %>% summary()

visual_noise <- ggplot(data=data, mapping=aes(x=noise_sd, y=mse_fit)) + 
  stat_summary(fun.data = "mean_cl_boot") + 
  stat_summary(fun.y = "mean", geom = "line")  + 
  ylab(label="MSE")+ 
  xlab(label="noise deviation") +
  theme_classic()
visual_noise

