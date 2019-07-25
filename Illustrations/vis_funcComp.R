library(dplyr)
library(ggplot2)
library(gridExtra) 
###
### dataLoading
### res_funcComp.data.csv
file <- file.choose()
data.file <- read.csv(file)
data.file$X <- NULL


#Illustrations displaying relations between observed psychometric function descriptors and noise, or scheme

#Threshold distance _noise_sd
plot_perf_th_noise_chunk1 <- ggplot(data=(data %>% filter(sigmoid_gen=="gumbel")), aes(x=sigmoid_fit, y=perf_th_err, col=as.factor(noise_sd), group=as.factor(noise_sd))) + 
  stat_summary(fun.data = "mean_cl_boot") + 
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(sigmoid_gen ~ .)  +
  scale_x_discrete(limits=c("gumbel","cauchy", "logistic", "weibull", "gauss")) + 
  theme_classic() +
  ylab(label="") +
  xlab(label="") 

plot_perf_th_noise_chunk2 <- ggplot(data=(data %>% filter(sigmoid_gen=="gauss")), aes(x=sigmoid_fit, y=perf_th_err, col=as.factor(noise_sd), group=as.factor(noise_sd))) + 
  stat_summary(fun.data = "mean_cl_boot") + 
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(sigmoid_gen ~ .)  +
  scale_x_discrete(limits=c( "gauss","gumbel","cauchy", "logistic", "weibull"))+ 
  theme_classic() +
  xlab(label=  "") +
  ylab(label = "")

plot_perf_th_noise_chunk3 <- ggplot(data=(data %>% filter(sigmoid_gen=="weibull")), aes(x=sigmoid_fit, y=perf_th_err, col=as.factor(noise_sd), group=as.factor(noise_sd))) + 
  stat_summary(fun.data = "mean_cl_boot") + 
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(sigmoid_gen ~ .)  +
  scale_x_discrete(limits=c( "weibull", "gauss", "gumbel","cauchy", "logistic")) + 
  theme_classic() +
  xlab(label="") +
  ylab(label = "Performance threshold error") 


plot_perf_th_noise_chunk4 <- ggplot(data=(data %>% filter(sigmoid_gen=="logistic")), aes(x=sigmoid_fit, y=perf_th_err, col=as.factor(noise_sd), group=as.factor(noise_sd))) + 
  stat_summary(fun.data = "mean_cl_boot") + 
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(sigmoid_gen ~ .)  +
  scale_x_discrete(limits=c( "logistic", "weibull", "gauss","gumbel","cauchy")) + 
  theme_classic() +
  ylab(label="")+
  xlab(label="") 

plot_perf_th_noise_chunk5 <- ggplot(data=(data %>% filter(sigmoid_gen=="cauchy")), aes(x=sigmoid_fit, y=perf_th_err, col=as.factor(noise_sd), group=as.factor(noise_sd))) + 
  stat_summary(fun.data = "mean_cl_boot") + 
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(sigmoid_gen ~ .)  +
  scale_x_discrete(limits=c("cauchy", "logistic", "weibull", "gauss", "gumbel")) + 
  theme_classic() +
  ylab(label="") +
  xlab(label="Psychometric function")

grid.arrange(plot_perf_th_noise_chunk1, plot_perf_th_noise_chunk2, plot_perf_th_noise_chunk3, plot_perf_th_noise_chunk4, plot_perf_th_noise_chunk5, nrow=5)

#perf_th sampling scheme
plot_perf_th_sch_chunk1 <- ggplot(data=(data %>% filter(sigmoid_gen=="gumbel")), aes(x=sigmoid_fit, y=perf_th_err, col=as.factor(id_predDistr), group=as.factor(id_predDistr))) + 
  stat_summary(fun.data = "mean_cl_boot") + 
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(sigmoid_gen ~ .)  +
  scale_x_discrete(limits=c("gumbel","cauchy", "logistic", "weibull", "gauss")) + 
  theme_classic() +
  ylab(label="") +
  xlab(label="")

plot_perf_th_sch_chunk2 <- ggplot(data=(data %>% filter(sigmoid_gen=="gauss")), aes(x=sigmoid_fit, y=perf_th_err, col=as.factor(id_predDistr), group=as.factor(id_predDistr))) + 
  stat_summary(fun.data = "mean_cl_boot") + 
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(sigmoid_gen ~ .)  +
  scale_x_discrete(limits=c( "gauss","gumbel","cauchy", "logistic", "weibull"))+ 
  theme_classic() +
  xlab(label=  "")+
  ylab("")

plot_perf_th_sch_chunk3 <- ggplot(data=(data %>% filter(sigmoid_gen=="weibull")), aes(x=sigmoid_fit, y=perf_th_err, col=as.factor(id_predDistr), group=as.factor(id_predDistr))) + 
  stat_summary(fun.data = "mean_cl_boot") + 
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(sigmoid_gen ~ .)  +
  scale_x_discrete(limits=c( "weibull", "gauss", "gumbel","cauchy", "logistic")) + 
  theme_classic() +
  ylab(label="") +
  xlab(label="")  +
  ylab(label = "Performance threshold error") 

plot_perf_th_sch_chunk4 <- ggplot(data=(data %>% filter(sigmoid_gen=="logistic")), aes(x=sigmoid_fit, y=perf_th_err, col=as.factor(id_predDistr), group=as.factor(id_predDistr))) + 
  stat_summary(fun.data = "mean_cl_boot") + 
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(sigmoid_gen ~ .)  +
  scale_x_discrete(limits=c( "logistic", "weibull", "gauss","gumbel","cauchy")) + 
  theme_classic() +
  ylab(label="") +
  xlab(label="")

plot_perf_th_sch_chunk5 <- ggplot(data=(data %>% filter(sigmoid_gen=="cauchy")), aes(x=sigmoid_fit, y=perf_th_err, col=as.factor(id_predDistr), group=as.factor(id_predDistr))) + 
  stat_summary(fun.data = "mean_cl_boot") + 
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(sigmoid_gen ~ .)  +
  scale_x_discrete(limits=c("cauchy", "logistic", "weibull", "gauss", "gumbel")) + 
  theme_classic() +
  ylab(label="") +
  xlab(label="Psychometric function") 

grid.arrange(plot_perf_th_sch_chunk1, plot_perf_th_sch_chunk2, plot_perf_th_sch_chunk3, plot_perf_th_sch_chunk4, plot_perf_th_sch_chunk5, nrow=5)

####IQR


plot_iqr_noise_chunk1 <- ggplot(data=(data %>% filter(sigmoid_gen=="gumbel") %>% filter(noise_sd < 0.04)), aes(x=sigmoid_fit, y=iqr_diff, col=as.factor(noise_sd), group=as.factor(noise_sd))) + 
  stat_summary(fun.data = "mean_cl_boot") + 
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(sigmoid_gen ~ .)  +
  scale_x_discrete(limits=c("gumbel","cauchy", "logistic", "weibull", "gauss")) +
  theme_classic() +
  ylab(label="") +
  xlab(label="") 

plot_iqr_noise_chunk2 <- ggplot(data=(data %>% filter(sigmoid_gen=="gauss") %>% filter(noise_sd < 0.04)), aes(x=sigmoid_fit, y=iqr_diff, col=as.factor(noise_sd), group=as.factor(noise_sd))) + 
  stat_summary(fun.data = "mean_cl_boot") + 
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(sigmoid_gen ~ .)  +
  scale_x_discrete(limits=c( "gauss","gumbel","cauchy", "logistic", "weibull"))+ 
  theme_classic() +
  xlab(label=  "") +
  ylab(label = "")

plot_iqr_noise_chunk3 <- ggplot(data=(data %>% filter(sigmoid_gen=="weibull") %>% filter(noise_sd < 0.04)), aes(x=sigmoid_fit, y=iqr_diff, col=as.factor(noise_sd), group=as.factor(noise_sd))) + 
  stat_summary(fun.data = "mean_cl_boot") + 
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(sigmoid_gen ~ .)  +
  scale_x_discrete(limits=c( "weibull", "gauss", "gumbel","cauchy", "logistic")) + 
  theme_classic() +
  xlab(label="") +
  ylab(label = "IQR error")


plot_iqr_noise_chunk4 <- ggplot(data=(data %>% filter(sigmoid_gen=="logistic") %>% filter(noise_sd < 0.04)), aes(x=sigmoid_fit, y=iqr_diff, col=as.factor(noise_sd), group=as.factor(noise_sd))) + 
  stat_summary(fun.data = "mean_cl_boot") + 
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(sigmoid_gen ~ .)  +
  scale_x_discrete(limits=c( "logistic", "weibull", "gauss","gumbel","cauchy")) + 
  theme_classic() +
  ylab(label="")+
  xlab(label="")

plot_iqr_noise_chunk5 <- ggplot(data=(data %>% filter(sigmoid_gen=="cauchy") %>% filter(noise_sd < 0.04)), aes(x=sigmoid_fit, y=iqr_diff, col=as.factor(noise_sd), group=as.factor(noise_sd))) + 
  stat_summary(fun.data = "mean_cl_boot") + 
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(sigmoid_gen ~ .)  +
  scale_x_discrete(limits=c("cauchy", "logistic", "weibull", "gauss", "gumbel")) + 
  theme_classic() +
  ylab(label="") +
  xlab(label="Psychometric function")

grid.arrange(plot_iqr_noise_chunk1, plot_iqr_noise_chunk2, plot_iqr_noise_chunk3, plot_iqr_noise_chunk4, plot_iqr_noise_chunk5, nrow=5)



####IQR _ sampling scheme

plot_iqr_sch_chunk1 <- ggplot(data=(data %>% filter(sigmoid_gen=="gumbel") %>% filter(noise_sd < 0.04)), aes(x=sigmoid_fit, y=iqr_diff, col=as.factor(id_predDistr), group=as.factor(id_predDistr))) + 
  stat_summary(fun.data = "mean_cl_boot") + 
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(sigmoid_gen ~ .)  +
  scale_x_discrete(limits=c("gumbel","cauchy", "logistic", "weibull", "gauss")) +
  theme_classic() +
  ylab(label="") +
  xlab(label="")

plot_iqr_sch_chunk2 <- ggplot(data=(data %>% filter(sigmoid_gen=="gauss") %>% filter(noise_sd < 0.04)), aes(x=sigmoid_fit, y=iqr_diff, col=as.factor(id_predDistr), group=as.factor(id_predDistr))) + 
  stat_summary(fun.data = "mean_cl_boot") + 
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(sigmoid_gen ~ .)  +
  scale_x_discrete(limits=c( "gauss","gumbel","cauchy", "logistic", "weibull"))+ 
  theme_classic() +
  xlab(label=  "") +
  ylab(label = "")

plot_iqr_sch_chunk3 <- ggplot(data=(data %>% filter(sigmoid_gen=="weibull") %>% filter(noise_sd < 0.04)), aes(x=sigmoid_fit, y=iqr_diff, col=as.factor(id_predDistr), group=as.factor(id_predDistr))) + 
  stat_summary(fun.data = "mean_cl_boot") + 
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(sigmoid_gen ~ .)  +
  scale_x_discrete(limits=c( "weibull", "gauss", "gumbel","cauchy", "logistic")) + 
  theme_classic() +
  xlab(label="") +
  ylab(label = "IQR error")


plot_iqr_sch_chunk4 <- ggplot(data=(data %>% filter(sigmoid_gen=="logistic") %>% filter(noise_sd < 0.04)), aes(x=sigmoid_fit, y=iqr_diff, col=as.factor(id_predDistr), group=as.factor(id_predDistr))) + 
  stat_summary(fun.data = "mean_cl_boot") + 
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(sigmoid_gen ~ .)  +
  scale_x_discrete(limits=c( "logistic", "weibull", "gauss","gumbel","cauchy")) + 
  theme_classic() +
  ylab(label="")+
  xlab(label="")

plot_iqr_sch_chunk5 <- ggplot(data=(data %>% filter(sigmoid_gen=="cauchy") %>% filter(noise_sd < 0.04)), aes(x=sigmoid_fit, y=iqr_diff, col=as.factor(id_predDistr), group=as.factor(id_predDistr))) + 
  stat_summary(fun.data = "mean_cl_boot") + 
  stat_summary(fun.y = "mean", geom = "line") +
  facet_grid(sigmoid_gen ~ .)  +
  scale_x_discrete(limits=c("cauchy", "logistic", "weibull", "gauss", "gumbel")) + 
  theme_classic() +
  ylab(label="") +
  xlab(label="Psychometric function")

grid.arrange(plot_iqr_sch_chunk1, plot_iqr_sch_chunk2, plot_iqr_sch_chunk3, plot_iqr_sch_chunk4, plot_iqr_sch_chunk5, nrow=5)

