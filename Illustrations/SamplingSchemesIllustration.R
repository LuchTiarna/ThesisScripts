#Sampling schemes illustration
library(PsyFuns)
library(ggplot2)
library(dplyr)

samplings <- bind_rows(
  data.frame(predictor=c(7.09,  7.99,  8.68,  9.02,  9.71, 10.64), observations= 20, id=1, distr=1),
  data.frame(predictor=c(4.72,  7.09,  7.99,  9.71, 10.64, 13.21), observations= 20, id=2, distr=2), 
  data.frame(predictor=c(7.09,  8.34, 10.64, 11.72, 13.21, 15.76), observations= 20, id=3, distr=3),
  data.frame(predictor=c(4.72,  6.07,  7.09,  7.99,  8.85,  9.71), observations= 20, id=4, distr=4),
  data.frame(predictor=c(4.37,  5.83,  6.90, 10.64, 12.38, 16.64), observations= 20, id=5, distr=5),
  data.frame(predictor=c(7.09,  7.99,  8.85,  9.71, 10.64, 16.64), observations= 20, id=6, distr=6),
  data.frame(predictor=c(7.46,  8.34,  9.19, 11.72, 13.21, 15.76), observations= 20, id=7, distr=7)
)

func <- PF("exponential", "polynom", 0.5, 0.00, c(8.85/((log(2)^(1/3))), 3))
x_vis <- seq(3,20, length.out = 100)
y_vis <- predict(func, x_vis)

plot <- ggplot() +
  geom_line(mapping=aes(x=x_vis, y_vis), size=2, color="#555555FF", alpha=0.8) +
  geom_vline(xintercept = 8.85, color="grey", linetype = "dotdash") + geom_hline(yintercept = 0.75, color="grey", linetype = "dotdash") +
  geom_vline(xintercept = 11.15, color="grey", linetype = "dotdash") + geom_hline(yintercept = 0.875, color="grey", linetype = "dotdash") +
  geom_vline(xintercept =  6.6, color="grey", linetype = "dotdash") + geom_hline(yintercept = 0.625, color="grey", linetype = "dotdash") +
  geom_line(data=samplings,mapping=aes(x=predictor, y=0.49 * distr / max(distr), group=distr), alpha=0.8) +
  geom_point(data=samplings,mapping=aes(x=predictor, y=0.49 * distr / max(distr), shape=as.factor(distr)), fill='#AAAAAAFF',size=5) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5,0.6, 0.7, 0.8,0.9,1), limits = c(0,1)) +
  ylab("Proportion of correct responses") +
  xlab("Stimulus intensity (log)") +
  scale_x_continuous(breaks = c(4.5,5.5,7,9,11,14,18),trans="log") +
  scale_shape_manual(values = c(4,15,18,21,22,24,25), labels=c("s1","s2","s3","s4","s5","s6","s7"), name="Sampling scheme") +
  guides(shape = guide_legend(nrow = 1, byrow = T)) +
  theme_classic(16)  +
  theme(aspect.ratio = 1, legend.position = "bottom", legend.box = "vertical")
plot

