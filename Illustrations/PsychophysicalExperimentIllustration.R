#Illustration Psychophysical experiment
library(PsyFuns)
library(ggplot2)

set.seed(16)
pfm1 <- PFm(PF("gauss", "al", 0.4,0.1,c(0.75,1)), predictor = c(0,1,1.5,2,3,4), observations = rep(30,6), type = "PC")
pfm2 <- PFm(PF("gauss", "al", 0.4,0.1,c(2.15,2)), predictor = c(0,1,1.5,2,3,4), observations = rep(30,6), type = "PC")

visualRange <- seq(-1,5,length.out = 100)
visualPC1 <- predict(pfm1,visualRange)
visualPoints1 <- noisedPredict(pfm1,pfm1$data$predictor, noiseDistr = "binomial")
visualPC2 <- predict(pfm2,visualRange)
visualPoints2 <- noisedPredict(pfm2,pfm2$data$predictor, noiseDistr = "binomial")

plot <- ggplot()
plot <- plot + geom_line(mapping =aes(x=visualRange,y=visualPC1), colour="green", linetype = "dashed")
plot <- plot + geom_point(mapping =aes(x=pfm1$data$predictor,y=visualPoints1), size=2.5, colour="darkgreen", fill="#55555500", shape=21, stroke=2)
plot <- plot + geom_point(mapping =aes(x=pfm1$perf_th,y=predict(pfm1, pfm1$perf_th)), size=4, colour="green", shape="triangle")
plot <- plot + geom_line(mapping =aes(x=visualRange,y=visualPC2), colour="red", linetype = "dashed")
plot <- plot + geom_point(mapping =aes(x=pfm2$data$predictor,y=visualPoints2), size=2.5, colour="darkred", fill="#55555500", shape=21, stroke=2)
plot <- plot + geom_point(mapping =aes(x=pfm2$perf_th,y=predict(pfm2, pfm2$perf_th)), size=4, colour="red", shape="triangle")
plot <- plot + ylim(0,1)
plot <- plot + scale_x_continuous(breaks = seq(0,4,1), labels = c(0,10,20,30,40))
plot <- plot + ylab("Proportion of \"Yes\" responses")
plot <- plot + xlab("Beep intensity [dB]")
plot <- plot + theme_classic(16)
plot <- plot + theme(aspect.ratio = 1)
plot

