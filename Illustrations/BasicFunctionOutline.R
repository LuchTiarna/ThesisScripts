library(ggplot2)
library(PsyFuns)
##plain plot

pfm1 <- PFm(PF("gauss", "al", 0.4,0.1,c(1,1)), predictor = c(0,1,1.5,2,3,4), observations = rep(30,6), type = "PC")
pfm2 <- PFm(PF("gauss", "al", 0.4,0.1,c(1,1/2)), predictor = c(0,1,1.5,2,3,4), observations = rep(30,6), type = "PC")
pfm3 <- PFm(PF("gauss", "al", 0.4,0.1,c(1,2)), predictor = c(0,1,1.5,2,3,4), observations = rep(30,6), type = "PC")

visualRange <- seq(-1,5,length.out = 100)
visualPC1 <- predict(pfm1,visualRange)
visualPC2 <- predict(pfm2,visualRange)
visualPC3 <- predict(pfm3,visualRange)

plot <- ggplot()
plot <- plot + geom_line(mapping =aes(x=visualRange,y=visualPC1),size=2, col="grey")
plot <- plot + geom_line(mapping =aes(x=visualRange,y=visualPC2),size=2, col="grey")
plot <- plot + geom_line(mapping =aes(x=visualRange,y=visualPC3),size=2, col="grey")
plot <- plot + xlab("abscissa")
plot <- plot + ylab("ordinate")
plot <- plot + scale_y_continuous(breaks = c(0,0.5,0.90,1), limits = c(0,1))
plot <- plot + scale_x_continuous(breaks = seq(0,4,1), labels = c(5,10,20,25,30))
plot <- plot + theme_classic(16)
plot <- plot + theme(aspect.ratio = 1)
plot
