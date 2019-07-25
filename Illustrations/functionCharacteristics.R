##plain plot
library(PsyFuns)

params <- c(11,0.5)
pfm <- PFm(PF("gumbel_r", "al", 0.5,0,params), predictor = c(0,1,1.5,2,3,4), observations = rep(30,6), type = "PC")

visualRange <- seq(7.75,19,length.out = 100)
visualPC <- predict(pfm,visualRange)

midpoint <- pfm$perf_th
imp_th <- pfm$imp_th
quant <- PsyFuns:::al.inverse_x.cdf(PsyFuns:::gumbel_r.inverse.cdf(c(0.05,0.25,0.75,0.95)),params)

plot <- ggplot()
plot <- plot + geom_line(mapping =aes(x=visualRange,y=visualPC),size=0.5, col="grey")
plot <- plot + geom_point(mapping = aes(x=midpoint, y=predict(pfm,midpoint)), size=3, fill="grey", shape=23)
plot <- plot + geom_point(mapping = aes(x=imp_th, y=predict(pfm,imp_th)), size=3, fill="grey", shape=24)
plot <- plot + geom_vline(xintercept = c(midpoint,imp_th), col="grey", linetype="dotted")
plot <- plot + geom_point(mapping = aes(x=quant, y=predict(pfm,quant)), size=2, fill="grey", shape=21)
plot <- plot + geom_vline(xintercept = quant, col="grey", linetype="dotdash")
plot <- plot + xlab("abscissa")
plot <- plot + ylab("ordinate")
plot <- plot + scale_y_continuous(breaks = c(0.5,0.625,0.75,0.875,1), limits = c(0.35,1))
plot <- plot + scale_x_continuous(breaks = round(c(8,10, 15,18,midpoint, imp_th)), trans="log")
plot <- plot + theme_classic(16)
plot <- plot + theme(aspect.ratio = 1)
plot

