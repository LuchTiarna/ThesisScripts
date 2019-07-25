library(igraph)
library(lsr)
###Script parameters

#parameter among which values Cohen's D is measured - MSE, Pearson X^2, Likelihood, perf_th_err, iqr_diff 
parameter <- c("perf_th_err")
group <- c("sigmoid_fit")
#direction <- 1

# pairwise t-test is computed with 
#pairwise.ttest(data$parameter, $group)

### dataLoading
### agregatedData.csv
file <- file.choose()
data <- read.csv(file)
data$X <- NULL

### filtering out data with high noise deviance for emensely bad fits of iqr 
#data_for_iqr <- data %>% filter(noise_sd < 0.04)
#data_grouped <- split(data_for_iqr, f=as.list(data[group]), drop = TRUE)

data_grouped <- split(data, f=as.list(data[group]), drop = TRUE)
data_grouped <- split(data_for_iqr, f=as.list(data_for_iqr[group]), drop = TRUE)
cds <- matrix(data=list(),nrow = length(data_grouped), ncol = length(data_grouped))
colnames(cds) <- names(data_grouped)
rownames(cds) <- names(data_grouped)

sigs <- matrix(data=list(),nrow = length(data_grouped), ncol = length(data_grouped))
colnames(sigs) <- names(data_grouped)
rownames(sigs) <- names(data_grouped)


#cd values
for(i in 1:length(data_grouped)){
  for(j in 1:length(data_grouped)){
    cohen_d <- cohensD(x=data_grouped[[i]][[parameter]], y=data_grouped[[j]][[parameter]])
    sig  <- sign(mean(data_grouped[[i]][[parameter]])-mean(data_grouped[[j]][[parameter]]))
    sigs[i,j] <- sig
    cds[i,j] <- cohen_d * sig
  }
}

###
#  Results
###

### resulting table of Cohen's Ds
res <- cds
res[upper.tri(res)] <- NaN
diag(res)           <- NaN
res <- res[-1,-ncol(res)]
res

### Resulting table of p-values
pairwise.t.test(data[[parameter]], data[[group]])


###
#   Graphical representation of Cohen D's in graph
###

ff <- function(x){ifelse(x > 0, x, 0)}
lines <-structure(sapply(sigs, ff), dim=dim(cds))
colnames(lines) <- names(data_grouped)
rownames(lines) <- names(data_grouped)


network <- graph_from_adjacency_matrix(lines, mode = "directed")

layout(matrix(1, 1,1))
plot(network,
     vertex.color=rgb(0,0,0,0),
     vertex.frame.color=rgb(0,0,0,0),
     #vertex.shape="circle",
     vertex.size=30,
     
     edge.label=t(cds)[t(lines>0)]
)

