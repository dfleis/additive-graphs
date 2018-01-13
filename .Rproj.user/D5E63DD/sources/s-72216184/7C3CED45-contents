#===== some useful libraries =====#
library(numbers) # 
library(network)
library(igraph)

library(GGally)
library(ggplot2)

#===== parameters =====#
n <- 10
s <- 1:n
cyc <- F

#===== functions =====#
opfun <- function(x, y) {
  x + y
}
mytest <- function(x) {
  isPrime(x) & (x > n)
}

#===== work =====#
M <- outer(s, s, FUN = opfun)
M[lower.tri(M, diag = T)] <- 0
x <- which(mytest(M) & (M != 0), arr.ind = T)

g <- make_graph(as.vector(t(x)), directed = F)
r <- make_ring(length(s), circular = cyc)

xnet <- network(x, directed = F)
ggnet2(xnet, node.size = 6,
       label = T, label.size = 3,
       edge.label = x[,1] + x[,2], edge.label.color = "gray20", 
       edge.label.size = 2.5)

pt <- proc.time()
isos <- subgraph_isomorphic(r, g)
proc.time() - pt

isos






