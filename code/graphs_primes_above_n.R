#===== some useful libraries =====#
library(numbers) # number theoretic tools (isPrime(), etc.)
library(igraph) # subgraph_isomorphic(), subgraph_isomorphisms()
library(GGally) # ggnet2(), plotting graphs
library(network) # generate graphs/networks for use with GGally

#===== parameters =====#
n <- 4
V <- 1:n # vertex set
cyc <- F # look for hamiltonian path (cyc <- F) or cycle (cyc <- T)

#===== functions =====#
edge_fun <- function(x, y) { # compute edge values
  x + y
}
edge_test <- function(x) { # edge set test 
  isPrime(x) & (x > n)
}

#===== work =====#

M <- outer(V, V, FUN = edge_fun) # define all additive pairs
M[lower.tri(M, diag = T)] <- 0 # (ignore symmetry)
E <- which(edge_test(M) & (M != 0), arr.ind = T) # define edge set

g <- make_graph(as.vector(t(E)), directed = F)
r <- make_ring(length(V), circular = cyc)

xnet <- network(E, directed = F)
ggnet2(xnet, node.size = 6,
       label = T, label.size = 3,
       edge.label = E[,1] + E[,2], edge.label.color = "gray20", 
       edge.label.size = 2.5)

pt <- proc.time()
iso <- subgraph_isomorphic(r, g)
proc.time() - pt

iso






