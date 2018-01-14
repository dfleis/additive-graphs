###
# PLOTS:
#
# Determines whether an additive graph of vertices {1, ..., n}
# (from n = nmin to n = nmax) and edges defined by a user specified
# function has a Hamiltonian path/cycle
###

#===== some useful libraries =====#
library(numbers) # number theoretic tools (isPrime(), etc.)
library(igraph) # subgraph_isomorphic(), subgraph_isomorphisms()
library(GGally) # ggnet2(), plotting graphs
library(ggplot2) 
library(network) # generate graphs/networks for use with GGally

#===== parameters =====#
nmin <- 6 # fewest vertices to consider
nmax <- 50 # most vertices to consider

nvec <- nmin:nmax

#===== functions =====#
is_integer <- function(x, eps = 1e-15) {
  min(abs(c(x %% 1, x %% 1 - 1))) < eps
}
vis_integer <- Vectorize(is_integer)
edge_fun <- function(x, y) { # compute edge values
  x + y
}
edge_test <- function(x) { # edge set test 
  isPrime(x) & (x > n)
  #vis_integer(sqrt(x))
}

#===== work =====#
for (n in nvec) {
  print(n)
  
  V <- 1:n
  M <- outer(V, V, FUN = edge_fun)
  M[lower.tri(M, diag = T)] <- 0
  E <- which(edge_test(M) & (M != 0), arr.ind = T)
  
  g <- make_graph(as.vector(t(E)), directed = F)
  
  xnet <- network(E, directed = F)
  
  nb_lead_zeros <- ceiling(log10(nmax) - log10(n)) + 1
  lead_zeros <- rep(0, nb_lead_zeros)
  filename <- paste0("./img/graph_", lead_zeros, n, ".pdf")
  
  #pdf(filename, width = 4, height = 4)
  ggnet2(xnet, node.size = 6,
         label = T, label.size = 3,
         edge.label = E[,1] + E[,2], edge.label.color = "gray20", 
         edge.label.size = 2.5)
  ggsave(filename)
}







