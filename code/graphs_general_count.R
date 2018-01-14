###
#
# Count number of Hamiltonian path/cycles (up to symmetry)
#
# Determines whether an additive graph of vertices {1, ..., n}
# (from n = nmin to n = nmax) and edges defined by a user specified
# function has a Hamiltonian path/cycle
###

#===== some useful libraries =====#
library(numbers) # number theoretic tools (isPrime(), etc.)
library(igraph) # subgraph_isomorphic(), subgraph_isomorphisms()
library(network) # generate graphs/networks for use with GGally

#===== parameters =====#

nmin <- 3 # fewest vertices to consider
nmax <- 10 # most vertices to consider
cyc <- F # look for hamiltonian path (cyc <- F) or cycle (cyc <- T)

nvec <- nmin:nmax
times <- vector(mode = 'numeric', length = length(nvec))
nb_hamilt <- vector(mode = 'numeric', length = length(nvec))

#===== functions =====#

is_integer <- function(x, eps = 1e-15) {
  min(abs(c(x %% 1, x %% 1 - 1))) < eps
}
vis_integer <- Vectorize(is_integer)
edge_fun <- function(x, y) { # compute edge values
  x + y
}
edge_test <- function(x) { # edge set test 
  isPrime(x)
  #isPrime(x) & (x > n)
}

#===== work =====#

for (n in nvec) {
  pt <- proc.time()
  
  V <- 1:n
  M <- outer(V, V, FUN = edge_fun)
  M[lower.tri(M, diag = T)] <- 0
  E <- which(edge_test(M) & (M != 0), arr.ind = T)
  
  g <- make_graph(as.vector(t(E)), directed = F)
  r <- make_ring(length(V), circular = cyc)
  
  nb_hamilt[n - nmin + 1] <- length(subgraph_isomorphisms(r, g))/2
  times[n - nmin + 1] <- unname((proc.time() - pt)[3])
  
  print(unname(cbind(n, times[n - nmin + 1])))
}

#===== plots =====#

plot(times ~ nvec, type = 'o', pch = 21, cex = 0.5, bg = "white",
     xlab = "n", ylab = "Time (s)")
plot(nb_hamilt ~ nvec, type = 'o', pch = 21, cex = 0.5, bg = "white",
     xlab = "n", ylab = ifelse(cyc, "Nb. Cycles", "Nb. Paths"))










