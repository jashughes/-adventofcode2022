library(igraph)
height <- function(x) utf8ToInt(x) - 96
coords <- matrix(c(1,0, -1,0, 0,1, 0,-1), ncol = 2, byrow = T)

# Parse input
inp <- lapply(readLines("12.txt", warn = F), height)
m <- matrix(unlist(inp), nrow = length(inp), byrow = TRUE)
st <- which(m == height("S"))
end <- which(m == height("E"))
m[st] <- height("a")
m[end] <- height("z")

# functions
neighbours <- function(idx, crd = coords, d = dim(m)) {
  nb <- t(t(crd) + idx[1,])
  nb[nb[,1] > 0 & nb[,1] <= d[1] & nb[,2] > 0 & nb[,2] <= d[2],, drop = F]
}
get_idx <- function(crd, lu) which(lu[,1] == crd[1] & lu[,2] == crd[2])
path <- function(from, to, g) length(shortest_paths(g, from, to)$vpath[[1]]) - 1

graph_hill <- function(m) {
  lu <- which(m >= 0, arr.ind = T)
  g <- make_directed_graph(NULL)
  g <- add_vertices(g, nrow(lu))
  for (i in 1:nrow(lu)) {
    idx <- lu[i, , drop = F]
    ival <- m[idx]
    nbs <- neighbours(idx)
    for (nb in 1:nrow(nbs)) {
      if  (m[nbs[nb, , drop = F]] - ival <= 1) {
        g <- add_edges(g, c(i, get_idx(nbs[nb,], lu)))
      }
    }
  }
  g
}

# Solve
g <- graph_hill(m)
from_a <- sapply(which(m == height("a")), path, end, g)

message("Part 1: ", path(st, end, g))
message("Part 2: ", min(setdiff(from_a, -1)))
