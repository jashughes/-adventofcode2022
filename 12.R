library(igraph)
inp <- lapply(readLines("12.txt", warn = F), function(x) utf8ToInt(x) - 96)
m <- matrix(unlist(inp), nrow = length(inp), byrow = TRUE)
st <- which(m == utf8ToInt("S") - 96, arr.ind = TRUE)
end <- which(m == utf8ToInt("E") - 96, arr.ind = TRUE)
m[st] <- utf8ToInt("a") - 96
m[end] <- utf8ToInt("z") - 96
lu <- which(m >= 0, arr.ind = T)

g <- make_undirected_graph(NULL)
g <- add_vertices(g, nrow(lu))

coords <- matrix(c(1,0, -1,0, 0,1, 0,-1), ncol = 2, byrow = T)
neighbours <- function(idx, crd = coords, d = dim(m)) {
  nb <- t(t(crd) + idx[1,])
  nb[nb[,1] > 0 & nb[,1] <= d[1] & nb[,2] > 0 & nb[,2] <= d[2],, drop = F]
}

get_idx <- function(crd, lu) which(lu[,1] == crd[1,1] & lu[,2] == crd[1,2])

for (i in 1:nrow(lu)) {
  idx <- lu[i, , drop = F]
  ival <- m[idx]
  nbs <- neighbours(idx)
  for (nb in 1:nrow(nbs)) {
    if  (m[nbs[nb, , drop = F]] - ival <= 1) {
      nb_idx <- get_idx(nbs[nb,, drop = F], lu)
      g <- add_edges(g, c(i, nb_idx))
      
    }
  }
}
paths <- shortest_paths(g, get_idx(st, lu), get_idx(end, lu))

message("Part 1: ", length(paths$vpath[[1]]))
message("Part 2: ", prod(sort(counts2, decreasing = TRUE)[1:2]))
