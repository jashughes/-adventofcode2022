inp <- strsplit(readLines("08.txt", warn = FALSE), "")
d <- length(inp)
m <- inp |>
  unlist() |>
  as.numeric() |>
  matrix(byrow = T, nrow = d)
dir_score <- function(v, tree) { 
  ifelse(any(v >= tree), length(v[1:which(v >= tree)[1]]), length(v))
} 

vis <- matrix(rep(TRUE, d * d), nrow = d)
scenic <- matrix(rep(0, d * d), nrow = d)
for (i in 2:(d - 1)) {
  for (j in 2:(d - 1)) {
    tree <- m[i, j]
    vis[i,j] <- any(c(
      all(m[1:(i-1), j] < tree), 
      all(m[(i+1):d, j] < tree), 
      all(m[i, 1:(j-1)] < tree), 
      all(m[i, (j+1):d] < tree)
    )) 
    scenic[i,j] <- prod(c(
      dir_score(rev(m[1:(i-1), j]), tree), 
      dir_score(m[(i+1):d, j], tree), 
      dir_score(rev(m[i, 1:(j-1)]), tree), 
      dir_score(m[i, (j+1):d], tree)
    )) 
  }
}

message("Part 1: ", sum(vis))
message("Part 2: ", max(scenic))