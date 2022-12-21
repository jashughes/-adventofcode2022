inp <- strsplit(readLines("18.txt", warn = F), ",") |>
  lapply(as.numeric) |> unlist() |> matrix(ncol = 3, byrow = T)
is_adj <- function(c1, c2) sum(c1 == c2) == 2 & max(abs(c1-c2)) == 1
p_in_m <- function(p, m) any(m[,1] == p[1] & m[,2] == p[2] & m[,3] == p[3])
hash <- function(m) m %*% (100**(0:2))
dirs <- matrix(c(1,0,0,-1,0,0,0,1,0,0,-1,0,0,0,1,0,0,-1), ncol = 3) 

overlapping_faces <- function(inp) {
  ol <- 0
  for (i in seq_len(nrow(inp))) {
    for (j in i:nrow(inp)) ol <- ol + is_adj(inp[i,], inp[j,])
  }
  nrow(inp) * 6 - 2 * ol
}

bubble_expand <- function(p, seen, d) {
  explore <- matrix(p, nrow = 1)
  seen <- hash(p)
  while (nrow(explore) > 0) {
    nbs <- matrix(ncol = 3, nrow = 0)
    for (r in 1:(nrow(explore))) {
      nbs <- rbind(nbs, d + rep(explore[r,], each = 6))
    }
    nbs <- nbs[!duplicated(nbs),]
    nbs <- pts[match(hash(nbs), hash(pts[,1:3])),]
    if (any(is.na(nbs[,4])) || any(nbs[,4] == 3)) { # open to air, whole bubble escapes
      nbs <- cbind(nbs, hash(nbs[,1:3]))
      nbs <- nbs[nbs[,4] == 0 & !is.na(nbs[,4]),, drop = FALSE]
      seen <- unique(c(seen, nbs[,5]))
      pts[match(seen, hash(pts[,1:3])), 4] <<- 3
      return(FALSE)
    }
    nbs <- cbind(nbs, hash(nbs[,1:3]))
    nbs <- nbs[nbs[,4] == 0 & !nbs[,5] %in% seen,, drop = FALSE]
    explore <- nbs[,1:3, drop = FALSE]
    seen <- unique(c(seen, nbs[,5]))
  }
  
  pts[match(seen, hash(pts[,1:3])), 4] <<- 2
  TRUE
}

lim <- c(apply(inp,2, min), apply(inp,2, max))
pts <- as.matrix(expand.grid(lim[1]:lim[4],lim[2]:lim[5],lim[3]:lim[6]))
pts <- cbind(pts, 0) # 0 = no info, 1 = input, 2 = bubble, 3 = void
in_input <- apply(pts, 1, p_in_m, inp)
pts[in_input,4] <- 1


for (i in seq_len(nrow(pts))) {
  if (pts[i,4] != 0) next
  bubble_expand(pts[i,1:3], matrix(ncol = 3, nrow = 0), d = dirs)
}

all_surface_area <- overlapping_faces(inp)

message("Part 1: ", all_surface_area)
message("Part 2: ", all_surface_area - overlapping_faces(pts[pts[,4] == 2,1:3]))