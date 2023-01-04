inp <- strsplit(readLines("23.txt", warn = F), "")
inp <- inp |> unlist() |> matrix(nrow = length(inp), byrow = TRUE)
dirs <- matrix(unlist(expand.grid(-1:1, -1:1)), ncol = 2)[-5,]
p_in_m <- function(p, m) any(m[,1] == p[1] & m[,2] == p[2])
mod1 <- function(n,m) (n - 1) %% m + 1
propose <- function(nbs, p, i) {
  j <- mod1(i,4)
  chk <- list(c(1,4,6),c(3,5,8),1:3,6:8)
  ret <- list(p + c(-1, 0), p + c(1, 0), p + c(0, -1), p + c(0, 1))
  if (!any(nbs)) return(p)
  for (k in 0:3) {
    if (!any(nbs[chk[[mod1(j+k,4)]]])) return(ret[[mod1(j+k,4)]])
  }
  p
}


check_once <- function(elves, dirs, i) {
  proposed <- matrix(NA, ncol = 2, nrow = nrow(elves))
  for (e in 1:nrow(elves)) {
    nb <- matrix(rep(elves[e,], 8), ncol = 2, byrow = T) + dirs
    has_nb <- apply(nb, 1, p_in_m, elves)
    proposed[e,] <- propose(has_nb, elves[e,,drop = F], i)
  }
  proposed
}

check_twice <- function(m) duplicated(m) | duplicated(m, fromLast = T)

elf_movement <- function(inp, rounds = Inf) {
  elves <- matrix(which(inp == "#", arr.ind = TRUE), ncol = 2)
  i <- 1
  repeat {
    proposed <- check_once(elves, dirs, i)
    not_moving <- check_twice(proposed)
    proposed[not_moving,] <- elves[not_moving,]
    #if (identical(new_elves, elves)) return(i)
    if (identical(proposed, elves)) break
    elves <- proposed
    i <- i + 1
    #if (i > rounds) return(elves)
    if (i > rounds) break
  }
}

empty_ground <- function(elves) {
  dx <- 1 + max(elves[,1]) - min(elves[,1])
  dy <- 1 + max(elves[,2]) - min(elves[,2])
  dx * dy - nrow(elves)
}

draw <- function(elves) {
  mnx <- min(elves[,1])
  mxx <- max(elves[,1])
  mny <- min(elves[,2])
  mxy <- max(elves[,2])
  transposed <- elves - matrix(rep(c(mnx-1,mny-1), each = nrow(elves)), ncol=2)
  m <- matrix(".", nrow = mxx - mnx + 1, ncol = mxy - mny + 1)
  m[transposed] <- "#"
  apply(m, 1, paste0, collapse = "") |> cat(sep = "\n")
}

message("Part 1: ", empty_ground(elf_movement(inp, 10)))
message("Part 2: ", elf_movement(inp))
