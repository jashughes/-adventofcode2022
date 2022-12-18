inp <- strsplit(readLines("17.txt", warn = FALSE), "")[[1]]
winds <- unname(setNames(c(1,-1), c(">", "<"))[inp])
rocks <- list(
  matrix(c(1,1,1,1), nrow = 1),
  matrix(c(0,1,0,1,1,1,0,1,0), nrow = 3),
  matrix(c(0,0,1,0,0,1,1,1,1), nrow = 3),
  matrix(c(1,1,1,1), ncol = 1),
  matrix(c(1,1,1,1), nrow = 2)
)
mod1 <- function(n,m) (n - 1) %% m + 1
unobstructed <- function(r, m, x, y) {
  m[y:(y+nrow(r)-1), x:(x+ncol(r)-1)] <- r + m[y:(y+nrow(r)-1), x:(x+ncol(r)-1)]
  max(m) <= 1
}
move <- function(r, x, y, m, wind) { 
  dx <- c(x + wind, ncol(r) + x + wind - 1)
  if (dx[1] > 0 && dx[2] < 8 && unobstructed(r, m, x + wind, y)) x <- x + wind
  if ((y + 1) <= nrow(m) && unobstructed(r, m, x, y + 1)) y <- y + 1
  c(x, y)
}

fall <- function(inp, rocks, n_rep) {
  m <- matrix(0, ncol = 7, nrow = 0)
  w <- 0
  h <- 0
  
  for (i in 1:n_rep) {
    r <- rocks[[mod1(i, 5)]]
    m <- rbind(matrix(0, ncol = 7, nrow = nrow(r) + 3), m)
    pos <- c(3, 1)
    repeat {
      w <- mod1(w + 1, length(winds))
      new <- move(r, pos[1], pos[2], m, winds[w])
      if (pos[2] == new[2]) {
        x <- new[1]
        y <- new[2]
        m[y:(y+nrow(r)-1),x:(x+ncol(r)-1)] <- r + m[y:(y+nrow(r)-1),x:(x+ncol(r)-1)]
        break
      }
      pos <- new
    }
    trim_0 <- which(apply(m, 1, max) != 0)[1]
    trim_max <- head(which(apply(m, 1, sum) == 7), 1)
    if (length(trim_max) == 0) {
      trim_max <- nrow(m)
    } else {
      h <- h + nrow(m) - trim_max
    }
    m <- m[trim_0:trim_max,, drop = F]
  }
  h + nrow(m)
}

height_at_number <- function(inp, rocks, x) {
  multiples <- x %/% 1745
  remainder <- x %% 1745
  multiples * 2778 + fall(inp, rocks, 1745 + remainder) - 2778
}

message("Part 1: ", fall(inp, rocks, 2022))
message("Part 2: ", height_at_number(inp, rocks, 1000000000000))



#### Identification of cycle periods & heights
#### This is the code I ran to get the constants above
testing <- FALSE
if (testing) {
  m <- matrix(0, ncol = 7, nrow = 0)
  w <- 0
  h <- 0
  heads <- matrix(0, ncol = 7, nrow = 0)
  wind_chime <- numeric()
  rock_chime <- numeric()
  chime_heights <- numeric()
  
  for (i in 1:(2022 * 10)) {
    r <- rocks[[mod1(i, 5)]]
    m <- rbind(matrix(0, ncol = 7, nrow = nrow(r) + 3), m)
    pos <- c(3, 1)
    repeat {
      w <- mod1(w + 1, length(winds))
      if (w == 1 & i > 1) {
        wind_chime <- c(wind_chime, i)
        rock_chime <- c(rock_chime, mod1(i, 5))
        heads <- rbind(
          heads, 
          matrix(apply(m, 2, Position, f = identity), ncol = 7)
        )
        chime_heights <- c(
          chime_heights, 
          h + nrow(m[which(apply(m, 1, max) != 0)[1]:nrow(m),])
        )
      }
      new <- move(r, pos[1], pos[2], m, winds[w])
      if (pos[2] == new[2]) {
        x <- new[1]
        y <- new[2]
        m[y:(y+nrow(r)-1), x:(x+ncol(r)-1)] <- r + m[y:(y+nrow(r)-1), x:(x+ncol(r)-1)]
        break
      }
      pos <- new
    }
    trim_0 <- which(apply(m, 1, max) != 0)[1]
    trim_max <- head(which(apply(m, 1, sum) == 7), 1)
    if (length(trim_max) == 0) {
      trim_max <- nrow(m)
    } else {
      h <- h + nrow(m) - trim_max
    }
    m <- m[trim_0:trim_max,, drop = F]
  }
  identical(heads[2,], heads[3,])
  identical(rock_chime[2], rock_chime[3])
  message("cycles repeat every ", wind_chime[1], " rocks")
  message("height grows ", chime_heights[1], " every cycle")
}
