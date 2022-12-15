inp <- readLines("15_test.txt", warn = FALSE)
sb <- regmatches(inp, gregexpr('\\(?[0-9-]+', inp)) |>
  unlist() |> 
  as.numeric() |>
  matrix(ncol = 4, byrow = T)

manhattan <- function(m1, m2) abs(m2[1] - m1[1]) + abs(m2[2] - m1[2])
overlaps <- function(a1,a2,b1,b2) {(a1<=b1 & a2>=b1) | (b1<=a1 & b2>=a1)}
signal_strength <- function(v) v[1] * 4000000 + v[2]
manhattan <- function(m) abs(m[3] - m[1]) + abs(m[4] - m[2])

blocked_on_y <- function(sb, y = 2000000) {
  dx <- apply(sb,1, manhattan) - abs(sb[,2] - y)
  itxm <- matrix(sb[which(dx > 0),1] + c(-dx[dx>0], dx[dx>0]), ncol = 2)
  ranges <- itxm[1,, drop = F]
  for (r in 2:nrow(itxm)) { 
    b <- itxm[r,,drop=F]
    ol <- which(overlaps(ranges[,1], ranges[,2], b[,1], b[,2]))
    if (length(ol) == 0) {
      ranges <- rbind(ranges, b)
    } else {
      new_range <- c(min(ranges[ol,1], b[,1]), max(ranges[ol,2], b[,2]))
      ranges <- rbind(ranges[-ol,,drop=F], matrix(new_range, ncol = 2))
    }
  }
  ranges
}

find_location <- function(sb, y_min = 0, y_max = 4000000) {
  for (y in y_min:y_max) {
    r <- blocked_on_y(sb,y)
    opts <- which(r[,1] > y_min | r[,2] < y_max)
    if(length(opts) > 0) {
      opts <- setdiff(opts, nrow(r))
      if (any(r[opts + 1,1] - (r[opts, 2] + 1) > 0)) {
        return(c(r[opts, 2] + 1, y))
      }
    }
  }
}

r <- blocked_on_y(sb)
message("Part 1: ", sum(r[,2]-r[,1])+nrow(r)-nrow(unique(sb[sb[,4] == y,3:4])))
message("Part 2: ", signal_strength(find_location(sb)))

