inp <- readLines("15.txt", warn = FALSE)
sb <- regmatches(inp, gregexpr('\\(?[0-9-]+', inp)) |>
  unlist() |> 
  as.numeric() |>
  matrix(ncol = 4, byrow = T)

overlaps <- function(a1,a2,b1,b2) {(a1<=b1 & a2>=b1) | (b1<=a1 & b2>=a1)}
signal_strength <- function(v) v[1] * 4000000 + v[2]
Nblock <- function(r,s,y) sum(r[,2]-r[,1])+nrow(r)-nrow(unique(s[s[,4]==y,3:4]))
manhattan <- function(m) abs(m[,3] - m[,1]) + abs(m[,4] - m[,2])

blocked_on_y <- function(sb, y = 2000000) {
  dx <- manhattan(sb) - abs(sb[,2] - y)
  itxm <- matrix(rep(0, 2 * sum(dx > 0)), ncol = 2)
  itxm[,1] <- sb[dx > 0, 1] - dx[dx > 0]
  itxm[,2] <- sb[dx > 0, 1] + dx[dx > 0]
  
  ranges <- matrix(nrow = nrow(sb), ncol = 2)
  nr <- 1
  b <- numeric(2)
  
  ranges[1,] <- itxm[1,]
  for (r in 2:nrow(itxm)) { 
    b[] <- itxm[r,]
    ol <- overlaps(ranges[seq_len(nr),1], ranges[seq_len(nr),2], b[1], b[2])
    if (any(ol)) {
      b <- c(min(ranges[seq_len(nr)[ol],1], b[1]), max(ranges[seq_len(nr)[ol],2], b[2]))
      ranges[seq_len(sum(!ol)),] <- ranges[seq_len(nr)[!ol],]
      nr <- sum(!ol) + 1
      ranges[nr,] <- b
    } else {
      nr <- nr + 1
      ranges[nr,] <- b
    }
  }
  ranges[1:nr,,drop=FALSE]
}

find_location <- function(sb, y_min = 0, y_max = 4000000) {
  for (y in y_min:y_max) {
    r <- blocked_on_y(sb,y)
    if (!any(r[,1] > y_min | r[,2] < y_max)) next
    opts <- setdiff(which(r[,1] > y_min | r[,2] < y_max), nrow(r))
    if (any(r[opts + 1,1] - (r[opts, 2] + 1) > 0)) return(c(r[opts, 2] + 1, y))
  }
}

message("Part 1: ", Nblock(blocked_on_y(sb, 2000000), sb, y = 2000000))
message("Part 2: ", signal_strength(find_location(sb)))
