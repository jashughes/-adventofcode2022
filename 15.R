inp <- readLines("15.txt", warn = FALSE)
sb <- regmatches(inp, gregexpr('\\(?[0-9-]+', inp)) |>
  unlist() |> 
  as.numeric() |>
  matrix(ncol = 4, byrow = T)

overlaps <- function(a1,a2,b1,b2) {(a1<=b1 & a2>=b1) | (b1<=a1 & b2>=a1)}
signal_strength <- function(v) v[1] * 4000000 + v[2]
manhattan <- function(m) abs(m[3] - m[1]) + abs(m[4] - m[2])

blocked_on_y <- function(sb, y = 2000000) {
  dx <- apply(sb,1, manhattan) - abs(sb[,2] - y)
  itxm <- matrix(sb[which(dx > 0),1] + c(-dx[dx>0], dx[dx>0]), ncol = 2)
  ranges <- itxm[1,, drop = F]
  for (r in 2:nrow(itxm)) { 
    b <- itxm[r,,drop=F]
    ol <- overlaps(ranges[,1], ranges[,2], b[,1], b[,2])
    ranges <- rbind(
      ranges[!ol,,drop=F],
      cbind(min(ranges[ol,1], b[,1]), max(ranges[ol,2], b[,2]))
    )
  }
  ranges
}

n_blocked <- function(sb, y = 2000000) {
  r <- blocked_on_y(sb)
  sum(r[,2]-r[,1])+nrow(r)-nrow(unique(sb[sb[,4] == y,3:4]))
}

find_location <- function(sb, y_min = 0, y_max = 4000000) {
  for (y in y_min:y_max) {
    r <- blocked_on_y(sb,y)
    if (!any(r[,1] > y_min | r[,2] < y_max)) next
    opts <- setdiff(which(r[,1] > y_min | r[,2] < y_max), nrow(r))
    if (any(r[opts + 1,1] - (r[opts, 2] + 1) > 0)) return(c(r[opts, 2] + 1, y))
  }
}

message("Part 1: ", n_blocked(sb))
message("Part 2: ", signal_strength(find_location(sb)))

