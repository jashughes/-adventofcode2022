inp <- strsplit(readChar("13.txt", file.info("13.txt")$size), "\n\n")[[1]]
bracket_switch <- function(s) gsub("\\]", ")", gsub("\\[", "list(", s))

check <- function(s1, s2) {
  # rule #1
  if (is.numeric(s1) && is.numeric(s2)) {
    return(sign(s2 - s1))
    
  } else if (is.list(s1) && is.list(s2)) {
    min_len <- min(length(s1), length(s2))
    for (i in seq_len(min_len)) {
      res <- check(s1[[i]], s2[[i]])
      if (res != 0) return(res)
    }
    
    return(sign(length(s2) - length(s1)))
  
  } else if (is.numeric(s1) != is.numeric(s2)) {
    check(as.list(s1), as.list(s2))
  }
}

right_order <- function(s) {
  p <- lapply(
    bracket_switch(strsplit(s, "\n")[[1]]), 
    function(x) eval(parse(text = x))
  )
  check(p[[1]], p[[2]])
}

parse_eval <- function(x) eval(parse(text = bracket_switch(x)))

pairs <- unlist(lapply(inp, strsplit, "\n"))
sorted <- c()

while (length(pairs) > 0) {
  for (x in 1:length(pairs)) {
    min <- 
    for (y in 1:length(pairs)) {
      if (y == x) next
      if (check(parse_eval(pairs[x]), parse_eval(pairs[y])) > 0)
    }
  }
}

pairs <- c(unlist(lapply(inp, strsplit, "\n")), "[[2]]", "[[6]]")
idx <- rep(1, length(pairs))

for (x in 1:length(pairs)) {
  for (y in 1:length(pairs)) {
    if (x == y) next
    if (check(parse_eval(pairs[x]), parse_eval(pairs[y])) > 0) idx[x] = idx[x] + 1
  }
}
sorted <- pairs[order(-idx)]

prod(which(sorted == "[[2]]" | sorted == "[[6]]"))

pairs <- c(7, 5, 3, 7, 17)
idx <- rep(1, length(pairs))

for (x in 1:length(pairs)) {
  for (y in 1:length(pairs)) {
    if (x == y) next
    if (pairs[x] < pairs[y]) idx[x] = idx[x] + 1
  }
}
pairs[order(-idx)]

while (length(pairs) > 0) {
  print(length(pairs))
  for (x in 1:length(pairs)) {
    mn <- pairs[x]
    for (y in 1:length(pairs)) {
      if (y == x) next
      if (check(parse_eval(pairs[x]), parse_eval(pairs[y])) > 0) mn <- pairs[y]
    }
  } 
  sorted <- c(sorted, mn)
  pairs <- pairs[-which(pairs == mn)[1]]
}

for (x in pairs) {
  if (check(parse_eval(pairs[x]), parse_eval(pairs[y])) > 0) mn <- pairs[y]
}

message("Part 1: ", sum(which(vapply(inp, right_order, numeric(1)) > -1)))

