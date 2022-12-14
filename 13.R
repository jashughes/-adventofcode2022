inp <- strsplit(readChar("13.txt", file.info("13.txt")$size), "\n\n")[[1]]
bracket_switch <- function(s) gsub("\\]", ")", gsub("\\[", "list(", s))
parse_eval <- function(x) eval(parse(text = bracket_switch(x)))
sig <- lapply(unlist(lapply(inp, strsplit, "\n")), parse_eval)
pt2ins <- lapply(c("[[2]]", "[[6]]"), parse_eval)

check <- function(s1, s2) {
  if (is.numeric(s1) && is.numeric(s2)) return(sign(s2 - s1))
  
  if (is.list(s1) && is.list(s2)) {
    min_len <- min(length(s1), length(s2))
    for (i in seq_len(min_len)) {
      res <- check(s1[[i]], s2[[i]])
      if (res != 0) return(res)
    }
    return(sign(length(s2) - length(s1)))
  } 
  if (is.numeric(s1) != is.numeric(s2)) check(as.list(s1), as.list(s2))
}

sort_signals <- function(sig) {
  idx <- rep(0, length(sig))
  for (x in 1:length(sig)) {
    for (y in 1:length(sig)) {
      if (check(sig[x], sig[y]) > 0) idx[x] = idx[x] - 1
    }
  }
  idx
}

checked <- mapply(check, sig[seq(1,length(sig),2)], sig[seq(2,length(sig),2)])

message("Part 1: ", sum(which(checked > 0)))
message("Part 2: ", prod(which(order(sort_signals(c(pt2ins, sig))) %in% 1:2)))

