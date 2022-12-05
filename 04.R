inp <- readLines("04.txt", warn = FALSE)

parse_ranges <- function(s) {
  m <- regexpr("^([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)$", s, perl = TRUE)
  matrix(
    as.numeric(
      substring(s,x <- attr(m,"capture.start"), attr(m,"capture.length") + x-1) 
    ),
    nrow = length(s)
  )
}
contains <- function(r) {(r[1]<=r[3] & r[2]>=r[4]) | (r[3]<=r[1] & r[4]>=r[2])}
overlaps <- function(r) {(r[1]<=r[3] & r[2]>=r[3]) | (r[3]<=r[1] & r[4]>=r[1])}

message("Part 1: ", sum(apply(parse_ranges(inp), 1, contains)))
message("Part 2: ", sum(apply(parse_ranges(inp), 1, overlaps)))
