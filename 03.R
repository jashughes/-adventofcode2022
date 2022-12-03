instr <- readLines("03.txt", warn = FALSE)
key <- setNames(1:52, c(letters, LETTERS))

find_overlap <- function(s) {
  nc <- nchar(s)
  items <- strsplit(s, "")[[1]]
  intersect(items[1:nc/2], items[(nc/2 + 1):nc])
}
overlap <- sapply(instr, find_overlap)

badges <- sapply(
  split(lapply(strsplit(instr, ""), unique), rep(1:100, each = 3)),
  Reduce,
  f = intersect
)

message("Part 1: ", sum(key[overlap]))
message("Part 2: ", sum(key[badges]))