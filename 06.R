inp <- strsplit(readLines("06.txt", warn = FALSE), "")[[1]]
find_first_n_unique <- function(inp, n) {
  for (i in n:length(inp)) {
    if (length(unique(inp[(i- n + 1):i])) == n) return(i)
  }
}

message("Part 1: ", find_first_n_unique(inp, 4))
message("Part 2: ", find_first_n_unique(inp, 14))
