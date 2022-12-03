filename <- "01.txt"
cals <- readChar(filename, file.info(filename)$size)
sum_of_cals <- sapply(
  strsplit(cals, "\n\n")[[1]],
  function(x) sum(as.numeric(strsplit(x, "\n")[[1]]))
)
message("Part 1:", max(sum_of_cals))
message("Part 2:", sum(sort(sum_of_cals, decreasing = TRUE)[1:3]))
