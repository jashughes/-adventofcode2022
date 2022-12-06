inp <- readLines("05.txt", warn = FALSE)
br <- which(inp == "")
stacks <- inp[1:(br-2)]
ns <- max(as.numeric(strsplit(inp[br-1],"")[[1]]), na.rm = T)
stacks <- lapply(strsplit(stacks, ""), `[`, seq(2, 4 * ns, 4))
mstacks <- t(matrix(unlist(stacks), ncol = ns, byrow = T))
stacks <- lapply(split(mstacks, row(mstacks)), function(x) setdiff(rev(x), " "))

instr <- inp[(br + 1):length(inp)]
take <- as.numeric(gsub("move ([0-9]+) .*", "\\1", instr))
from <- as.numeric(gsub(".*from ([0-9]+) .*", "\\1", instr))
dest <- as.numeric(gsub(".*to ([0-9]+)", "\\1", instr))

move1 <- function(st, n, fr, dst) {
  st[[dst]] <- c(st[[dst]], rev(tail(st[[fr]], n)))
  st[[fr]] <- st[[fr]][1:(length(st[[fr]]) - n)]
  st
}

move2 <- function(st, n, fr, dst) {
  st[[dst]] <- c(st[[dst]], tail(st[[fr]], n))
  st[[fr]] <- st[[fr]][1:(length(st[[fr]]) - n)]
  st
}

stacks1 <- stacks
for (i in 1:length(instr)) {
  stacks1 <- move1(stacks1, take[i], from[i], dest[i])
}
stacks2 <- stacks
for (i in 1:length(instr)) {
  stacks2 <- move2(stacks2, take[i], from[i], dest[i])
}


message("Part 1: ", paste0(sapply(stacks1, tail, 1), collapse = ""))
message("Part 2: ", paste0(sapply(stacks2, tail, 1), collapse = ""))
