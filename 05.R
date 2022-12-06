inp <- readLines("05.txt", warn = FALSE)
br <- which(inp == "")
stacks <- inp[1:(br-2)]
ns <- max(as.numeric(strsplit(inp[br-1],"")[[1]]), na.rm = T)
stacks <- lapply(strsplit(stacks, ""), `[`, seq(2, 4 * ns, 4))
mstacks <- t(matrix(unlist(stacks), ncol = ns, byrow = T))
stacks <- lapply(split(mstacks, row(mstacks)), function(x) setdiff(rev(x), " "))
instr <- inp[(br + 1):length(inp)]

move <- function(st, n, fr, dst, transf) {
  st[[dst]] <- c(st[[dst]], transf(tail(st[[fr]], n)))
  st[[fr]] <- st[[fr]][1:(length(st[[fr]]) - n)]
  st
}

operate <- function(stacks, instr, transf) {
  take <- as.numeric(gsub("move ([0-9]+) .*", "\\1", instr))
  from <- as.numeric(gsub(".*from ([0-9]+) .*", "\\1", instr))
  dest <- as.numeric(gsub(".*to ([0-9]+)", "\\1", instr))
  
  for (i in 1:length(instr)) {
    stacks <- move(stacks, take[i], from[i], dest[i], transf)
  }
  paste0(sapply(stacks, tail, 1), collapse = "")
}

message("Part 1: ", operate(stacks, instr, rev))
message("Part 2: ", operate(stacks, instr, c))
