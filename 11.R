inp <- readLines("11.txt", warn = FALSE)
extract_num <- function(s) as.numeric(gsub(".* ([0-9]+)", "\\1", s))
inv <- gsub(".*: ","", inp[which(grepl("Starting", inp))]) |>
  strsplit(", ") |>
  lapply(as.numeric)
op <- gsub(".*: ","", inp[which(grepl("Operation", inp))]) 
test <- extract_num(inp[which(grepl("Test", inp))])
ifT <- extract_num(inp[which(grepl("If true", inp))]) + 1
ifF <- extract_num(inp[which(grepl("If false", inp))]) + 1

inspect <- function(old, op, modby = 3, new = NULL) {
  eval(parse(text = op))
  print(new)
  floor(new/modby)
} 
counts <- rep(0, length(op))

for (r in 1:20) {
  for (monkey in 1:length(op)) {
    items <- inv[[monkey]]
    inv[[monkey]] <- numeric(0)
    if (length(items) > 0) {
      for (i in 1:length(items)) {
        item <- inspect(items[i], op[monkey])
        counts[monkey] <- counts[monkey] + 1
        pass_to <- ifelse(item %% test[monkey] == 0, ifT[monkey], ifF[monkey])
        inv[[pass_to]] <- c(inv[[pass_to]], item)
      }
    }
  }
}

play <- function(inv, op, test, ifT, ifF, reps = 20, modby = 3){
  counts <- rep(0, length(op))
  
  for (r in 1:reps) {
    for (monkey in 1:length(op)) {
      items <- inv[[monkey]]
      inv[[monkey]] <- numeric(0)
      if (length(items) > 0) {
        for (i in 1:length(items)) {
          item <- inspect(items[i], op[monkey], modby)
          counts[monkey] <- counts[monkey] + 1
          pass_to <- ifelse(item %% test[monkey] == 0, ifT[monkey], ifF[monkey])
          inv[[pass_to]] <- c(inv[[pass_to]], item)
        }
      }
    }
  }
  list(counts = counts, inv = inv)
}

counts <- play(inv, op, test, ifT, ifF, 20, 3)
counts <- play(inv, op, test, ifT, ifF, 7, 1)

message("Part 1: ", prod(sort(counts, decreasing = TRUE)[1:2]))


