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
  floor(new/modby)
} 

play1 <- function(inv, op, test, ifT, ifF, reps = 20, modby = 3){
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


play2 <- function(inv, op, test, ifT, ifF, reps = 20, modby = 3){
  counts <- rep(0, length(op))
  iv <- lapply(inv, lapply, function(x) x %% test)
  
  for (r in 1:reps) {
    for (monkey in 1:length(op)) {
      items <- iv[[monkey]]
      iv[[monkey]] <- list()
      if (length(items) > 0) {
        for (i in 1:length(items)) {
          item <- inspect(items[[i]], op[monkey], modby) %% test
          counts[monkey] <- counts[monkey] + 1
          pass_to <- ifelse(item[monkey] == 0, ifT[monkey], ifF[monkey])
          iv[[pass_to]] <- c(iv[[pass_to]], list(item))
        }
      }
    }
  }
  counts
}

counts1 <- play1(inv, op, test, ifT, ifF, 20, 3)
counts2 <- play2(inv, op, test, ifT, ifF, 10000, 1)

message("Part 1: ", prod(sort(counts1, decreasing = TRUE)[1:2]))
message("Part 2: ", prod(sort(counts2, decreasing = TRUE)[1:2]))
