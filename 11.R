inp <- readLines("11.txt", warn = FALSE)
extract_num <- function(s) as.numeric(gsub(".* ([0-9]+)", "\\1", s))
inv <- gsub(".*: ","", inp[which(grepl("Starting", inp))]) |>
  strsplit(", ") |>
  lapply(as.numeric)
op <- gsub(".*: ","", inp[which(grepl("Operation", inp))]) 
test <- extract_num(inp[which(grepl("Test", inp))])
ifT <- extract_num(inp[which(grepl("If true", inp))]) + 1
ifF <- extract_num(inp[which(grepl("If false", inp))]) + 1

inspect <- function(old, op, new = NULL) eval(parse(text = op))

play <- function(inv, op, test, ifT, ifF, f, val, reps = 20){
  counts <- rep(0, length(op))
  if (length(val) == 1) val <- rep(val, length(op))
  iv <- lapply(inv, lapply, f, val)
  
  for (r in 1:reps) {
    for (monkey in 1:length(op)) {
      items <- iv[[monkey]]
      iv[[monkey]] <- list()
      if (length(items) > 0) {
        for (i in 1:length(items)) {
          item <- f(inspect(items[[i]], op[monkey]), val)
          counts[monkey] <- counts[monkey] + 1
          pass_to <- ifelse(item[monkey] %% test[monkey] == 0, ifT[monkey], ifF[monkey])
          iv[[pass_to]] <- c(iv[[pass_to]], list(item))
        }
      }
    }
  }
  counts
}



counts1 <- play(lapply(inv, `*`, 3), op, test, ifT, ifF, `%/%`, 3, 20)
counts2 <- play(inv, op, test, ifT, ifF, `%%`, test, 10000)
message("Part 1: ", prod(sort(counts1, decreasing = TRUE)[1:2]))
message("Part 2: ", prod(sort(counts2, decreasing = TRUE)[1:2]))
