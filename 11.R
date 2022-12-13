extract_num <- function(s) as.numeric(gsub(".* ([0-9]+)", "\\1", s))
inspect <- function(old, op, new = NULL) eval(parse(text = op))
top2prod <- function(vec) prod(sort(vec, decreasing = TRUE)[1:2])
pass_test <- function(n, div, if_T, if_F) ifelse(n %% div == 0, if_T, if_F)

play <- function(inp, f, reps = 20){
  inv <- gsub(".*: ","", inp[which(grepl("Starting", inp))]) |>
    strsplit(", ") |>
    lapply(as.numeric)
  op <- gsub(".*: ","", inp[which(grepl("Operation", inp))]) 
  test <- extract_num(inp[which(grepl("Test", inp))])
  ifT <- extract_num(inp[which(grepl("If true", inp))]) + 1
  ifF <- extract_num(inp[which(grepl("If false", inp))]) + 1
  counts <- rep(0, length(op))
  iv <- lapply(inv, lapply, rep, length(op))
  
  for (r in 1:reps) {
    for (monkey in 1:length(op)) {
      items <- iv[[monkey]]
      iv[[monkey]] <- list()
      if (length(items) > 0) {
        for (i in 1:length(items)) {
          item <- f(inspect(items[[i]], op[monkey]))
          counts[monkey] <- counts[monkey] + 1
          to <- pass_test(item[monkey], test[monkey], ifT[monkey], ifF[monkey])
          iv[[to]] <- c(iv[[to]], list(item))
        }
      }
    }
  }
  counts
}
inp <- readLines("11.txt", warn = FALSE)
test <- extract_num(inp[which(grepl("Test", inp))])

message("Part 1: ", top2prod(play(inp, function(x) x %/% 3, 20)))
message("Part 2: ", top2prod(play(inp, function(x) x %% test, 10000)))
