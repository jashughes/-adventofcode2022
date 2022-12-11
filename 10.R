inp <- strsplit(readLines("10.txt", warn = FALSE), " ")
which_cycles <- c(20, 60, 100, 140, 180, 220)
m <- as.data.frame(Reduce(rbind, lapply(inp, `length<-`, 2)), row.names = FALSE)
m[,1] <- cumsum((m[,1] == "addx") + 1)
midcycles <- setdiff(1:240, m[,1])
m <- rbind(m, matrix(c(midcycles, rep(0, length(midcycles))), ncol = 2))
m <- m[order(m[,1]),]
m[is.na(m[,2]),2] <- 0
m[,2] <- c(0, cumsum(m[,2])[-nrow(m)]) + 1
m[,3] <- (m[,1] - 1) %% 40

print_crt <- function(sprite) {
  sapply(
    split(sprite, rep(1:6, each = 40)), 
    function(x) paste0(c(".", "#")[x+1], collapse = "")
  )
}

message("Part 1: ", sum(m[which_cycles, 1] * m[which_cycles, 2]))
message("Part 2: ", cat(print_crt(abs(m[,2] - m[,3]) <= 1), sep = "\n"))
