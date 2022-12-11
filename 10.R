inp <- strsplit(readLines("09.txt", warn = FALSE), " ")
key <- list(R = c(1, 0), L = c(-1, 0), U = c(0, 1), D = c(0, -1))
save_pos <- function(pos, seen) {unique(c(seen, paste0(pos[1], "_", pos[2])))}

knotted_string <- function(inp, nk, k = key) {
  pos <- lapply(1:nk, function(x) c(0, 0))
  seen <- save_pos(pos[[nk]], c())
  
  for (i in inp) {
    for (step in 1:as.numeric(i[2])) {
      pos[[1]] <- pos[[1]] + k[[i[1]]]
      for (knot in 2:nk) {
        taught <- pos[[knot - 1]] - pos[[knot]]
        pos[[knot]] <- pos[[knot]] + sign(taught) * any(abs(taught) > 1)
      }
      seen <- save_pos(pos[[nk]], seen)
    }
  }
  save_pos(pos[[nk]], seen)
}

message("Part 1: ", length(knotted_string(inp, 2)))
message("Part 2: ", length(knotted_string(inp, 10)))