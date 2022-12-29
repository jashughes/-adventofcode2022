inp <- readLines("25.txt", warn = F)

desnafu <- function(s) {
  key <- setNames(2:-2, c("2", "1", "0", "-", "="))
  arr <- strsplit(s, "")[[1]]
  sum(key[arr] * (5 ** seq(length(arr) - 1, 0, -1)))
}

snafu <- function(d) {
  nr <- numeric(0)
  while (d > 0) {
    nn <- d %% 5
    if (nn > 2) {
      nn <- nn - 5
      d <- d + 5
    }
    nr <- c(nr, nn)
    d <- d %/% 5
  }
  setNames(c("2","1","0","-","="), as.character(2:-2))[as.character(rev(nr))] |>
    paste0(collapse = "")
}

message("Part 1: ", sum(vapply(inp, desnafu, numeric(1))) |> snafu())
