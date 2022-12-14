inp <- readLines("14.txt", warn = FALSE)
idx <- function(v) matrix(v, nrow = 1)
valid <- function(m, d) m[1,1] <= d[1] && m[1,2] <= d[2]
origin <- idx(c(500,0) + c(1, 1))

str2idx <- function(s) {
  strsplit(s, " -> ")[[1]] |>
    vapply(
      function(x) as.numeric(strsplit(x, ",")[[1]]) + 1, # base 1 
      numeric(2), USE.NAMES = F
    ) |>
    t()
}
expand <- function(m) { # has duplicates
  Reduce(
    rbind, 
    lapply(1:(nrow(m)-1), function(x) {
      expand.grid(m[x,1]:m[x+1,1], m[x,2]:m[x+1,2])
    }) 
  ) |> unlist() |> matrix(ncol = 2) |> rbind(m) 
}

map_cave <- function(inp, origin = c(500,0)) {
  walls <- Reduce(rbind, lapply(inp, function(x) expand(str2idx(x))))
  dx <- max(origin[,1], walls[,1])
  dy <- max(origin[,2], walls[,2])
  cave <- matrix(rep(0, dx * dy), nrow =  dx)
  cave[walls] <- 1
  cave
}

draw <- function(cave) {
  apply(cave, 2, function(x) paste0(c(".","#", "S")[x+1], collapse = "")) |>
    cat(sep = "\n")
}

move <- function(pos, cave) {
  for (dir in list(idx(c(0, 1)), idx(c(-1, 1)), idx(c(1, 1)))) {
    if (!valid(pos + dir, dim(cave))) return(NULL)
    if (cave[pos + dir] == 0) return(pos + dir)
  }
  pos
}

pour <- function(cave, origin) {
  pos <- origin
  tot <- 0
  repeat {
    new_pos <- move(pos, cave)
    if (is.null(new_pos)) return(tot)
    if (all(pos == new_pos) && all(new_pos == origin)) return(tot + 1)# top
    if (all(pos == new_pos)) { # sand comes to rest
      cave[pos] <- 1
      # new sand
      tot <- tot + 1
      pos <- origin
    } else {
      pos <- new_pos
    }
  }
}

but_bigger <- function(m) {
  wider <- rbind(m, matrix(rep(0, ncol(m) * nrow(m)), ncol = ncol(m)))
  taller <- cbind(wider, matrix(c(rep(0, 2*nrow(m)),rep(1, 2*nrow(m))), ncol=2))
  taller
}

cave <- map_cave(inp, origin)
message("Part 1: ", pour(cave, origin)) 
message("Part 2: ", pour(but_bigger(cave), origin))

