inp <- readLines("07.txt", warn = FALSE)

fps <- function(fp) paste0(fp, collapse = "_")
path <- function(fp, nw) if (nw == "..") { fp[-length(fp)] } else { c(fp, nw) }

get_dir_structure <- function(inp) {
  dirs <- c(top = 0)
  fp <- c("top")
  for (command in inp[-1]) {
    if (grepl("^dir", command)) {
      dirs <- c(dirs, setNames(0, fps(c(fp, gsub("^dir ", "", command)))))
    } else if (grepl("^[0-9]+ .*", command)) {
      dirs[fps(fp)] <- dirs[fps(fp)] + as.numeric(gsub(" .*", "", command))
    } else if (grepl("\\$ cd ", command)) {
      fp <- path(fp, gsub("\\$ cd ", "",command))
    }
  }
  dirs
}

dir_sizes <- function(dirs, lim = 100000) {
  opts <- setNames(rep(0, length(dirs)), names(dirs))
  for (nm in names(dirs)) {
    sub_dirs <- c(nm, names(dirs)[grepl(paste0("^", nm, "_"), names(dirs))])
    opts[nm]<- sum(dirs[sub_dirs])
  }
  opts
}

dirs <- get_dir_structure(inp)
sz <- dir_sizes(dirs)
message("Part 1: ", sum(sz[sz < 100000]))
message("Part 2: ", min(sz[sz > 30000000 - (70000000 - sum(dirs))]))
