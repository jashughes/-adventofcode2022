inp <- readLines("07.txt", warn = FALSE)

fps <- function(fp) paste0(fp, collapse = "_")
path <- function(fp, nw) if (nw == "..") { fp[-length(fp)] } else { c(fp, nw) }
sub_dirs <- function(fp, d) c(fp, names(d)[grepl(paste0("^",fp,"_"), names(d))])
dir_sizes <- function(d) sapply(names(d), function(x) sum(d[sub_dirs(x, d)]))

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

dirs <- get_dir_structure(inp)
sz <- dir_sizes(dirs)
message("Part 1: ", sum(sz[sz < 100000]))
message("Part 2: ", min(sz[sz > 30000000 - (70000000 - sum(dirs))]))
