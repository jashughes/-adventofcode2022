inp <- readLines("07.txt", warn = FALSE)

fps <- function(fp) paste0(fp, collapse = "_")

get_dir_structure <- function(inp) {
  instr <- which(grepl("\\$ ", inp))[-1]
  dirs <- c(top = 0)
  fp <- c("top")
  for (i in instr) {
    command <- inp[i]
    if (grepl(" ls$", command)) {
      ls_out <- inp[(i+1):(pmin(instr[instr > i][1] - 1, length(inp), na.rm=T))]
      new_dirs <- gsub("dir ", "", ls_out[grepl("^dir" , ls_out)])
      if (length(new_dirs) > 0) {
        new_path <- paste0(fps(fp), "_", new_dirs)
        dirs <- c(dirs, setNames(rep(0, length(new_dirs)), new_path))
      }
      new_files <- as.numeric(gsub(" .*", "", ls_out[!grepl("^dir" , ls_out)]))
      dirs[fps(fp)] <- sum(new_files)
      
    } else {
      new_path <- gsub("\\$ cd ", "",command)
      if (new_path == "..") {
        fp <- fp[-length(fp)]
      } else {
        fp <- c(fp, new_path)
      }
    }
  }
  dirs
}

sum_of_small_dirs <- function(dirs, lim = 100000) {
  tot <- 0
  for (nm in names(dirs)) {
    sub_dirs <- c(nm, names(dirs)[grepl(paste0("^", nm, "_"), names(dirs))])
    dir_tot <- sum(dirs[sub_dirs])
    if (dir_tot < lim) tot <- tot + dir_tot
  }
  tot
}

dir_to_delete <- function(dirs, must_delete) {
  opts <- setNames(rep(0, length(dirs)), names(dirs))
  for (nm in names(dirs)) {
    sub_dirs <- c(nm, names(dirs)[grepl(paste0("^", nm, "_"), names(dirs))])
    dir_tot <- sum(dirs[sub_dirs])
    opts[nm] <- dir_tot
  }
  min(opts[opts > must_delete])
}

dirs <- get_dir_structure(inp)
message("Part 1: ", sum_of_small_dirs(dirs))
message("Part 2: ", dir_to_delete(dirs, 30000000 - (70000000 - sum(dirs))))