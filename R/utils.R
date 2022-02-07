create_db_pr_paths <- function(paths = NULL) {

  ###### too risky to create something unwanted (hasnt happend yet, but function should be handled with care)
  # if (is.null(paths)) {
  #   for(path in ls(envir = .GlobalEnv)[stringr::str_detect(ls(envir = .GlobalEnv), "path_db_pr")]) {
  #     current_path <- get(envir = .GlobalEnv, path)
  #
  #     if(!fs::dir_exists(current_path)) {
  #       fs::dir_create(current_path)
  #       message(paste("Just created", current_path))
  #     }
  #   }
  # }

  if (!is.null(paths)) {
    for (path in paths) {
      current_path <- path

      if (!fs::dir_exists(current_path)) {
        fs::dir_create(current_path)
        message(paste("Just created", current_path))
      }
    }
  }
}

show_folder_structure <- function(path_pattern = "path") {
  all_paths <- NA

  for (i in 1:length(ls(envir = .GlobalEnv)[stringr::str_detect(ls(envir = .GlobalEnv), path_pattern)])) {
    current_path <- get(envir = .GlobalEnv, ls(envir = .GlobalEnv)[stringr::str_detect(ls(envir = .GlobalEnv), path_pattern)][i])

    current_path <- ifelse(is.function(current_path), NA, current_path)

    all_paths <- c(all_paths, current_path)
  }

  all_paths <- all_paths[!is.na(all_paths)]

  all_paths <- lapply(strsplit(all_paths, "/"), function(all_paths) as.data.frame(t(all_paths)))
  all_paths <- plyr::rbind.fill(all_paths)
  all_paths$pathString <- apply(all_paths, 1, function(all_paths) paste(trimws(stats::na.omit(all_paths)), collapse = "/"))
  all_paths <- all_paths[order(all_paths$pathString), ]

  all_paths <- data.tree::as.Node(all_paths)

  print(all_paths)
  plot(all_paths)
}

show_diff_rows <- function(data, initial_n_rows, cause = "") {
  diff <- initial_n_rows - nrow(data)
  if (diff > 0) cat(crayon::red("\n", diff, "rows have been removed", cause, "\n", "\n"))
  if (diff == 0) cat(crayon::green("\n", "Number of rows has not changed", cause, "\n", "\n"))
  if (diff < 0) cat(crayon::blue("\n", -diff, "rows have been added", cause, "\n", "\n"))

  return(data)
}
