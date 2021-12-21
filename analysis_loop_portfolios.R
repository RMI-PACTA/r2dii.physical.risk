path_db_pacta_project_pr_output_base <- path_db_pacta_project_pr_output

projects <- list.dirs(
  fs::path(path_db_pacta_project, "30_Processed_Inputs"),
  recursive = FALSE
)

# PACTA project output path
path_db_pacta_project_pr_output <- fs::path(
  path_db_pacta_project,
  "60_Physical_Risk"
)

# create PACTA project output path
r2dii.physical.risk:::create_db_pr_paths(
  paths = c(path_db_pacta_project_pr_output)
)

for (project in projects) {

  base_project_dir <- fs::path_file(project)

  path_db_pacta_project_pr_output <- fs::path(
    path_db_pacta_project_pr_output_base,
    base_project_dir
    )

  fs::dir_create(
    path_db_pacta_project_pr_output
  )

  total_portfolio_raw <- readRDS(
    fs::path(
      project,
      base::paste0("total_portfolio"),
      ext = "rda"
    )
  )

  total_portfolio <- r2dii.physical.risk:::format_portfolio_data(
    total_portfolio_raw
  )

  source("analysis.R")

}
