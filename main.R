# main.R Entry Point
main <- function(){
  os_dir <- switch(
    Sys.info()[["sysname"]],
    "Darwin" = "macos",
    "Linux" = "linux",
    "Windows" = "windows",
    tolower(Sys.info()[["sysname"]])
  )

  r_version <- paste(
    R.version[["major"]],
    strsplit(R.version[["minor"]], "[.]")[[1]][1],
    sep = "."
  )

  project_library <- file.path(
    getwd(),
    "renv",
    "library",
    os_dir,
    paste0("R-", r_version),
    R.version[["platform"]]
  )

  if (dir.exists(project_library)) {
    .libPaths(unique(c(project_library, .libPaths())))
  }

  library(optparse)

  option_list <- list(
    make_option(
      c("--data"),
      action  = "store_true",
      default = FALSE,
      help    = "Perform data loading and wrangling routines"
    ),
    make_option(
      c("--analysis"),
      action  = "store_true",
      default = FALSE,
      help    = "Perform data analysis routines"
    ),
    make_option(
      c("--viz"),
      action  = "store_true",
      default = FALSE,
      help    = "Perform data visualization routines"
    ),
    make_option(
      c("--main_indicators"),
      action  = "store_true",
      default = FALSE,
      help    = "Produce a country-level indicators table for all study variables"
    ),
    make_option(
      c("--verbose"),
      action  = "store_true",
      default = FALSE,
      help    = "Print verbose output during execution"
    ),
    make_option(
      c("--update-packages"),
      action  = "store_true",
      default = FALSE,
      help    = "Update project packages with renv and snapshot the lockfile"
    )
  )

  opt_parser <- OptionParser(
    option_list     = option_list,
    add_help_option = TRUE,
    description     = "R project for the EU Legal Needs Country Report"
  )
  opt <- parse_args(opt_parser)
  assign("opt", opt, envir = .GlobalEnv)

  if (opt$`update-packages`) {
    if (!requireNamespace("renv", quietly = TRUE)) {
      stop("Package 'renv' is required to update project packages.")
    }

    renv::update(prompt = FALSE, project = ".")
    renv::snapshot(prompt = FALSE, project = ".")
    return(invisible(NULL))
  }

  source("src/utils/config.R")
  
  if (opt$data){
    verbose_message("Performing data wrangling routine:")
    source("src/data/worker-data.R")
    verbose_message("Data wrangling routine successful ✅")
  }
  if (opt$analysis){
    verbose_message("Performing data analysis routines:")
    source("src/analysis/worker-analysis.R")
    verbose_message("Data analysis successful ✅")
  }
  if (opt$main_indicators){
    verbose_message("Performing main indicators routine:")
    suppressWarnings(
      suppressMessages(
        library("tidyverse", quietly = TRUE)
      )
    )
    source("src/utils/utils.R")
    source("src/data/data-loading.R")
    source("src/analysis/main_indicators.R")
    verbose_message("--- Loading data for main indicators...")
    master <- load_data(
      path2EU = path2EU,
      source = "subset"
    )
    regions <- load_data(
      path2EU = path2EU,
      source = "regions"
    )
    verbose_message("--- Building main indicators table...")
    main_indicators(master, regions)
    verbose_message("Main indicators routine successful ✅")
  }
  if (opt$viz){
    verbose_message("Performing data visualization routines:")
    source("src/viz/worker-viz.R")
    verbose_message("Data visualization successful ✅")
  }
  
}


if(!interactive()){
  main()
  quit(save = "no", status = 0)
}
