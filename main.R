library(optparse)

# Define command line options
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
    c("--verbose"),
    action  = "store_true",
    default = FALSE,
    help    = "Print verbose output during execution"
  )
)

# Parse command line options
opt_parser <- OptionParser(
  option_list     = option_list,
  add_help_option = TRUE,
  description     = "R project for the EU Legal Needs Country Report"
)
opt <- parse_args(opt_parser)

# main.R Entry Point
main <- function(){
  
  renv::activate()
  
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
  
}


if(!interactive()){
  main()
  quit(save = "no", status = 0)
}