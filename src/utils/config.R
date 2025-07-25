## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Configuration file
## Author(s):         Carlos Toruno (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     July 22, 2025
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SharePoint Path
if (Sys.info()["user"] == "carlostoruno") {
  path2EU <- "/Users/carlostoruno/OneDrive - World Justice Project/EU Subnational/EU-S Data"
  path2DA <- "/Users/carlostoruno/OneDrive - World Justice Project/Data Analytics"
}

# Helper function to print verbose messages
verbose_message <- function(message) {
  if (opt$verbose) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    cat(paste0("[INFO ", timestamp, "] ", message, "\n"))
  }
}