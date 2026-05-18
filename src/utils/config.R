## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Configuration file
## Author(s):         Carlos Toruno (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     July 22, 2025
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

if (!nzchar(Sys.getenv("CHROMOTE_CHROME"))) {
  chrome_candidates <- c(
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Google Chrome .app/Contents/MacOS/Google Chrome",
    "/opt/homebrew/bin/chromium"
  )

  chrome_path <- chrome_candidates[file.exists(chrome_candidates)][1]

  if (!is.na(chrome_path)) {
    Sys.setenv(CHROMOTE_CHROME = chrome_path)
  }
}

# SharePoint Path
if (Sys.info()["user"] == "carlostoruno") {
  path2EU <- "/Users/carlostoruno/OneDrive - World Justice Project/EU Subnational/EU-S Data"
  path2DA <- "/Users/carlostoruno/OneDrive - World Justice Project/Data Analytics"
} else if (Sys.info()["user"] == "santiagopardo") {
  path2EU <- "/Users/santiagopardo/Library/CloudStorage/OneDrive-WorldJusticeProject/EU Subnational/EU-S Data"
  path2DA <- "/Users/santiagopardo/Library/CloudStorage/OneDrive-WorldJusticeProject/Data Analytics"
}

# Helper function to print verbose messages
verbose_message <- function(message) {
  if (opt$verbose) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    cat(paste0("[INFO ", timestamp, "] ", message, "\n"))
  }
}
