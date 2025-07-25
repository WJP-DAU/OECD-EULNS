load_data <- function(path2EU, source){
  
  if (!(source %in% c("gpp", "subset", "regions"))){
    stop("'source' argument should be one of: 'gpp', 'subset', 'regions'")
  }
  
  if (source == "gpp"){
    path2data <- file.path(
      path2EU,
      "eu-gpp", "1. Data", "3. Merge", "EU_GPP_2024.dta"
    )
    
    if (file.exists(path2data)){
      df <- haven::read_stata(path2data)
    } else {
      stop(glue::glue("File '{path2data}' does not exist."))
    }
  }
  
  if (source == "subset"){
    path2data <- "data/master.csv"

    if (file.exists(path2data)){
      df <- read_csv(
        path2data, 
        show_col_types = FALSE
      )
    } else {
      stop(glue("File '{path2data}' does not exist."))
    }
  }
  
  if (source == "regions"){
    path2data <- "data/regions.csv"
    
    if (file.exists(path2data)){
      df <- read_csv(
        path2data, 
        show_col_types = FALSE
      )
    } else {
      stop(glue("File '{path2data}' does not exist."))
    }
  }
  
  return(df)
}