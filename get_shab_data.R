# get_shab_data_range.R

library(httr)
library(XML)
library(dplyr)

# Function to get data for a specific date
Get_Shab_DF <- function(download_date) {
  pickles_folder <- './shab_data'
  if (!dir.exists(pickles_folder)) {
    dir.create(pickles_folder)
  }
  
  import_folder <- './import'
  if (!dir.exists(import_folder)) {
    dir.create(import_folder)
  }
  
  download_date_str <- format(as.Date(download_date), "%Y-%m-%d")
  pickle_file <- file.path(pickles_folder, paste0('shab-', download_date_str, '.RDS'))
  
  if (file.exists(pickle_file)) {
    df <- readRDS(pickle_file)
  } else {
    data <- list()
    pages <- c(0, 1)
    for (page in pages) {
      xmlfile <- file.path(import_folder, paste0('shab_', download_date_str, '_', page + 1, '.xml'))
      
      url <- paste0('https://amtsblattportal.ch/api/v1/publications/xml?publicationStates=PUBLISHED&tenant=shab&rubrics=HR&rubrics=KK&rubrics=LS&rubrics=NA&rubrics=SR&publicationDate.start=', 
                    download_date_str, '&publicationDate.end=', download_date_str, 
                    '&pageRequest.size=3000&pageRequest.sortOrders&pageRequest.page=', 
                    page)
      
      r <- GET(url)
      writeBin(content(r, "raw"), xmlfile)
      
      tree <- xmlTreeParse(xmlfile, useInternalNodes = TRUE)
      root <- xmlRoot(tree)
      file.remove(xmlfile)
      
      tryCatch({
        rls_list <- getNodeSet(root, './publication/meta')
        for (rls in rls_list) {
          inner <- list(
            id = xmlValue(rls[['id']]),
            date = xmlValue(rls[['publicationDate']]),
            title = xmlValue(rls[['title']][['de']]),
            rubric = xmlValue(rls[['rubric']]),
            subrubric = xmlValue(rls[['subRubric']]),
            publikations_status = xmlValue(rls[['publicationState']]),
            primaryTenantCode = xmlValue(rls[['primaryTenantCode']]),
            kanton = xmlValue(rls[['cantons']])
          )
          data <- append(data, list(inner))
        }
      }, error = function(e) {
        cat('Failed to process', xmlfile, ':', e$message, '\n')
      })
    }
    
    df <- bind_rows(data)
    if (nrow(df) > 0) {
      df <- df %>% filter(subrubric == "HR01" | subrubric == "HR03")
    }
    
    saveRDS(df, pickle_file)
  }
  
  if (!'date' %in% names(df)) {
    df$date <- as.Date(NA)
  }
  
  return(df)
}

# Function to get data for a range of dates
Get_Shab_DF_from_range <- function(from_date, to_date) {
  df_Result <- NULL
  main_pickle <- './shab_data/last_df.RDS'
  
  if (file.exists(main_pickle)) {
    df_Result <- readRDS(main_pickle)
    if (!'date' %in% names(df_Result)) {
      df_Result$date <- as.Date(NA)
    } else {
      df_Result$date <- as.Date(df_Result$date)
    }
    
    if (min(df_Result$date, na.rm = TRUE) <= (from_date + 3) && max(df_Result$date, na.rm = TRUE) >= (to_date - 3)) {
      df_Result <- df_Result %>% filter(date >= from_date & date <= to_date)
      return(df_Result)
    }
    
    if (min(df_Result$date, na.rm = TRUE) > (from_date + 3)) {
      for (date in seq(from_date, min(df_Result$date, na.rm = TRUE) - 1, by = "day")) {
        df <- Get_Shab_DF(date)
        if (!'date' %in% names(df)) {
          df$date <- as.Date(NA)
        } else {
          df$date <- as.Date(df$date)
        }
        df_Result <- bind_rows(df_Result, df)
      }
      saveRDS(df_Result, main_pickle)
      return(df_Result)
    }
    
    if (max(df_Result$date, na.rm = TRUE) < (to_date - 3)) {
      for (date in seq(max(df_Result$date, na.rm = TRUE) + 1, to_date, by = "day")) {
        df <- Get_Shab_DF(date)
        if (!'date' %in% names(df)) {
          df$date <- as.Date(NA)
        } else {
          df$date <- as.Date(df$date)
        }
        df_Result <- bind_rows(df_Result, df)
      }
      saveRDS(df_Result, main_pickle)
      return(df_Result)
    }
  } else {
    for (date in seq(from_date, to_date, by = "day")) {
      df <- Get_Shab_DF(date)
      if (!'date' %in% names(df)) {
        df$date <- as.Date(NA)
      } else {
        df$date <- as.Date(df$date)
      }
      if (is.null(df_Result)) {
        df_Result <- df
      } else {
        df_Result <- bind_rows(df_Result, df)
      }
    }
    saveRDS(df_Result, main_pickle)
    return(df_Result)
  }
}
