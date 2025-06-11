# get_shab_data_range.R

library(httr)
library(XML)
library(dplyr)
library(tibble) # Add if not present

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
  
  df <- NULL

  if (file.exists(pickle_file)) {
    df <- readRDS(pickle_file)
  } else {
    data_list_for_date <- list()
    current_page <- 0
    max_results_per_page <- 3000

    repeat {
      xmlfile <- file.path(import_folder, paste0('shab_', download_date_str, '_', current_page + 1, '.xml'))
      
      url <- paste0('https://amtsblattportal.ch/api/v1/publications/xml?publicationStates=PUBLISHED&tenant=shab&rubrics=HR&rubrics=KK&rubrics=LS&rubrics=NA&rubrics=SR&publicationDate.start=', 
                    download_date_str, '&publicationDate.end=', download_date_str, 
                    '&pageRequest.size=', max_results_per_page, '&pageRequest.sortOrders&pageRequest.page=',
                    current_page)
      
      r <- NULL
      http_error_occurred <- FALSE
      try_get_result <- try(r <- GET(url), silent = TRUE)

      if (inherits(try_get_result, "try-error")) {
          cat('Failed GET request for date', download_date_str, 'page', current_page, ':', as.character(try_get_result), '\n')
          http_error_occurred <- TRUE
      } else {
          tryCatch({
              httr::stop_for_status(r)
          }, error = function(e) {
              cat('HTTP error for date', download_date_str, 'page', current_page, ':', e$message, '\n')
              http_error_occurred <<- TRUE
          })
      }

      if(http_error_occurred) {
          if (file.exists(xmlfile)) { file.remove(xmlfile) }
          break
      }
      
      if(is.null(r)){
          cat('GET request did not return a response object for date', download_date_str, 'page', current_page, '\n')
          if (file.exists(xmlfile)) { file.remove(xmlfile) }
          break
      }

      writeBin(content(r, "raw"), xmlfile)
      
      publications_on_page <- 0
      tree <- NULL
      tryCatch({
        tree <- xmlTreeParse(xmlfile, useInternalNodes = TRUE)
        root <- xmlRoot(tree)

        rls_list <- getNodeSet(root, './publication/meta')
        if (length(rls_list) == 0) {
          file.remove(xmlfile)
          break
        }
        publications_on_page <- length(rls_list)

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
          data_list_for_date <- append(data_list_for_date, list(inner))
        }
        file.remove(xmlfile)
      }, error = function(e) {
        cat('Failed to process XML content for date', download_date_str, 'page', current_page, ':', e$message, '\n')
        if (file.exists(xmlfile)) {
            file.remove(xmlfile)
        }
        break
      })

      if (publications_on_page < max_results_per_page) {
        break
      }

      current_page <- current_page + 1
      if (current_page > 100) {
          cat("Warning: Exceeded 100 pages for date", download_date_str, ". Assuming no more data.\n")
          if (file.exists(xmlfile)) { file.remove(xmlfile) }
          break
      }
    } # end repeat
    
    df <- dplyr::bind_rows(data_list_for_date) # Added dplyr:: for clarity
    if (nrow(df) > 0) {
      df <- df %>% dplyr::filter(subrubric == "HR01" | subrubric == "HR03") # Added dplyr::
    }
    
    saveRDS(df, pickle_file)
  } # end else (new data download block)
  
  expected_cols_spec <- tibble::tibble(
    id = character(), date = as.Date(character()), title = character(),
    rubric = character(), subrubric = character(), publikations_status = character(),
    primaryTenantCode = character(), kanton = character()
  )

  if (is.null(df)) {
      df <- expected_cols_spec
  } else if (nrow(df) > 0) {
    for (col_name in names(expected_cols_spec)) {
        if (!col_name %in% names(df)) {
            df[[col_name]] <- expected_cols_spec[[col_name]][0][NA_integer_]
        }
    }
    if ('date' %in% names(df)) {
        df$date <- as.Date(df$date)
    } else {
        df$date <- as.Date(NA_character_)
    }
  } else { # nrow(df) == 0
    df <- expected_cols_spec
  }
  
  return(df)
}

Get_Shab_DF_from_range <- function(from_date, to_date) {
  # Ensure from_date and to_date are Date objects
  from_date <- as.Date(from_date)
  to_date <- as.Date(to_date)

  main_pickle <- './shab_data/last_df.RDS'
  
  # Helper to get a standard empty dataframe structure
  get_empty_df_structure <- function() {
    # Use a known date (like today or from_date) just to get the structure from Get_Shab_DF
    # Get_Shab_DF is guaranteed to return a correctly structured tibble (possibly empty)
    temp_date_for_struct <- if (!is.na(from_date)) from_date else Sys.Date()
    empty_df <- Get_Shab_DF(temp_date_for_struct)
    return(empty_df[0, ]) # Return 0 rows, but with correct columns and types
  }

  df_Result <- NULL

  if (file.exists(main_pickle)) {
    df_Result <- readRDS(main_pickle)
    
    # Validate and standardize df_Result
    if (is.null(df_Result) || !is.data.frame(df_Result) || ncol(df_Result) == 0) {
      df_Result <- get_empty_df_structure()
    } else {
      # Ensure 'date' column exists and is Date type
      if (!'date' %in% names(df_Result)) {
        df_Result$date <- as.Date(NA_character_)
      } else {
        df_Result$date <- as.Date(df_Result$date)
      }
      # Ensure all other expected columns are present (important if old RDS format differs)
      expected_cols <- names(get_empty_df_structure())
      for(col_name in expected_cols){
          if(!col_name %in% names(df_Result)){
              df_Result[[col_name]] <- get_empty_df_structure()[[col_name]][0][NA_integer_]
          }
      }
      # Select columns in expected order to ensure consistency
      df_Result <- df_Result[, expected_cols, drop = FALSE]
    }
  } else {
    df_Result <- get_empty_df_structure()
  }

  min_existing_date <- if (nrow(df_Result) > 0 && any(!is.na(df_Result$date))) min(df_Result$date, na.rm = TRUE) else NA
  max_existing_date <- if (nrow(df_Result) > 0 && any(!is.na(df_Result$date))) max(df_Result$date, na.rm = TRUE) else NA

  # Check for full coverage if existing data is valid
  if (!is.na(min_existing_date) && !is.na(max_existing_date) &&
      min_existing_date <= from_date && max_existing_date >= to_date) {
    # Already covered, just filter and return
    return(df_Result %>% dplyr::filter(date >= from_date & date <= to_date))
  }

  new_data_list <- list()
  dates_to_fetch <- c()

  # Determine dates to fetch before existing data
  dates_to_prepend <- c()
  if (is.na(min_existing_date) || min_existing_date > from_date) {
    actual_end_prepend <- if (is.na(min_existing_date)) to_date else min(to_date, min_existing_date - 1)
    if (actual_end_prepend >= from_date) {
      dates_to_prepend <- seq(from_date, actual_end_prepend, by = "day")
    }
  }

  # Determine dates to fetch after existing data
  dates_to_append <- c()
  if (is.na(max_existing_date) || max_existing_date < to_date) {
    actual_start_append <- if (is.na(max_existing_date)) from_date else max(from_date, max_existing_date + 1)
    if (to_date >= actual_start_append) {
      dates_to_append <- seq(actual_start_append, to_date, by = "day")
    }
  }

  all_dates_to_fetch <- unique(c(dates_to_prepend, dates_to_append))

  if (length(all_dates_to_fetch) > 0) {
    for (date_val in sort(all_dates_to_fetch)) {
      df_day <- Get_Shab_DF(date_val)
      if (nrow(df_day) > 0) {
        new_data_list[[length(new_data_list) + 1]] <- df_day
      }
    }
  }

  if (length(new_data_list) > 0) {
    new_data_combined <- dplyr::bind_rows(new_data_list)
    # Ensure df_Result is a valid tibble for bind_rows
    if (nrow(df_Result) == 0 && ncol(df_Result) == 0 && !("tbl_df" %in% class(df_Result)) ){
        df_Result <- get_empty_df_structure()
    } else if (!identical(sapply(df_Result, class), sapply(get_empty_df_structure(), class))){
        # Coerce df_Result to expected structure if classes mismatch (e.g. old RDS format)
        # This is a bit aggressive; assumes column names are compatible for conversion by bind_rows
        # A more robust way might be to select common columns or convert types individually.
        # For now, rely on Get_Shab_DF's consistency and previous standardization of df_Result.
    }

    df_Result <- dplyr::bind_rows(df_Result, new_data_combined)
    df_Result <- dplyr::distinct(df_Result, dplyr::across(tidyselect::everything()))
  }

  # Final filter for the requested date range
  if (nrow(df_Result) > 0) {
    df_Result <- df_Result %>% dplyr::filter(date >= from_date & date <= to_date)
  } else {
    df_Result <- get_empty_df_structure() # Ensure standard empty if no data in range
  }

  saveRDS(df_Result, main_pickle)
  return(df_Result)
}


