# get_shab_data.R

Get_Shab_DF <- function(download_date) {
  library(httr)
  library(XML)
  library(dplyr)

  pickles_folder <- "./shab_data"
  if (!dir.exists(pickles_folder)) {
    dir.create(pickles_folder)
  }

  import_folder <- "./import"
  if (!dir.exists(import_folder)) {
    dir.create(import_folder)
  }

  download_date_str <- format(as.Date(download_date), "%Y-%m-%d")
  pickle_file <- file.path(pickles_folder, paste0("shab-", download_date_str, ".RDS"))

  if (file.exists(pickle_file)) {
    return(readRDS(pickle_file))
  } else {
    data <- list()
    pages <- c(0, 1)
    for (page in pages) {
      xmlfile <- file.path(import_folder, paste0("shab_", download_date_str, "_", page + 1, ".xml"))

      url <- paste0(
        "https://amtsblattportal.ch/api/v1/publications/xml?publicationStates=PUBLISHED&tenant=shab&rubrics=HR&rubrics=KK&rubrics=LS&rubrics=NA&rubrics=SR&publicationDate.start=",
        download_date_str, "&publicationDate.end=", download_date_str,
        "&pageRequest.size=3000&pageRequest.sortOrders&pageRequest.page=",
        page
      )

      r <- GET(url)
      writeBin(content(r, "raw"), xmlfile)

      tree <- xmlTreeParse(xmlfile, useInternalNodes = TRUE)
      root <- xmlRoot(tree)
      file.remove(xmlfile)

      tryCatch(
        {
          rls_list <- getNodeSet(root, "./publication/meta")
          for (rls in rls_list) {
            inner <- list(
              id = xmlValue(rls[["id"]]),
              date = xmlValue(rls[["publicationDate"]]),
              title = xmlValue(rls[["title"]][["de"]]),
              rubric = xmlValue(rls[["rubric"]]),
              subrubric = xmlValue(rls[["subRubric"]]),
              publikations_status = xmlValue(rls[["publicationState"]]),
              primaryTenantCode = xmlValue(rls[["primaryTenantCode"]]),
              kanton = xmlValue(rls[["cantons"]])
            )
            data <- append(data, list(inner))
          }
        },
        error = function(e) {
          cat("Failed to process", xmlfile, ":", e$message, "\n")
        }
      )
    }

    df <- bind_rows(data)
    if (nrow(df) > 0) {
      df <- df %>% filter(subrubric == "HR01" | subrubric == "HR03")
    }

    saveRDS(df, pickle_file)
    return(df)
  }
}
