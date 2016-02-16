### support functions for obis_count()
library(httr)

obis_url <- function() {
  getOption("robis_url", "http://api.iobis.org/")
}

max_characters <- function() {
  getOption("robis_max_characters", 50000)
}

handle_date <- function(date) {
  if(!is.null(date) && class(date) == "Date") {
    as.character(date)
  } else {
    date
  }
}

handle_vector <- function(x) {
  if(!is.null(x)) {
    paste0(x, collapse = ",")
  } else {
    x
  }
}

log_request <- function(result) {
  cat("\n", paste(result$request$method, result$request$url, result$status_code, result$headers$age, result$times[["total"]]), "\n", sep="")
}

log_progress <- function(total, count) {
  cat("\rRetrieved ", total, " records of ", count, " (", floor(total/count*100),"%)", sep="")
}

http_request <- function(method, path, query) {
  if (method == "GET") {
    httr::GET(obis_url(), user_agent("robis - https://github.com/iobis/robis"),
        path = path, query = query)
  } else if (method == "POST") {
    httr::POST(obis_url(), user_agent("robis - https://github.com/iobis/robis"),
         path = path, body = query)
  }
}

##########################################################################

obis_count <- function(
  scinames,
  verbose = FALSE) {

  offset <- 0
  i <- 0
  occ_list <- list() ### initialize occurrence count list
  ptm_total <- proc.time()
  
  for (sciname in scinames) {
    i <- i + 1
    lastpage <- FALSE
    ptm <- proc.time()
    
    query <- list(scientificname = sciname,
                  offset = format(offset, scientific = FALSE))
    
    result <- http_request("GET", "occurrence", query)
    
    stop_for_status(result)
    res <- content(result)
    
    if(!is.null(res$message)) {
      lastpage = TRUE
      warning(res$message)
      occ_count <- NA
    } else {
      occ_count <- res$count
    }
    tmp_df <- data.frame('sciname' = sciname, 'obis_occ' = occ_count, stringsAsFactors = FALSE)
    
    if(verbose) {
      ptm <- round((proc.time() - ptm)[["elapsed"]], 2)
      message(i, '. Species ', sciname, '; Occurrence count: ', occ_count, 
              '; Elapsed time: ', ptm, ' seconds')
    } else if(i == round(i, -2)) {
      ptm <- round((proc.time() - ptm_total)[["elapsed"]], 2)
      message(i, ' species processed of ', length(scinames), 
              '; Total time: ', ptm, ' seconds')
    } 
    
    occ_list[[i]] <- tmp_df
  }
  
  ptm <- round((proc.time() - ptm_total)[["elapsed"]], 2)
  message('Total: ', i, ' species processed; Total time: ', ptm, ' seconds')
  
  occ_df <- rbind_all(occ_list)

  return(occ_df)
}
