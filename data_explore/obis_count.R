### support functions for obis_count()

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
      obis_occ <- NA
    } else {
      obis_occ <- res$count
    }
    tmp_df <- data.frame('sciname' = sciname, 'occ_count' = obis_occ, stringsAsFactors = FALSE)
    
    ptm <- round((proc.time() - ptm)[["elapsed"]], 2)
    if(verbose) message(i, '. Species ', sciname, '; Occurrence count: ', obis_occ, '; Total time: ', ptm, ' seconds')
    else if(i == round(i, -2)) message('# species processed: ', i)
    
    occ_list[[i]] <- tmp_df
  }
  
  occ_df <- rbind_all(occ_list)

  return(occ_df)
}
