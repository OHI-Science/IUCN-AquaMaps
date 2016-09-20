### support functions for obis_count()
library(httr)
library(jsonlite)

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

obis_occurrence <- function(
  scientificname = NULL,
  year = NULL,
  obisid = NULL,
  aphiaid = NULL,
  resourceid = NULL,
  startdate = NULL,
  enddate = NULL,
  startdepth = NULL,
  enddepth = NULL,
  geometry = NULL,
  qc = NULL,
  fields = NULL,
  verbose = FALSE) {
  
  
  offset <- 0
  i <- 1
  lastpage <- FALSE
  total <- 0
  datalist <- list()
  
  t <- proc.time()
  
  while (!lastpage) {
    query <- list(scientificname = scientificname,
                  year = year,
                  obisid = obisid,
                  aphiaid = aphiaid,
                  resourceid = resourceid,
                  # startdate = handle_date(startdate),
                  # enddate = handle_date(enddate),
                  startdepth = startdepth,
                  enddepth = enddepth,
                  geometry = geometry
                  # qc = handle_vector(qc),
                  # fields = handle_vector(fields),
                  # offset = format(offset, scientific=FALSE)
                  )
    
    # use POST for complex geometries, only GET is cached
    if (!is.null(geometry) && nchar(geometry) > max_characters()) {
      result <- http_request("POST", "occurrence", query)
    } else {
      result <- http_request("GET", "occurrence", query)
    }
    
    stop_for_status(result)
    text <- content(result, "text", encoding="UTF-8")
    res <- fromJSON(text, simplifyVector=TRUE)
    
    if(!is.null(res$message)) {
      lastpage = TRUE
      warning(res$message)
    } else {
      limit <- res$limit
      offset <- offset + limit
      lastpage <- res$lastpage
      if(res$count > 0) {
        datalist[[i]] <- res$results
        total <- total + nrow(res$results)
        log_progress(total, res$count)
        i <- i + 1
      }
    }
  }
  cat("\n")
  if (verbose) {
    cat("Total time:", (proc.time() - t)[["elapsed"]], "seconds\n")
  }
  
  data <- rbind_all(datalist)
  
  if(!is.null(fields)) {
    missing_fields <- setdiff(fields, colnames(data))
    if(length(missing_fields) > 0) {
      warning("Following fields where not found and initialized to NA: ", paste0(missing_fields, collapse = ", "))
      data[,missing_fields] <- NA
    }
    for (extra_col in setdiff(colnames(data), fields)) { # remove fields that were not requested
      data[,extra_col] <- NULL
    }
    data <- data[, fields] # re-order columns to the expected order
  }
  return(data)
}

x <- obis_occurrence(scientificname = 'Culeolus herdmani')
