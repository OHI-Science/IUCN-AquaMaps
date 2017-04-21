library(tidyr)
library(dplyr)
library(stringr)
library(readr)
library(rgbif)


### support functions for obis_count()
library(httr)
library(jsonlite)

obis_url <- function() {
  getOption("robis_url", "http://api.iobis.org/")
}

max_characters <- function() {
  getOption("robis_max_characters", 50000)
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

obis_occurrence <- function(scientificname,  verbose = FALSE) {
  
  offset <- 0
  i <- 1
  lastpage <- FALSE
  total <- 0
  datalist <- list()
  
  t <- proc.time()
  
  while (!lastpage) {
    query <- list(scientificname = scientificname)
    
    result <- http_request("GET", "occurrence", query)
    
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
        if(verbose) message("Retrieved ", total, " records of ", res$count, " (", floor(total/res$count*100),"%)", sep="")
        i <- i + 1
      }
    }
  }

  if (verbose) {
    message("Total time:", (proc.time() - t)[["elapsed"]], "seconds")
  }
  
  data <- bind_rows(datalist)
  
  return(data)
}

### spp_ex copied from fao_clip_pac script - these are four species with interesting maps
spp_ex <- c('Hoplichthys regani' = 'Fis-34036', 
            'Ipnops meadi'       = 'Fis-143854', 
            'Culeolus herdmani'  = 'URM-2-3539', 
            'Chrionema chlorotaenia' = 'Fis-33989')


for(i in 1:length(spp_ex)) {
  x <- obis_occurrence(scientificname = names(spp_ex)[i])
  
  y <- rgbif::occ_search(scientificName = names(spp_ex)[i])[['data']]
  
  z <- x %>% 
    select(obis_id = id, long = decimalLongitude, lat = decimalLatitude) %>%
    mutate(long = round(long, 5),
           lat  = round(lat, 5)) %>%
    full_join(y %>%
                select(gbif_id = key, long = decimalLongitude, lat = decimalLatitude) %>%
                mutate(long = round(long, 5),
                       lat  = round(lat, 5)),
              by = c('long', 'lat')) %>%
    filter(!is.na(lat))
  
  z <- z %>%
    mutate(obs = ifelse(is.na(obis_id), 'gbif', 'obis'),
           obs = ifelse(!is.na(obis_id) & !is.na(gbif_id), 'both', obs)) %>%
    select(-gbif_id, -obis_id) %>%
    distinct()
  
  write_csv(z, paste0('clip_fao/obs_for_', str_replace_all(tolower(spp_ex[i]), '-', '_'), '.csv'))
  
}

clipped_spp_sciname <- read_csv('clip_fao/clipped_mid_pac.csv') %>%
  filter(lim == 'e_lim') %>%
  select(am_sid, sciname) %>%
  mutate(occ_count = NA, 
         mean_occ  = NA)



# get_occ_summary <- function(scientificName) {
#   
#   x <- obis_occurrence(scientificname = scientificName)
#   if(nrow(x) == 0) 
#     x <- data.frame(id = NA, decimalLongitude = NA, decimalLatitude = NA)
#   
#   y <- rgbif::occ_search(scientificName = scientificName)[['data']]
#   if(nrow(y) == 0) 
#     y <- data.frame(key = NA, decimalLongitude = NA, decimalLatitude = NA)
#   
#   z <- x %>% 
#     select(obis_id = id, long = decimalLongitude, lat = decimalLatitude) %>%
#     mutate(long = round(long, 5),
#            lat  = round(lat, 5)) %>%
#     full_join(y %>%
#                 select(gbif_id = key, long = decimalLongitude, lat = decimalLatitude) %>%
#                 mutate(long = round(long, 5),
#                        lat  = round(lat, 5)),
#               by = c('long', 'lat')) %>%
#     filter(!is.na(lat))
#   
#   z <- z %>%
#     mutate(obs = ifelse(is.na(obis_id), 'gbif', 'obis'),
#            obs = ifelse(!is.na(obis_id) & !is.na(gbif_id), 'both', obs)) %>%
#     select(-gbif_id, -obis_id) %>%
#     distinct()
#   
#   x <- x %>% filter(!is.na(decimalLatitude)); y <- y %>% filter(!is.na(decimalLatitude)); 
# 
#   # message(i, '. For ', clipped_spp_sciname$sciname[i], ': OBIS: ', nrow(x), ', GBIF: ', nrow(y), ', total unique: ', nrow(z))
#   
#   temp <- data.frame(sciname   = scientificName,
#                      occ_count = nrow(z),
#                      mean_occ  = (nrow(x) + nrow(y))/2,
#                      stringsAsFactors = FALSE)
# 
#   return(temp)
# }

# library(parallel)
# 
# occ_list <- mclapply(clipped_spp_sciname$sciname, get_occ_summary, mc.cores = 12)
# 
# occ_df <- bind_rows(occ_list)
# 
# write_csv(occ_df, 'data/clipped_spp_occurrences.csv')