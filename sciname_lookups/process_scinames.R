library(tidyr)
library(dplyr)
library(stringr)

files_list <- list.files('sciname_lookups')

all_df <- data.frame()

for (i in 1:length(files_list)) { # i = 1
  x <- scan(file.path('sciname_lookups', files_list[i]), what = 'character', sep = '\n', skip = 3)
  
  df <- data.frame('raw' = str_trim(x), stringsAsFactors = FALSE) %>%
    mutate(raw = str_replace(raw, '  ', ' ')) %>%
    separate(raw, c('level', 'name'), sep = ' ', remove = FALSE, extra = 'drop') %>%
    mutate(level = tolower(str_replace(level, ':', '')),
           name = tolower(name)) %>%
    mutate(sub_infra = str_detect(level, '^sub')  | str_detect(level, '^infra')) %>%
    mutate(kingdom = ifelse(str_detect(level, 'kingdom'), name, NA),
           phylum  = ifelse(str_detect(level, 'phylum'),  name, NA),
           class   = ifelse(str_detect(level, 'class'),   name, NA),
           order   = ifelse(str_detect(level, 'order'),   name, NA),
           family  = ifelse(str_detect(level, 'family'),  name, NA),
           genus   = ifelse(str_detect(level, 'genus'),   name, NA))
  
  df1 <- df %>%
    filter(!sub_infra) %>% 
    fill(kingdom:family) %>%
    filter(!is.na(genus)) %>%
    select(kingdom:genus)
  
  all_df <- rbind(all_df, df1)
}

write.csv(all_df, 'sciname_lookups/scinames.csv', row.names = FALSE)
