### Compare status and trend 

setwd('~/github/ohiprep')
library(ggplot2)
source('src/R/common.R')

goal     <- 'globalprep/SPP_ICO'
scenario <- 'vAM_IUCN'
dir_data  <- file.path('~/github/ohiprep', goal, scenario, 'data') 


#############################################################################=
scatterPlot <- function(csv_orig, csv_new, title_text, zero_one_new = FALSE, zero_one_old = FALSE,
                        fig_save = file.path(dir_data, '../graphs', paste0(title_text, '_scatterPlot.png'))) {
  
  require(git2r)
  require(ggplot2)
  require(RColorBrewer)
  
  names <- read.csv("~/github/ohi-global/eez2013/layers/rgn_labels.csv") %>%
    filter(type=="eez") %>%
    select(rgn_id, label)
  
  data_orig <- read.csv(csv_orig)
  names(data_orig) <- c('rgn_id', 'score')
  data_new <- read.csv(csv_new)
  names(data_new) <- c('rgn_id', 'score')
  if(zero_one_old) data_orig$score = data_orig$score*100
  if(zero_one_new) data_new$score = data_new$score*100
  
  data_combo <- data_orig %>%
    dplyr::rename(scores_old = score) %>%
    left_join(data_new %>%
                dplyr::rename(scores_new = score), 
              by=c('rgn_id')) %>%
    dplyr::mutate(change = scores_new - scores_old) %>%
    dplyr::mutate(mean = mean(change, na.rm=TRUE),
           sd =  sd(change, na.rm=TRUE)) %>%
    ungroup() %>%
    dplyr::mutate(z_score = (change-mean)/sd) %>%
    dplyr::mutate(z_greater_1 = ifelse(abs(z_score) > 1, "yes", "no")) %>%
    left_join(names, by='rgn_id') %>%
    filter(rgn_id != 0) %>%
    dplyr::mutate(
#      label     = ifelse(is.na(label), as.character(rgn_id), label),
      plotLabel = ifelse(z_greater_1=="yes", as.character(label), NA)
    ) 
    
  ggplot(data_combo, aes(x = scores_old, y = scores_new)) +
    geom_point(shape = 19) +
    theme_bw() + 
    labs(title=sprintf('Score differences for %s', title_text), 
         x = paste0('Orig: ', basename(csv_orig)), 
         y = paste0('New: ', basename(csv_new)) ) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    geom_text(aes(label = plotLabel), vjust = 1.5, size = 3)
  
  ggsave(fig_save, width = 10, height = 8)
}

#############################################################################=
### SPP Comparison Graphs -----
#############################################################################=

pref_flag <- c('IUCN', 'AM')
prob_flag <- c('0.4', '0.01')

i <- c(1, 1, 1, 1)

status_base <- file.path(dir_data, sprintf('spp_status_global_%spref_prob%s.csv', pref_flag[i[1]], prob_flag[i[3]]))
status_test <- file.path(dir_data, sprintf('spp_status_global_%spref_prob%s.csv', pref_flag[i[2]], prob_flag[i[4]]))

trend_base <- file.path(dir_data, sprintf('spp_trend_global_%spref_prob%s.csv', pref_flag[i[1]], prob_flag[i[3]]))
trend_test <- file.path(dir_data, sprintf('spp_trend_global_%spref_prob%s.csv', pref_flag[i[2]], prob_flag[i[4]]))

scatterPlot(status_base, status_test, sprintf('GL_SPP_st_pref%svs%s_prob%svs%s', pref_flag[i[1]], pref_flag[i[2]], prob_flag[i[3]], prob_flag[i[4]]))
scatterPlot(trend_base,  trend_test,  sprintf('GL_SPP_tr_pref%svs%s_prob%svs%s',  pref_flag[i[1]], pref_flag[i[2]], prob_flag[i[3]], prob_flag[i[4]]))

status_base <- '~/github/ohi-global/eez2015/layers/spp_status.csv'
trend_base <- '~/github/ohi-global/eez2015/layers/spp_trend.csv'

status_test <- file.path(dir_data, sprintf('spp_status_global_%spref_prob%s.csv', pref_flag[i[1]], prob_flag[i[3]]))
trend_test <- file.path(dir_data, sprintf('spp_trend_global_%spref_prob%s.csv', pref_flag[i[1]], prob_flag[i[3]]))

scatterPlot(status_base, status_test, 'SPP_status_OHI2015_orig_v_new')
scatterPlot(trend_base,  trend_test,  'SPP_trend_OHI2015_orig_v_new')

#############################################################################=
### Box Plots of change per scenario -----
#############################################################################=

status_base   <- read.csv(file.path(dir_data, sprintf('spp_status_global_%spref_prob%s.csv', 'IUCN',  '0.4')))
st_lo_thresh  <- read.csv(file.path(dir_data, sprintf('spp_status_global_%spref_prob%s.csv', 'IUCN', '0.01')))
st_am_pref    <- read.csv(file.path(dir_data, sprintf('spp_status_global_%spref_prob%s.csv',  'AM',   '0.4')))
st_lo_am_pref <- read.csv(file.path(dir_data, sprintf('spp_status_global_%spref_prob%s.csv',  'AM',  '0.01')))

scen_data <- status_base %>%
  rename(base_score = score) %>%
  full_join(st_lo_thresh %>%
              rename(score_lo = score),
            by = 'rgn_id') %>%
  full_join(st_am_pref %>%
              rename(score_am = score),
            by = 'rgn_id') %>%
  full_join(st_lo_am_pref %>%
              rename(score_lo_am = score),
            by = 'rgn_id') %>%
  mutate(diff_lo_abs = 100*(base_score - score_lo),
         diff_lo_rel = (base_score - score_lo)/base_score,
         diff_am_abs = 100*(base_score - score_am),
         diff_am_rel = (base_score - score_am)/base_score,
         diff_lo_am_abs = 100*(base_score - score_lo_am),
         diff_lo_am_rel = (base_score - score_lo_am)/base_score) %>%
  gather(scenario, change, diff_lo_abs:diff_lo_am_rel)

scen_data_rel <- scen_data %>%
  filter(str_detect(scenario, 'rel'))
scen_data_abs <- scen_data %>%
  filter(str_detect(scenario, 'abs'))

ggplot(data = scen_data_abs, aes(scenario, change)) +
  geom_boxplot() +
  labs(title = 'Absolute change in SPP status')

ggplot(data = scen_data_rel, aes(scenario, change)) +
  geom_boxplot() +
  labs(title = 'Relative change in SPP status')

