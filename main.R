# cleaning -----
rm(list = ls())
gc(reset = TRUE)

# import library ----------------------------------------------------------

renv::activate()
renv::hydrate(prompt = FALSE)

library('tidyverse')
library('readxl')
library('janitor')
library('purrr')
library('stringi')
library('psych')
library('skimr')
library('stats')

set.seed(42)

# import files ------------------------------------------------------------
getwd()
b12_13<- read_csv('b12_13_big-and-sick_20230404_PlantEye.csv')
b14<- read_csv('b14_15_medium_20230404_PlantEye(b14).csv')
b15<- read_csv('b14_15_medium_20230404_PlantEye(b15).csv')
b16_17<- read_csv('b16_17_small_20230404_PlantEye.csv')
b18_19<- read_csv('b18_19_small-and-short_20230405_PlantEye.csv')
number<- read_csv2('TXY to VTR.csv')
all_data <- read_xlsx('potato_iformation.xlsx')

# action_with_tables ------------------------------------------------------------------

b12_13clear <- b12_13 %>% 
 slice(-(1:4)) #removed the first 1.5 days in "big and sick", they were bug

b14 <- b14 %>% filter(str_detect(unit, '^14'))
b15 <- b15 %>% filter(str_detect(unit, '^15'))
b14_15 <- bind_rows(b14,b15)

b18_19 <- b18_19 %>%
  filter( {timestamp %>% as.Date} <= as.Date('2022-12-02')) 
  #distinct(timestamp) %>%View()

b12_19 <-  
  bind_rows(b12_13clear,b14_15,b16_17,b18_19) %>% 
  drop_na() 

remove(list = c('b12_13clear','b14_15','b16_17','b18_19','b14','b15','b12_13'))

with_number <- full_join(number,b12_19,by=c('T:X:Y'='unit')) %>% 
  janitor::clean_names() %>% 
  select(-'treatment') %>% 
  select(-'g_alias') %>% 
  drop_na()

remove(list = c('b12_19','number'))

# aggregation with dbscan -----------------------------------------------------------------------

hours_eps <- 1

with_number <- with_number %>%
  mutate(hours_from_start =
                  as.numeric(difftime(timestamp, min(timestamp),
                                      units = 'hours')))
dbscan_cluster <- with_number %>%
  select(hours_from_start) %>%
  pull() %>%
  matrix(ncol = 1) %>%
  dbscan::dbscan(., eps = hours_eps) %>%
  .$cluster %>%
  as_factor(.) %>%
  as_tibble_col(column_name = 'dbscan_cluster')

checkmate::assert_true(dbscan_cluster %>%
                         n_distinct() > 1)

with_number <- with_number %>%
  bind_cols(dbscan_cluster) %>%
  select(-hours_from_start)

remove(list = c('dbscan_cluster', 'hours_eps'))

# remove outlier groups -----------------------------------------------------------------

not_outliers <- with_number %>% #delete over-3-sigmas
  group_by(dbscan_cluster) %>%
  mutate(across(where(is.numeric),
                              {\(x) (x - min(x))/(max(x) - min(x))})) %>%
  mutate(across(where(is.numeric),
                              {\(x) abs(x - mean(x)) / sd(x) <= 3})) %>%
  ungroup() %>%
  pivot_longer(where(is.logical),
                      names_to = 'trait',
                      values_to = 'trait_value') %>%
  filter(trait_value) %>%
  select(-trait_value)

with_number <- with_number %>%
  pivot_longer(where(is.numeric),
                      names_to = 'trait',
                      values_to = 'trait_value') %>%
  right_join(not_outliers) %>%
  pivot_wider(names_from = 'trait',
                     values_from = 'trait_value')

remove(not_outliers)

# percentage to logit -------------------------------

fix_perc_imprecision <- \(x) case_when((x >= 0) &
                                                (x <= 100) ~ x/100,
                                              (x < 0) &
                                                (x >= -1) ~ 0,
                                              (x > 1) &
                                                (x <= 101) ~ 1,
                                              .default = NA)

with_number <- with_number %>%
  mutate(across(contains('_percent'),
        \(x) qlogis(fix_perc_imprecision(x))
                )
        ) %>%
  rename_with(\(x) stri_replace_all_fixed(x,
                                          pattern = '_percent',
                                          replacement = '_logit'))

# will replace -Inf (from 0 inputs) with the closest negative value
minus_inf_replacement <- with_number %>%
  select(contains('_logit')) %>%
  pull() %>%
  .[!is.na(.) & !is.infinite(.)] %>%
  min() %>%
  floor()

with_number <- with_number %>%
  mutate(across(contains('_logit'), \(x) if_else(is.infinite(x), minus_inf_replacement, x)))

logit_numeric_colnames <- with_number %>%
  select(where(is.numeric)) %>%
  colnames()

# -------------------------------------------------------------------------

final <- with_number %>% 
  full_join(all_data, by = join_by(v_t_r == 'numbers'))%>% 
  janitor::clean_names()
 
final

#write.csv(b12_19, 'b12_19.csv')
# -------------------------------------------------------------------------

str(final)

skimr::skim(final)


