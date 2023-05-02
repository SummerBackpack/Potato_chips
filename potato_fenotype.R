
# import library ----------------------------------------------------------
renv::hydrate(prompt = FALSE)

library('tidyverse')
library('readxl')
library('janitor')
library('purrr')
library('stringi')


# import files ------------------------------------------------------------
getwd()
b12_13<- read_csv('b12_13_big-and-sick_20230404_PlantEye.csv')
b14<- read_csv('b14_15_medium_20230404_PlantEye(b14).csv')
b15<- read_csv('b14_15_medium_20230404_PlantEye(b15).csv')
b16_17<- read_csv('b16_17_small_20230404_PlantEye.csv')
b18_19<- read_csv('b18_19_small-and-short_20230405_PlantEye.csv')


# action ------------------------------------------------------------------

b12_13clear <- b12_13 %>% 
 slice(-(1:4))


b14_15 <- bind_rows(b14,b15)

# b18_19 %>%
#   filter(!({
#     timestamp %>%
#       as.Date %>%
#       as.character
#   } %in% c('2022-12-05','2022-12-06', '2022-12-07')))


b18_19 <- b18_19 %>%
  filter( {timestamp %>% as.Date} <= as.Date('2022-12-02'))
  #distinct(timestamp) %>%View()

b12_19 <-  
  bind_rows(b12_13clear,b14_15,b16_17,b18_19) %>% 
  drop_na()

remove(list = c('b12_13clear','b14_15','b16_17','b18_19','b14','b15','b12_13'))

write.csv(b12_19, 'b12_19.csv')
# -------------------------------------------------------------------------

