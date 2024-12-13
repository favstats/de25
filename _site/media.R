

library(tidyverse)
library(metatargetr)

# setwd("/mnt/sdb/Dropbox/postdoc/de25")

# here::here()

# rollama::

df_all_Germany <- read_csv("data/df_all_Germany.csv")

get_ad_snapshots2 <- purrr::possibly(get_ad_snapshots, otherwise = NULL, quiet = F)


hash_table <- read_csv("data/media/hash_table.csv")

already_there <- hash_table$ad_id %>% 
  str_remove_all("adid_") %>% 
  str_remove_all("_.*")



media_dat <- df_all_Germany$id %>% 
  unique() %>%
  setdiff(already_there) %>% 
  # .[10] %>% 
  map_dfr(~{
    
    get_ad_snapshots2(.x, download = T, hashing = T, mediadir = "data/media")
  }, .progress = T)


saveRDS(media_dat, file = "data/media_dat.rds")