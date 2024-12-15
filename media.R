

library(tidyverse)
library(metatargetr)

# setwd("/mnt/sdb/Dropbox/postdoc/de25")

# here::here()

# rollama::

df_all_Germany <- read_csv("data/df_all_Germany.csv")

fb_dat <- readRDS("data/fb_dat.rds")

get_ad_snapshots2 <- purrr::possibly(get_ad_snapshots, otherwise = NULL, quiet = F)


hash_table <- read_csv("data/media/hash_table.csv") 

hash_locall <- read_csv("hash_table.csv")

hash_table <- hash_table %>% 
  bind_rows(hash_locall) %>% 
  select(hash, ad_id) %>% 
  distinct() 

already_there <- hash_table$ad_id %>%
  str_remove_all("adid_") %>%
  str_remove_all("_.*")


fb_dat3 %>% 
  filter(!(id %in% already_there))  %>% 
  pull(id)%>% 
  unique()  %>%
  rev() %>% 
  # .[10] %>%
  map_dfr(~{
    
    get_ad_snapshots2(.x, download = T, hashing = T, mediadir = "data/media")
  }, .progress = T)


those_hashes_are_there <- hahes_are_there %>% c(c(dir("data/media/img_hash") %>% 
                                                    str_remove_all("\\..*"),
                                                  dir("data/media/vid_hash")  %>% 
                                                    str_remove_all("\\..*")))

media_dat <- hash_table %>% 
  filter(!(hash %in% those_hashes_are_there)) %>% 
  pull(ad_id)%>% 
  str_remove_all("adid_") %>% 
  str_remove_all("_.*") %>% 
  unique()  %>%
  rev() %>% 
  # .[10] %>%
  map_dfr(~{
    
    get_ad_snapshots2(.x, download = T, hashing = T, mediadir = "data/media")
  }, .progress = T)


saveRDS(media_dat, file = "data/media_dat.rds")
# 
# media_dat <- fb_dat$id %>% 
#   unique() %>%
#   setdiff(already_there) %>%
#   # .[10] %>% 
#   map_dfr(~{
#     
#     get_ad_snapshots2(.x, download = T, hashing = T, mediadir = "data/media")
#   }, .progress = T)
# 
# 
# saveRDS(media_dat, file = "data/media_dat.rds")

