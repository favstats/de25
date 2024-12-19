

library(tidyverse)
library(metatargetr)

# setwd("/mnt/sdb/Dropbox/postdoc/de25")

# here::here()

# rollama::

# df_all_Germany <- read_csv("data/df_all_Germany.csv")

# fb_dat <- readRDS("data/fb_dat.rds")
fb_dat <- readRDS("data/fb_dat_all_profiles.rds")

get_ad_snapshots2 <- purrr::possibly(get_ad_snapshots, otherwise = NULL, quiet = F)


hash_table <- read_csv("data/media/hash_table.csv")
# 
## this is for the protoptype
# hash_locall <- read_csv("hash_table.csv")

# hash_table <- hash_table %>%
#   bind_rows(hash_locall) %>%
#   select(hash, ad_id) %>%
#   distinct()
###

ips_targeting <- read_lines("ips-targeting.txt")



get_proxy <- function(prxs) {
  stringr::str_split_1(prxs[[1]][1], ":")
}

get_proxy_user <- function(prxs) {
  stringr::str_split_1(prxs[[1]][2], ":")
}

# Define a function to get page insights

  
# Function to get media data with httr2 and proxy integration
get_media <- function(ad_id, download = FALSE, mediadir = "data/media", hashing = FALSE) {
  # Construct the URL
  
  
  
  prx <- sample(ips_targeting, 1)
  prxs <- stringr::str_split(prx, "(?<=\\d)\\:", n = 2)
  
  url <- glue::glue("https://www.facebook.com/ads/library/?id={ad_id}")
  
  # Retrieve proxy and user credentials
  proxy <- get_proxy(prxs)
  userpw <- get_proxy_user(prxs)
  
  # Perform the HTTP GET request using httr2
  response <- httr2::request(url) %>% 
    httr2::req_headers(
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/109.0.0.0 Safari/537.36",
      `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"
    ) %>%
    httr2::req_proxy(
      url = proxy[1],
      port = as.numeric(proxy[2]),
      username = userpw[1],
      password = userpw[2]
    ) %>%
    httr2::req_perform()
  
  # Extract raw HTML content
  html_raw <- response %>% httr2::resp_body_string()
  
  # Extract relevant script containing snapshot
  script_seg <- stringr::str_extract_all(html_raw, "<script.*?>.*?</script>") %>% 
    unlist() %>% 
    .[stringr::str_detect(., "snapshot")]
  
  # Convert the extracted script into JSON format
  dataasjson <-  metatargetr:::detectmysnap(script_seg)
  
  # Perform final data conversion and add ad_id
  fin <- dataasjson %>% metatargetr::stupid_conversion() %>% dplyr::mutate(id = ad_id)
  
  # Download media if requested
  if (download) {
    fin %>% metatargetr::download_media(mediadir = mediadir, hashing)
  }
  
  return(fin)
}

debugonce(get_media)
get_media("3107592112807842", download = T, hashing = T, mediadir = "data/media")

already_there <- hash_table$ad_id %>%
  str_remove_all("adid_") %>%
  str_remove_all("_.*")

# fb_dat$page_id %>% unique()

fb_dat %>% 
  sample_n(n()) %>% 
  filter(!(id %in% already_there))  %>%
  pull(id) %>% 
  unique()  %>%
  # rev() %>% 
  # .[10] %>%
  map_dfr(~{
    
    get_ad_snapshots2(.x, download = T, hashing = T, mediadir = "data/media")
  }, .progress = T)


# those_hashes_are_there <- hahes_are_there %>% c(c(dir("data/media/img_hash") %>% 
#                                                     str_remove_all("\\..*"),
#                                                   dir("data/media/vid_hash")  %>% 
#                                                     str_remove_all("\\..*")))
# 
# media_dat <- hash_table %>% 
#   filter(!(hash %in% those_hashes_are_there)) %>% 
#   pull(ad_id)%>% 
#   str_remove_all("adid_") %>% 
#   str_remove_all("_.*") %>% 
#   unique()  %>%
#   rev() %>% 
#   # .[10] %>%
#   map_dfr(~{
#     
#     get_ad_snapshots2(.x, download = T, hashing = T, mediadir = "data/media")
#   }, .progress = T)
# 
# 
# saveRDS(media_dat, file = "data/media_dat.rds")





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

