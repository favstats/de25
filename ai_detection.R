


library(httr) 
library(jsonlite)
library(tidyverse)

# Define the function
analyze_image <- function(image_path, model = "genai", api_user = "29963653", api_secret = "dVtHbV6mdSEhuiFqWKmC3Hr4Wg2HP22Q") {
  # Validate the image file
  if (!file.exists(image_path)) {
    stop("The specified image file does not exist.")
  }
  
  # API endpoint
  api_url <- "https://api.sightengine.com/1.0/check.json"
  
  # Prepare API parameters
  params <- list(
    models = model,
    api_user = api_user,
    api_secret = api_secret
  )
  
  # Perform the POST request
  response <- POST(
    url = api_url,
    body = list(media = upload_file(image_path)),
    encode = "multipart",
    query = params
  )
  
  # Check for successful response
  if (http_status(response)$category != "Success") {
    stop("API request failed with status: ", http_status(response)$message)
  }
  
  # Parse the JSON response
  result <- content(response, as = "text", encoding = "UTF-8")
  parsed_result <- fromJSON(result)
  
  return(parsed_result)
}
# 
# 
# dir("data/media/vid_hash/",recursive = T)
# 
#  dir("data/examples/media", full.names = T) %>% 
#   .[25] %>%
#   analyze_image()
#  
#  
#  file.show("data/media/img_hash/6a63588e3aad4acaca8068251c2d5152.png")
#  data/media/img_hash//

annotate_them <- function(path) {
  # print(path)
  the_output <- analyze_image(path)
  
   fin <- the_output %>% 
    purrr::flatten() %>% 
     set_names(c("status", "id_2", "timestamp", "operations", "ai_generated", 
                 "id_6", "uri")) %>% 
    as_tibble(.) %>% 
    janitor::clean_names() %>% 
    mutate(thepath = path)
  
   # print(fin$ai_generated)
   
   fin %>% appendornot::save_csv("data/aiclass.csv")
   
   return(fin)
}

annotate_them2 <- possibly(annotate_them,otherwise = NULL,quiet = F)

# theend <- dir("data/media/img_hash/", full.names = T) %>% 
#   map_dfr(annotate_them2, .progress = T)
# 
# saveRDS(theend, "data/aiclass.rds")

annotate_them2 <- possibly(annotate_them,otherwise = NULL,quiet = F)

theend <- dir("output_frames2/", full.names = T) %>% 
  map_dfr(annotate_them2, .progress = T)

dir()

saveRDS(theend, "data/aiclass2.rds")

aiclass1 <- readRDS("data/aiclass.rds")
aiclass2 <- readRDS("data/aiclass2.rds")

aiclass <- read_csv("data/aiclass.csv")

aiclass %>%
filter(ai_generated >= .50)

##### was man nachholen musste
valid_hand2 <- aiclass %>% 
  arrange(desc(ai_generated)) %>% 
  filter(ai_generated >= .5) %>% 
  anti_join(valid_hand) 

valid_hand2 %>% 
  pull(thepath) %>% 
  # .[1] %>% 
  walk(~{
    fs::file_copy(.x, str_replace(.x,"output_frames2//|data/media/img_hash//", "valid_hand2/"))
  })

openxlsx::write.xlsx(valid_hand2, "data/valid_hand2.xlsx")



hash_table %>% 
  filter(str_detect(hash, "2cfe95c38a839abb7c3babfeef0469a8")) %>% View()


 #######

aiclass %>% 
  anti_join(aiclass1)

aiclass2 %>% 
  ggplot(aes(ai_generated)) +
  geom_histogram()

aiclass2 %>% 
  arrange(desc(ai_generated)) %>% 
  View()


set.seed(24101991)

valid_hand <- bind_rows(aiclass %>% 
  arrange(desc(ai_generated)) %>% 
  filter(ai_generated >= .50),
  aiclass %>% 
  arrange(desc(ai_generated)) %>% 
  filter(ai_generated < .50) %>%
  sample_n(75))





# aiclass %>% View()



valid_hand$thepath %>% 
  walk(~{
    fs::file_copy(.x, str_replace(.x,"output_frames//|data/media/img_hash//", "valid_hand/"))
  })

 # openxlsx::write.xlsx(valid_hand %>% , "data/valid_hand.xlsx")


 
 hash_table %>% 
   filter(str_detect(hash, "2cfe95c38a839abb7c3babfeef0469a8")) %>% View()
 
# annotate_them("data/examples/media/AI_CDU.jpg")

# Example usage
# Replace with your actual model, API user, and API secret
# result <- analyze_image("path/to/image.jpg", "nudity-2.0", "your_api_user", "your_api_secret")
# print(result)

 ### runde 3 - annotations
 
 valid_hand3 <- bind_rows(aiclass %>% anti_join(aiclassold) %>% 
                            arrange(desc(ai_generated)) %>% 
                            filter(ai_generated >= .50),
                          aiclass %>% anti_join(aiclassold) %>% 
                            arrange(desc(ai_generated)) %>% 
                            filter(ai_generated < .50) %>%
                            sample_n(46))
 
 
 valid_hand3$thepath %>% 
   walk(~{
     fs::file_copy(.x, str_replace(.x,"output_frames//|data/media/img_hash/", "valid_hand3/"))
   })
 
 
 openxlsx::write.xlsx(valid_hand3, "data/valid_hand3.xlsx")
 
 
 
 
hl <-  read_csv("data/handlabelling - Sheet 1.csv")
h2 <-  read_csv("data/valid_hand2.xlsx - Sheet 1.csv")
h3 <-  read_csv("data/valid_hand3.xlsx - Sheet 1.csv")


hl %>% 
  bind_rows(h2) %>% 
  bind_rows(h3) %>% 
  mutate(ai = ifelse(is.na(ai), 0, ai)) %>%
  ggplot(aes(as.character(ai), ai_generated)) +
  geom_boxplot()
  mutate(pred = ifelse(ai_generated >= 0.5, 1, 0)) %>% 
  mutate(pred = as.factor(pred)) %>% 
  mutate(ai = as.factor(ai)) %>% 
  yardstick::accuracy(ai, pred)

 
aiclass %>% View()



glimpse(aiclass)


#### JOINING THE DATA

hash_table %>% View()

hl_join <- hl %>%
  mutate(ai = ifelse(is.na(ai), 0, ai)) %>%
  mutate(hash = str_remove(uri, "\\..*")) %>% 
  select(ai,hash)

hl_join %>% filter(ai == 1)

fb_dat %>% filter(id == "802024048102983")

df_all_Germany <- read_csv("data/df_all_Germany.csv")


aidat <- hash_table %>%
  distinct(hash, ad_id, .keep_all = T)  %>%
  # filter(hash == "7b0a842a872665aef2bf6762bf7567c9")

  left_join(aiclass %>% mutate(hash = str_remove_all(uri, "\\..*|_.*"))) %>% 
  distinct(hash, ad_id, .keep_all = T)  %>%
  left_join(hl %>%
              mutate(ai = ifelse(is.na(ai), 0, ai)) %>%
              mutate(hash = str_remove_all(uri, "\\..*|_.*")) %>% 
              select(ai,hash)) %>% 
  distinct(hash, ad_id, .keep_all = T)  %>%
  drop_na(ai_generated) %>%
  mutate(ai = ifelse(is.na(ai), 0, ai))  %>%
  distinct(hash, ad_id, .keep_all = T) %>% 
  mutate(id = str_remove_all(ad_id, "adid_|\\..*|_.*")) %>% 
  left_join(fb_dat3 %>% mutate_all(as.character) %>% bind_rows(df_all_Germany %>% mutate_all(as.character))) %>% 
  drop_na(ai) %>% 
  distinct(id, .keep_all = T)  %>% 
  mutate(date_range_start = as.Date(ad_delivery_start_time))  %>%
  mutate(
    spend_lower = str_extract(spend, "lower_bound[ =:\\[\\\"]+(\\d+)") %>% str_extract("\\d+"),
    spend_upper = str_extract(spend, "upper_bound[ =:\\[\\\"]+(\\d+)") %>% str_extract("\\d+"),
    audience_lower = str_extract(estimated_audience_size, "lower_bound[ =:\\[\\\"]+(\\d+)") %>% str_extract("\\d+"),
    audience_upper = str_extract(estimated_audience_size, "upper_bound[ =:\\[\\\"]+(\\d+)") %>% str_extract("\\d+"),
    impressions_lower = str_extract(impressions, "lower_bound[ =:\\[\\\"]+(\\d+)") %>% str_extract("\\d+"),
    impressions_upper = str_extract(impressions, "upper_bound[ =:\\[\\\"]+(\\d+)") %>% str_extract("\\d+")
  ) %>%
  mutate_at(vars(spend_lower, spend_upper,
                 impressions_lower, impressions_upper,
                 audience_lower, audience_upper), as.numeric) %>%
  # drop_na(spend_lower, spend_upper, impressions_lower, impressions_upper) %>%
  mutate(impressions_lower = case_when(
    # is.na(impressions_upper_bound) ~ 0,
    # is.na(impressions_lower_bound) ~ 0,
    impressions_lower == 0 ~ 50,
    T ~ impressions_lower)) %>%
  mutate(spend_lower = case_when(
    spend_lower == 0 ~ 50,
    T ~ spend_lower))  %>% 
  mutate(page_id = as.character(page_id)) %>% 
  left_join(tabelle3 %>% mutate(page_id = as.character(id)) %>% select(-id)) %>%
  distinct(id, .keep_all = T) 
  # group_by(ai) %>% 
  # summarize(spend_lower = sum(spend_lower, na.rm = T),
  #           impressions_lower = sum(impressions_lower, na.rm = T))
  # filter(id == "965876451913336") %>% 
  # select(ad_creation_time)
  # filter(is.na(ad_creation_time)) %>% # View()
  # View()
  # filter(ad_id == "adid_1024684215458526") %>% View()
  # count(id, ai, sort = T)
  # filter(ai == 1)
  # filter(ai == 1) %>%
  # count(ai)
  # distinct(hash)

  # filter(hash == "158d623a80f3d502aaa180f1131a44b1")
  
saveRDS(aidat, file = "data/aidat.rds")
  # glimpse(aidat)
 # Partei d. Humanisten
 #  BIG Partei 
 # Piratenpartei
 #  Mensch Umwelt Tierschutz 
 # Freie Wähler Bund
 # SGP
 # Bündnis Deutschland
tabelle3 <- tabelle2 %>% 
  bind_rows(
    tibble(id = c("499613946840551", "353378298097249", "149871337782",
                  "108568039165729", "204327886385912", "90523314133",
                  "101011128446841") %>% as.numeric,
           partei = c("PdH", "BIG", "Piraten", "Tierschutz", "FWB", "SGP", "Bündnis Deutschland"),
    land  = "bund")
  )

aidat %>% 
  group_by(ai) %>%
  summarize(spend_lower = sum(spend_lower, na.rm = T),
            impressions_lower = sum(impressions_lower, na.rm = T))

# 
# aidat %>% 
#   mutate(page_id = as.character(page_id)) %>% 
#   left_join(tabelle3 %>% mutate(page_id = as.character(id)) %>% select(-id)) %>%
#   distinct(id, .keep_all = T) %>% 
#   filter(is.na(partei)) %>% 
#   drop_na(page_id) %>% 
#   count(page_id, sort = T)
# aidat %>% 
#   # left_join(tabelle2 %>% mutate(page_id = as.character(id)) %>% select(-id)) %>% 
#   filter(page_id == "167785411957")

# aidat %>% 
#   mutate(page_id = as.character(page_id)) %>% 
#   left_join(tabelle3 %>% mutate(page_id = as.character(id)) %>% select(-id)) %>%
#   distinct(id, .keep_all = T) %>% 
#   # filter(partei == "Piraten") %>%
#   # select(spend, spend_lower)   %>%
#   mutate(
#     spend_lower = str_extract(spend, "lower_bound[ =:\\[\\\"]+(\\d+)") %>% str_extract("\\d+"),
#     spend_upper = str_extract(spend, "upper_bound[ =:\\[\\\"]+(\\d+)") %>% str_extract("\\d+"),
#     audience_lower = str_extract(estimated_audience_size, "lower_bound[ =:\\[\\\"]+(\\d+)") %>% str_extract("\\d+"),
#     audience_upper = str_extract(estimated_audience_size, "upper_bound[ =:\\[\\\"]+(\\d+)") %>% str_extract("\\d+"),
#     impressions_lower = str_extract(impressions, "lower_bound[ =:\\[\\\"]+(\\d+)") %>% str_extract("\\d+"),
#     impressions_upper = str_extract(impressions, "upper_bound[ =:\\[\\\"]+(\\d+)") %>% str_extract("\\d+")
#   ) %>% View()

df_all_Germany %>% 
  filter(page_id == "149871337782") %>% 
  select(spend)

aidat %>% 
  # filter(page_id == "167785411957")
  group_by(ai, partei) %>%
  summarize(spend_lower = sum(spend_lower, na.rm = T),
            impressions_lower = sum(impressions_lower, na.rm = T)) %>% 
  filter(ai == 1) %>% 
  # drop_na(partei) %>% 
  arrange(desc(spend_lower))

aidat %>% 
  mutate(page_id = as.character(page_id)) %>% 
  left_join(tabelle2 %>% mutate(page_id = as.character(id)) %>% select(-id)) %>%
  distinct(id, .keep_all = T) %>% 
  # filter(page_id == "167785411957")
  group_by(ai, land) %>%
  summarize(spend_lower = sum(spend_lower, na.rm = T),
            impressions_lower = sum(impressions_lower, na.rm = T)) %>% 
  filter(ai == 1) %>% 
  drop_na(land) %>% 
  arrange(desc(spend_lower))
  
  ####### new round of detections
  
# TODO: find out which AD IDs are missing
##  1. classify ad media that wasnt classified before
##  2. filter out .50 >= prob 
##  3. handcode the .50 >= prob
##  4. rerun script of probability
##  5. explore the data
  
# which ads were not classified?
  
present_hashes <- dir("data/media/img_hash") %>% 
  tibble(hash = .)%>%
  mutate(hash = str_remove_all(hash, "\\..*|_.*"))
  
# those_are_NOT_present <- hash_table %>%
#   distinct(hash) %>% 
#   # mutate(hash = str_remove_all(uri, "\\..*|_.*")) %>% 
#   anti_join(present_hashes)

# those are NOT classified:
those_are_NOT_classified <- present_hashes %>% 
  anti_join(aiclass %>%
              mutate(hash = str_remove_all(uri, "\\..*|_.*")) %>% 
              distinct(hash))
 
### classifiy remainders:
# theend <-
  dir("data/media/img_hash", full.names = T) %>% 
    tibble(path = .) %>% 
    mutate(hash = str_remove_all(path, "\\..*|_.*|data/media/img_hash/")) %>% 
    filter(hash %in% those_are_NOT_classified$hash) %>% 
    pull(path) %>% 
  map_dfr(annotate_them2, .progress = T)



# present_hashes %>% filter(hash == "00db11a94f4b37f1d70f783b5b822ccb")
aiclass %>%
  mutate(hash = str_remove_all(uri, "\\..*|_.*")) %>% 
  # distinct(hash)  %>% 
  filter(hash == "016f1a848c7af8ebaac6f7923b394850")

aiclass  %>%
  mutate(hash = str_remove_all(uri, "\\..*|_.*")) %>% 
  filter(str_detect(hash, "009cb3016ac68ba2baeda8631d859d7e"))
  
  