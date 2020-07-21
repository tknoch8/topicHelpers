require(tidyverse)
require(tidytext)
require(lubridate)

dupes <- rio::import(here::here("R", "some_testing", "data_clean", "dupes.csv"))

train_text <- rio::import(here::here("R", "some_testing", "data_clean", "train_text_for_general_model.xlsx")) %>% 
  dplyr::filter(! report_key %in% c(dupes$report_key)) %>% 
  dplyr::mutate(year_occurred = lubridate::year(date)) %>% 
  dplyr::mutate(month_occurred = lubridate::floor_date(date, unit = "month"),
        month_occurred = as.numeric(month_occurred)) %>% 
  dplyr::mutate(province_helmand = if_else(province == "Helmand", "Helmand", "Other"),
        province_helmand = as.factor(province_helmand),
        province = as.factor(province)) %>% 
  dplyr::mutate(taac_region = as.factor(taac_region)) %>% 
  dplyr::mutate(day_occurred = lubridate::floor_date(date, unit = "day")) %>%
  dplyr::mutate(day_occurred = lubridate::yday(day_occurred)) %>%
  dplyr::mutate(day_occurred = if_else(year_occurred == 2014, day_occurred + 365, day_occurred)) %>% 
 # text is in conversation column
  dplyr::select(-details) %>% 
  dplyr::mutate(gender = if_else(month_occurred %in% sample(month_occurred, 3), "female", "male"),
        gender = as.factor(gender)) %>% 
  tidyr::drop_na() %>% 
  dplyr::sample_n(as.integer(nrow(.) * 0.05))

word <- c("c", "cthe", "will", "people", "afghanistan", "afghan",
         "country", "region", "district", "province", "provincial",
         "person", "mgrs", "villages", "village", "ago", "lot",
         "can", "don't", "dont", "don", "days", "lots", "told", "coming", "rid",
         "thousand", "it", "it's", "its", "individual", "individuals",
         "city", "situation", "people's", "it's")

sw <- tidytext::stop_words
# custom stop word list
mystops <- tibble(word = word, lexicon = "CUSTOM")
# join stop words lists, make data.table
sw <- dplyr::bind_rows(sw, mystops)

# unnest tokens and remove stopwords, filter for words appearing 5 or more times
tidy_train_text <- train_text %>% 
  tidytext::unnest_tokens(word, conversation, token = "words") %>% 
  dplyr::anti_join(sw) %>% 
  dplyr::filter(! str_detect(word, "[0-9]+")) %>% 
  dplyr::filter(nchar(word) > 2) %>% 
  dplyr::add_count(word) %>% 
  dplyr::filter(n >= 5) %>% 
  dplyr::select(-n) %>%
 # filter out outlier/error date entry
  dplyr::filter(date != lubridate::ymd("2014-10-14"))

# unnest words into text column for stm pre_processing
nested_tidy_train_text <- tidy_train_text %>%
  tidyr::nest(word) %>%
  dplyr::mutate(text = map(data, unlist),
        text = map_chr(text, paste, collapse = " ")) %>%
  dplyr::select(-data) %>%
  dplyr::filter(text != "character")

# stm text processing
train_text_processed <- stm::textProcessor(
 documents = nested_tidy_train_text$text,
 metadata = nested_tidy_train_text,
 onlycharacter = TRUE,
 removestopwords = FALSE,
 stem = FALSE,
 lowercase = FALSE,
 removenumbers = FALSE,
 removepunctuation = FALSE,
 wordLengths = c(2, 100),
 striphtml = TRUE,
 language = "english"
)

# prepare documents for stm::stm()
train_text_prepped <- stm::prepDocuments(
 documents = train_text_processed$documents,
 vocab = train_text_processed$vocab,
 meta = train_text_processed$meta
)

topic_model_test <- stm::stm(
  documents = train_text_prepped$documents,
  vocab = train_text_prepped$vocab,
  data = train_text_prepped$meta,
  verbose = TRUE,
  K = 195,
  max.em.its = 3
  # prevalence = ~ province
  # content = ~ province
)

topic_model_test_2 <- stm::stm(
 documents = train_text_prepped$documents,
 vocab = train_text_prepped$vocab,
 data = train_text_prepped$meta,
 verbose = TRUE,
 K = 195,
 max.em.its = 3,
 prevalence = ~ province
 # content = ~ province
)

tm_ex_dat <- list(
  train_text = train_text,
  tidy_train_text = tidy_train_text,
  nested_tidy_train_text = nested_tidy_train_text,
  train_text_prepped = train_text_prepped,
  topic_model_test = topic_model_test,
  topic_model_test_2 = topic_model_test_2
)

# dat <- load(here::here("data-raw", "tm_ex_dat.RData"))

usethis::use_data(tm_ex_dat, overwrite = TRUE)
