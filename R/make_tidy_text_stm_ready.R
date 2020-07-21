#' @title make an stm::prepDocuments() object ready for stm::stm
#' @description get a pre-processed stm::prepDocuments() object ready for use in stm::stm(). This function allows for the
#' expedition of preparing a tidy text object for use in stm::stm()
#' @usage make_tidy_text_stm_ready(tidy_train_text_obj)
#' @param tidy_train_text_obj the "tidy text" object that is to be processed. see https://juliasilge.com/blog/evaluating-stm/
#' @return a list object containing "documents", "vocab", "meta", etc,. typically returned by stm::prepDocuments()
#' @examples 
#' \dontrun{
#' data("tm_ex_dat")
#' 
#' tidy_train_text <- tm_ex_dat$tidy_train_text
#' 
#' nested_dat <- tm_ex_dat$nested_tidy_train_text
#' 
#' make_tidy_text_stm_ready(tidy_train_text)
#' }
#' @import tm
#' @importFrom tidyr nest
#' @importFrom dplyr mutate select
#' @importFrom stm textProcessor prepDocuments
#' @importFrom purrr map map_chr
#' @export
make_tidy_text_stm_ready <- function(tidy_train_text_obj) {
  
  nested_dat <- tidy_train_text_obj %>%
    nest(word) %>%
    mutate(text = map(data, unlist),
           text = map_chr(text, paste, collapse = " ")) %>%
    select(-data)
  
  # stm text processing
  text_processed <- textProcessor(
    documents = nested_dat$text,
    metadata = nested_dat,
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
  text_prepped <- prepDocuments(
    documents = text_processed$documents,
    vocab = text_processed$vocab,
    meta = text_processed$meta
  )
  
  text_prepped
  
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("word", "data", "text"))