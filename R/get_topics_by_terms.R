#' @title search gamma_terms for words
#' @description search a gamma_terms object for a speicific word or two
#' @usage get_topics_by_terms(gamma_terms, search_words, terms_col, n = 7, ...)
#' @param gamma_terms a tibble or dataframe containing terms by gamma by topic
#' @param search_words a character string or vector of the word or words to search for
#' @param n the number of words to keep in for each topic,
#' @param terms_col the column containing the terms, which will be separated into n columns
#' @param ... additional arguments passed
#' @return a tibble with columns "topic", "gamma", "term_1", ..., "term_n"
#' @importFrom tidyr separate 
#' @importFrom dplyr filter enquo
#' @importFrom stringr str_detect
#' @examples 
#' \dontrun{
#' data("tm_ex_dat")
#' 
#' td_beta <- get_td_beta(tm_ex_dat$topic_model_test)
#' 
#' top_n_beta <- get_top_n_beta_terms(td_beta)
#' 
#' td_gamma <- get_td_gamma(tm_ex_dat$topic_model_test)
#' 
#' my_gamma_terms <- get_gamma_terms(top_n_beta, td_gamma)
#' 
#' get_topics_by_terms(my_gamma_terms, search_words = c("government", "taliban"), terms)
#' }
#' @note Columns term_1, term_2, ..., term_n will be created by the function, 
#' as terms_col is separated to make a separate column for each value in 1 to n
#' @export
get_topics_by_terms <- function(gamma_terms, search_words, terms_col, n = 7, ...) {
  
  # options(warn = -1)
  
  .terms_col <- enquo(terms_col)
  # .term_1 <- dplyr::enquo(col1)
  # .term_2 <- dplyr::enquo(col2)
  
  gamma_terms %>% 
    separate(!!.terms_col, paste0("term_", c(1:n)), sep = " ") %>% 
    filter(str_detect(term_1, paste(search_words, collapse = "|")) | 
             str_detect(term_2, paste(search_words, collapse = "|")))
  
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("term_1", "term_2"))