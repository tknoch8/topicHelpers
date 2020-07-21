#' @title get top n beta terms for each topic in an stm topic model
#' @description get the top n beta terms for each topic in an stm topic model
#' @usage get_top_n_beta_terms(td_beta_obj, n = 7)
#' @param td_beta_obj a tidied beta matrix from the stm topic model of interest. Can be created with topicHelpers::get_td_beta
#' @param n the number of top terms to be returned for each topic
#' @return a tbl_df with columns "topic" and "terms"
#' @importFrom dplyr arrange group_by top_n select mutate
#' @importFrom tidyr unnest
#' @importFrom purrr map
#' @examples 
#' \dontrun{
#' data("tm_ex_dat")
#' 
#' beta_obj <- get_td_beta(tm_ex_dat$topic_model_test)
#' 
#' get_top_n_beta_terms(beta_obj)
#' }
#' @export
get_top_n_beta_terms <- function(td_beta_obj, n = 7) {
  td_beta_obj %>% 
    arrange(beta) %>% 
    group_by(topic) %>% 
    top_n(n, beta) %>% 
    arrange(-beta) %>% 
    select(topic, term) %>% 
    summarize(terms = list(term)) %>% 
    mutate(terms = map(terms, paste, collapse = ", ")) %>% 
    unnest()
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "term"))