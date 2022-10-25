#' @title get gamma terms object
#' @description get gamma terms object from an stm topic model object and its top_n_beta_terms object
#' @usage get_gamma_terms(top_n_beta_obj, td_gamma_obj, criteria = "frequency", topicModel = NULL)
#' @param top_n_beta_obj can be created with topicHelpers::get_top_n_beta_terms
#' @param td_gamma_obj can be created by topicHelpers::get_td_gamma
#' @param criteria one of either "frequency" for highest frequency words or "frex" for the "weighted harmonic mean of words'
#' ranks in terms of exclusivity and frequency" (Roberts, Stewart, Tingley)
#' (See https://cran.r-project.org/web/packages/stm/vignettes/stmVignette.pdf).
#' @param topicModel if criteria = "frex", this is the stm topic model object from which to extract the "frex" indices.
#' Defaults to highest frequency words
#' @importFrom dplyr group_by summarize arrange left_join mutate select row_number everything desc
#' @importFrom tidyr unite
#' @importFrom tibble as_tibble
#' @importFrom stats reorder
#' @importFrom stm labelTopics
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
#' get_gamma_terms(top_n_beta, td_gamma)
#' 
#' get_gamma_terms(top_n_beta, td_gamma, criteria = "frex", topicModel = tm_ex_dat$topic_model_test)
#' }
#' @return a tbl_df containing the top n gamma terms for each topic
#' @export 
get_gamma_terms <- function(top_n_beta_obj, td_gamma_obj, criteria = "frequency", topicModel = NULL) {
  
  if (criteria == "frequency") {
    
    td_gamma_obj %>% 
      group_by(topic) %>% 
      summarize(gamma = mean(gamma)) %>% 
      arrange(dplyr::desc(gamma)) %>% 
      left_join(top_n_beta_obj, by = "topic") %>% 
      mutate(topic = paste0("Topic ", topic),
             topic = reorder(topic, gamma))
  
  } else if (criteria == "frex") {
    
    frex_tops <- labelTopics(topicModel)
    
    frex_words <- as_tibble(frex_tops$frex) %>% 
      mutate(topic = row_number()) %>% 
      select(topic, everything()) %>% 
      select(-topic) %>% 
      unite(terms, paste0("V", 1:length(.)), sep = ", ") %>% 
      mutate(topic = row_number()) %>% 
      select(topic, terms)
    
    td_gamma_obj %>% 
      group_by(topic) %>% 
      summarize(gamma = mean(gamma)) %>% 
      arrange(desc(gamma)) %>% 
      left_join(frex_words, by = "topic") %>% 
      mutate(topic = paste0("Topic ", topic),
                    topic = reorder(topic, gamma))
  
  } else {
    stop("Something broke")
  }
  
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("topic", "gamma", "frex_tops", "frex_words", "terms", "."))