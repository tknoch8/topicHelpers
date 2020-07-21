# #' @title get "frex" words for topics by group
# #' @description  get top n frex words for each topic in an stm topic model, within each group
# #' @usage get_vocab_frex_by_group(topicModel, topicModel_K, n = 7, ...)
# #' @param topicModel the stm topic model object to be operated on
# #' @param topicModel_K the number of topics K in topicModel
# #' @param n the number of words to return for each topic
# #' @param ... additional arguments passed
# #' @note this function is not ready for use. gets frex by topic. needs to get frex by group
# get_vocab_frex_by_group <- function(topicModel, topicModel_K, n = 7, ...) {
#   frexes <- topicModel$beta$logbeta
#   frexes <- purrr::map(.x = frexes, .f = stm::calcfrex)
#   frexes <- purrr::map(.x = frexes, .f = utils::head, n)
#   vocab <- topicModel$vocab
#   frexes <- purrr::map(frexes, tibble::as_tibble)
#   get_vocab_words_from_indices <- function(data) {
#     purrr::map_df(data, function(x) vocab[x])
#   }
#   frexes <- purrr::map_df(.x = frexes, .f = get_vocab_words_from_indices)
#   frexes %>% 
#     dplyr::rename_all(~paste0("topic_", c(1:topicModel_K)))
# }

