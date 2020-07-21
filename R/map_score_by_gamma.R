# #' @title get score words by gamma for multiple values
# #' @description get score words for each topic in multiple stm topic model objects simoultaneously
# #' @usage map_score_gamma(model_list, ...)
# #' @param model_list the list object containing the topic model objects to be operated on
# #' @param ... additional arguments passed
# #' @examples 
# #' data("tm_ex_dat")
# #' 
# #' model_1 <- tm_ex_dat$topic_model_test_1
# #' model_2 <- tm_ex_dat$topic_model_test_2
# #' 
# #' my_model_list <- list(model_1, model_2)
# #' 
# #' map_score_gamma(my_model_list)
# map_score_gamma <- function(model_list, ...) {
#   options(warn = -1)
#   
#   .gamma_mats <- map(model_list, topicHelpers::get_td_gamma)
#   # .gamma_mat <- model_obj %>%
#   #   get_gamma_matrix()
#   
#   .topic_gamma_score <- map(.gamma_mats, get_taac_score_gammas)
#   # .topic_gamma_score <- .gamma_mat %>%
#   #   get_taac_score_gammas()
#   
#   options(warn = 1)
#   
#   dfs <- .topic_gamma_score %>%
#     map(as.data.frame)
#   # df <- .topic_gamma_score %>% 
#   #   get_taac_score_gammas()
#   
#   # dfs <- dfs %>% 
#   #   map(function(x) x %>% drop_na())
#   
#   subset_score_gamma_to_K <- function(data, x, ...) {
#     data[1:x, ]
#   }
#   
#   dfs %>% 
#     map2(.y = K_each, .f = subset_score_gamma_to_K)
#   
# }