# #' @title get score words by gamma
# #' @description get the score words for each topic of an stm topic model
# 
# get_score_gammas <- function(td_gamma_obj) {
#   
#   td_gamma_obj %>% 
#     mutate(topic = as.factor(topic)) %>% 
#     group_by(topic) %>% 
#     summarize(gamma = mean(gamma)) %>% 
#     arrange(desc(gamma)) %>% 
#     mutate(topic = paste0("topic_", topic),
#            topic = reorder(topic, gamma)) %>% 
#     left_join(scores_obj, by = "topic")
#   
# }