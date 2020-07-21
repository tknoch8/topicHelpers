#' @title get ranked topic importance for all documents
#' @description get the top most important topics, in order, for each document
#' @usage make_td_thetas_df(prepped_meta_obj, id_col, topicModel, 
#'                          topicModel_K, train_text, n_topics = 5)
#' @param prepped_meta_obj if using stm, this is the $meta dataframe in the object created by stm::prepDocuments
#' @param id_col the column of prepped_meta_obj containing the unique ids for each row/document
#' @param topicModel the stm topic model object to be operated on
#' @param topicModel_K the number of topics K in topicModel
#' @param train_text dataframe containing a column of documents to be used. This must be the 
#'                   dataframe/documents that were used to make topicModel
#' @param n_topics the number of topics to be returned for each row/document
#' @import rlang
#' @return a dataframe containing a row for each document that contains its most important topics
#' @importFrom dplyr enquo select rename_at vars
#' @importFrom tidyselect contains
#' @importFrom rlang get_expr
#' @examples 
#' \dontrun{
#' data("tm_ex_dat")
#' 
#' make_td_thetas_df(prepped_meta_obj = tm_ex_dat$train_text_prepped$meta,
#'                   id_col = id_conversation,
#'                   topicModel = tm_ex_dat$topic_model_test,
#'                   topicModel_K = tm_ex_dat$topic_model_test$settings$call$K,
#'                   train_text = tm_ex_dat$train_text)
#' }
#' @export
make_td_thetas_df <- function(prepped_meta_obj, id_col, topicModel, topicModel_K, train_text, n_topics = 5) {
  
  my_id_col <- enquo(id_col)
  
  message("-----------enquo done")
  
  test <- as.data.frame(prepped_meta_obj) %>% 
    select(!!my_id_col) %>% 
    cbind(topicModel$theta) %>% 
    rename_at(vars(-contains("id_")), list(~paste0("topic_", c(1:topicModel_K))))
  
  message("------------renaming done")
  
  # use get_expr() to get quoted version of my_id_col to pass as function argument to sort_all_document_topics
  # because sort_all_document_topics() is not a tidyeval function
  my_quoted_column <- as.character(get_expr(my_id_col))
  
  sorted_rows <- topicHelpers::sort_all_document_topics(data = test,
                                                        id_column = my_quoted_column,
                                                        n_topics = n_topics)

  message("------------sorted the rows")

  sorted_rows
    # dplyr::left_join(train_text, by = c(id_col = "id_conversation")) %>%
    # dplyr::select(id_col, dplyr::everything())
  
  # message("------------all done!!!")
  
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("id_", "V1"))



# my_col <- train_text_prepped$meta$id_conversation
# 
# make_td_thetas_df(
#   prepped_meta_id_col = my_col, 
#   topicModel = topic_model_test,
#   train_text = train_text
# )
