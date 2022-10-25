#' @title get the gamma matrix from an stm topic model object
#' @description get a broom::tidy matrix containing the probabiliies each document is generated from each topic
#' @usage 
#' get_td_gamma(topicModel)
#' @param topicModel the stm topic model from which to extract the beta matrix
#' @return a tbl_df with columns "document", "topic", "gamma"
#' @importFrom tidytext tidy
#' @examples 
#' \dontrun{
#' data("tm_ex_dat")
#' 
#' get_td_gamma(tm_ex_dat$topic_model_test)
#' }
#' @export
get_td_gamma <- function(topicModel) {
  topicModel %>% 
    tidy(matrix = "gamma")
}
