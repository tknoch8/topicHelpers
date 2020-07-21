#' @title get the beta matrix from an stm topic model object
#' @description get a broom::tidy matrix containing the probabiliies each word is generated from each topic
#' @usage 
#' get_td_beta(topicModel)
#' @param topicModel the stm topic model from which to extract the beta matrix
#' @return a tbl_df with columns "topic", "term", and "beta"
#' @importFrom tidytext tidy
#' @examples 
#' \dontrun{
#' data("tm_ex_dat")
#' 
#' get_td_beta(tm_ex_dat$topic_model_test)
#' }
#' @export
get_td_beta <- function(topicModel) {
  topicModel %>% 
    tidy(matrix = "beta")
}
