#' Example data from a simple stm topic model, and it's required pre-processing steps.
#' objects include:
#' train_text - original data frame
#' tidy_train_text - words unnested and other filtering performed
#' nested_tidy_train_text - nested tidy_train_text
#' train_text_prepped - result of stm::prepDocuments
#' topic_model_test - result of stm::stm, has no content and no province arguments
#' topic_model_test_2 - result of stm::stm, has no content and no province arguments
#' 
#' @docType data
#' 
#' @usage data(tm_ex_dat)
#' 
#' @format an object of class RData
#' 
#' @keywords datasets
#' 
#' @examples 
#' data(tm_ex_dat)
#' 
"tm_ex_dat"