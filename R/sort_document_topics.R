#' @title sort topics in a row by beta
#' @description filter for a row and sort it by topic importance
#' @usage sort_document_topics(data, id_column, sort_id, n_topics, values = TRUE, ...)
#' @param data df or tibble containing betas by topic for one document
#' @param id_column the column containing the unique id of the row to be sorted
#' @param sort_id the unique integer id of the row, from id_column, to be sorted
#' @param n_topics the number of topics to keep
#' @param values should the topic probabilities be returned (TRUE), or only the topic names (FALSE)
#' @param ... additional arguments passed
#' @export
sort_document_topics <- function(data, id_column, sort_id, n_topics, values = TRUE, ...) {
  
  # data must be coercible to a dataframe
  # id_column should be the name of the column that contains integer IDs
  # sort_id should be an ID from the id_column
  # n_topics should be the number of topics you want to keep
  # values, if TRUE you get topic probabilities, if FALSE you get topic names only
  
  # coerce data to dataframe
  data <- as.data.frame(data)
  
  # first check if submitted sort_id exists
  if (!sort_id %in% data[, 1]) stop("sort_id not found",
                                    call. = FALSE)
  
  mycol <- id_column
  
  # filter to just the requested row
  filtered <- data[data[, 1] == sort_id, ]
  # separate the id column before sorting other columns
  column <- subset(filtered, select = mycol)
  
  # sort the remaining columns then reattach id column
  columns_other <- filtered[, colnames(filtered) != mycol]
  columns_other <- columns_other[, order(-columns_other[1, ])]
  topics_ordered <- cbind(column, columns_other)
  keep_cols <- n_topics + 1
  topics_ordered <- topics_ordered[, 1:keep_cols]
  
  message(paste0("topics for document id ", sort_id, " are now sorted"))
  
  if (values == TRUE) {
    return(topics_ordered)
  } else {
    topics_ordered[1, 2:ncol(topics_ordered)] <- c(names(topics_ordered[, 2:ncol(topics_ordered)]))
    return(topics_ordered)
  }
  
}
