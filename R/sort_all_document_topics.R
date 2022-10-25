#' @title get all topic importance rankings for every document
#' @description sort topic importance for each document (row)
#' @usage sort_all_document_topics(data, id_column, n_topics, ...)
#' @param data df or tibble containing betas by topic for one document
#' @param id_column the column containing the unique integer id of the row to be sorted
#' @param n_topics the number of topics to keep for each document
#' @param ... additional arguments passed
#' @note to be used within topicHelpers::make_td_thetas_df. Shouldn't need to be used outside of that context
#' @importFrom purrr map_df
<<<<<<< HEAD
#' @importFrom data.table subset
=======
>>>>>>> d253ff39e0a445a70ff3845ff7444c3f5ab863a9
#' @export
sort_all_document_topics <- function(data, id_column, n_topics = 5, ...) {
  
  # data must be coercible to a dataframe
  # id_column should be the name of the column that contains integer IDs
  # n_topics should be the number of topics you want to keep per document
  
  # make each row a list element so each can be ordered rowise
  splits <- (split(data, seq(nrow(data))))
  
  message("-------split data by rows")
  
  # the function that sorts a dataframe that consists of only one row
  sort_rows <- function(data, id_column, n_topics) {
    
    # coerce data to dataframe
    data <- as.data.frame(data)
    
    message("------coerced to dataframe")
    
    # separate the id column before sorting other columns
    mycol <- id_column
    message("------mycol assigned")
    column <- subset(data, select = mycol)
    message("--------first subset")
    
    # sort the remaining columns then reattach id column
    columns_other <- data[, colnames(data) != mycol]
    columns_other <- columns_other[, order(-columns_other[1, ])]
    
    # reattach id column
    topics_ordered <- cbind(column, columns_other)
    
    # keep only number of topics specified by user
    keep_cols <- n_topics + 1
    topics_ordered <- topics_ordered[, 1:keep_cols]
    
    # replace values with topic names
    topics_ordered[1, 2:ncol(topics_ordered)] <- c(names(topics_ordered[, 2:ncol(topics_ordered)]))
    
    # rename columns
    names(topics_ordered) <- c(id_column, paste0("t_", 1:n_topics))
    
    topics_ordered
    
  }
  
  # map that function over each single-row df in the list splits
  sorted_rows <- map_df(splits,
                        sort_rows,
                        id_column = id_column,
                        n_topics = n_topics)
  
  sorted_rows
  
}
