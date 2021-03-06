% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_td_thetas_df.R
\name{make_td_thetas_df}
\alias{make_td_thetas_df}
\title{get ranked topic importance for all documents}
\usage{
make_td_thetas_df(prepped_meta_obj, id_col, topicModel, 
                         topicModel_K, train_text, n_topics = 5)
}
\arguments{
\item{prepped_meta_obj}{if using stm, this is the $meta dataframe in the object created by stm::prepDocuments}

\item{id_col}{the column of prepped_meta_obj containing the unique ids for each row/document}

\item{topicModel}{the stm topic model object to be operated on}

\item{topicModel_K}{the number of topics K in topicModel}

\item{train_text}{dataframe containing a column of documents to be used. This must be the 
dataframe/documents that were used to make topicModel}

\item{n_topics}{the number of topics to be returned for each row/document}
}
\value{
a dataframe containing a row for each document that contains its most important topics
}
\description{
get the top most important topics, in order, for each document
}
\examples{
\dontrun{
data("tm_ex_dat")

make_td_thetas_df(prepped_meta_obj = tm_ex_dat$train_text_prepped$meta,
                  id_col = id_conversation,
                  topicModel = tm_ex_dat$topic_model_test,
                  topicModel_K = tm_ex_dat$topic_model_test$settings$call$K,
                  train_text = tm_ex_dat$train_text)
}
}
