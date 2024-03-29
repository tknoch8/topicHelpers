% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_td_gamma.R
\name{get_td_gamma}
\alias{get_td_gamma}
\title{get the gamma matrix from an stm topic model object}
\usage{
get_td_gamma(topicModel)
}
\arguments{
\item{topicModel}{the stm topic model from which to extract the beta matrix}
}
\value{
a tbl_df with columns "document", "topic", "gamma"
}
\description{
get a broom::tidy matrix containing the probabiliies each document is generated from each topic
}
\examples{
\dontrun{
data("tm_ex_dat")

get_td_gamma(tm_ex_dat$topic_model_test)
}
}
