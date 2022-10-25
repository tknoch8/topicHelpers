#' @title make a plot showing the top n most prevalent topics with associated words
#' @description make a plot showing the top n most prevalent topics with associated gamma terms
#' @usage make_topic_prevalence_plot(gamma_terms_obj, k = 20)
#' @param gamma_terms_obj can be created by topicHelpers::get_gamma_terms
#' @param k the number of topics to plot
#' @importFrom dplyr top_n
#' @importFrom ggplot2 ggplot aes geom_col geom_text coord_flip scale_y_continuous theme element_text
#' @importFrom scales percent_format
#' @importFrom ggthemes theme_tufte
#' @examples 
#' \dontrun{
#' data("tm_ex_dat")
#' 
#' td_beta <- get_td_beta(tm_ex_dat$topic_model_test)
#' 
#' top_n_beta <- get_top_n_beta_terms(td_beta)
#' 
#' td_gamma <- get_td_gamma(tm_ex_dat$topic_model_test)
#' 
#' my_gamma_terms <- get_gamma_terms(top_n_beta, td_gamma)
#' 
#' make_topic_prevalence_plot(my_gamma_terms)
#' 
#' make_topic_prevalence_plot(my_gamma_terms, k = 50)
#' }
#' @note additional ggplot2 elements can be added, i.e  make_topic_prevalence_plot(gamma_terms_object) + labs(title = "title")
#' @export
make_topic_prevalence_plot <- function(gamma_terms_obj, k = 20) {
  gamma_terms_obj %>% 
    top_n(k, gamma) %>% 
    ggplot(aes(topic, gamma, label = terms, fill = topic)) +
    geom_col(show.legend = FALSE, width = 0.5) +
    geom_text(hjust = 0, nudge_y = 0.0005, size = 3) +
    #family = "IBMPlexSans") +
    coord_flip() +
    scale_y_continuous(expand = c(0, 0),
                                limits = c(0, 0.09),
                                labels = percent_format()) +
    theme_tufte(ticks = FALSE) + #, base_family = "IBMPlexSans") +
    theme(plot.title = element_text(size = 16), #family = "IBMPlexSans-Bold"),
                   plot.subtitle = element_text(size = 13))
}

