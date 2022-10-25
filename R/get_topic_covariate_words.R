# #' @title get topic-covariate interaction terms
# #' @description return a dataframe with topic-covariate interaction terms by a grouping variable
# #' @usage get_topic_covariate_words(topics = NULL, topicModel, n = 7, 
# #'                                  frexweight = 0.5, content_var, ...)
# #' @param topics don't worry about it for now
# #' @param topicModel stm topic model object to be operated on
# #' @param n the number of words to keep for each topic-covariate interaction
# #' @param frexweight the value of frexweight to be used in calculation of interaction terms,
# #' @param content_var the variable by which to group the interaction terms
# #' @param ... additional arguments passed
# #' @export
# get_topic_covariate_words <- function(topics = NULL, topicModel, n = 7, frexweight = 0.5, content_var, ...) {
#   if (n < 1) 
#     stop("n must be 1 or greater")
#   logbeta <- topicModel$beta$logbeta
#   K <- topicModel$settings$dim$K
#   vocab <- topicModel$vocab
#   if (is.null(topics)) 
#     topics <- 1:nrow(logbeta[[1]])
#   aspect <- length(logbeta) > 1
#   out <- list()
#   if (!aspect) {
#     out$prob <- list()
#     out$frex <- list()
#     out$lift <- list()
#     out$score <- list()
#     logbeta <- logbeta[[1]]
#     wordcounts <- topicModel$settings$dim$wcounts$x
#     frexlabels <- try(stm::calcfrex(logbeta, frexweight, wordcounts), 
#                       silent = TRUE)
#     liftlabels <- try(stm::calclift(logbeta, wordcounts), silent = TRUE)
#     scorelabels <- try(stm::calcscore(logbeta), silent = TRUE)
#     problabels <- apply(logbeta, 1, order, decreasing = TRUE)
#     for (k in 1:K) {
#       out$prob[[k]] <- vocab[problabels[1:n, k]]
#       if (class(frexlabels) == "try-error") {
#         out$frex[[k]] <- "FREX encountered an error and failed to run"
#       }
#       else {
#         out$frex[[k]] <- vocab[frexlabels[1:n, k]]
#       }
#       if (class(liftlabels) == "try-error") {
#         out$lift[[k]] <- "Lift encountered an error and failed to run"
#       }
#       else {
#         out$lift[[k]] <- vocab[liftlabels[1:n, k]]
#       }
#       if (class(scorelabels) == "try-error") {
#         out$lift[[k]] <- "Score encountered an error and failed to run"
#       }
#       else {
#         out$score[[k]] <- vocab[scorelabels[1:n, k]]
#       }
#     }
#     out <- lapply(out, do.call, what = rbind)
#   }
#   else {
#     labs <- lapply(topicModel$beta$kappa$params, function(x) {
#       windex <- order(x, decreasing = TRUE)[1:n]
#       ifelse(x[windex] > 0.001, vocab[windex], "")
#     })
#     labs <- do.call(rbind, labs)
#     A <- topicModel$settings$dim$A
#     anames <- topicModel$settings$covariates$yvarlevels
#     i1 <- K + 1
#     i2 <- K + A
#     intnums <- (i2 + 1):nrow(labs)
#     out$topics <- labs[topics, , drop = FALSE]
#     out$covariate <- labs[i1:i2, , drop = FALSE]
#     rownames(out$covariate) <- anames
#     if (topicModel$settings$kappa$interactions) {
#       tindx <- rep(1:K, each = A)
#       intnums <- intnums[tindx %in% topics]
#       out$interaction <- labs[intnums, , drop = FALSE]
#     }
#   }
#   out$topicnums <- topics
#   class(out) <- "labelTopics"
#   row.names(out$interaction) <- row.names(out$covariate)
#   out$interaction <- as.data.frame(out$interaction)
#   names(out$interaction) <- paste0("Word ", c(1:7))
#   out$interaction <- tibble::rownames_to_column(out$interaction, var = content_var)
#   return(out$interaction)
# }
# 