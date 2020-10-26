#' plot_important_terms
#'
#' Plots the most relevant terms for each topic using weighted log odds. This function can help to distinguish topics.
#'
#' @param long_labelled_data A long-styled labelled dataframe.
#' @param text_var The text variable that the user wants to explore.
#' @param log_odds_n The number of terms to include. Default: 30
#'
#' @return A word graph for each topics with term frequency against weighted log odds ratio.
#' @importFrom magrittr "%>%"
#' @export

plot_important_terms <- function(long_labelled_data,
                                 text_var,
                                 log_odds_n = 30){

  text_var <- rlang::enquo(text_var)

  # visualise the most important words for each topic, measured by weighted log odds ratio
  pre_process <- long_labelled_data %>%
    tidytext::unnest_tokens(word, !! text_var) %>%
    dplyr::group_by(type) %>%
    dplyr::count(word, sort = TRUE) %>%
    dplyr::ungroup() %>%
    tidylo::bind_log_odds(set = type, feature = word, n = n)

  log_odds_word_graph <- pre_process %>%
    dplyr::group_by(type) %>%
    dplyr::top_n(n = log_odds_n, wt = n) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(x = n, y = log_odds_weighted, label = word)) +
    ggplot2::geom_hline(yintercept = 0, lty = 2,
                        color = "gray50", alpha = 0.5, size = 1.2) +
    ggrepel::geom_text_repel(size = 3, segment.size = 0.5) +
    ggplot2::geom_point(size = .2) +
    ggplot2::facet_wrap(~ type, nrow = 3, scales = "free") +
    ggplot2::scale_x_log10() +
    ggplot2::labs(x = "Word frequency",
                  y = "Log odds ratio, weighted by uninformative Dirichlet prior")

  log_odds_word_graph
}
