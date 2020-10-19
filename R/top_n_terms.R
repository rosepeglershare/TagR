#' top_n_terms
#'
#' Displays the most frequent n words for each topic.
#'
#' @param long_labelled_data A long-styled labelled dataframe.
#' @param text_var The text variable that the user wants to explore.
#' @param top_n The number of terms to include. Default: 10
#'
#' @return A bar-chart visualising the top n words for each topic.
#' @usage top_n_terms(
#' long_labelled_data,
#' text_var,
#' top_n = 10)
#' @importFrom magrittr "%>%"
#' @export

top_n_terms <- function(long_labelled_data, text_var, top_n = 10){
  text_var <- rlang::enquo(text_var)

  # find the most frequent words for each topic
  topn <- long_labelled_data %>%
    tidytext::unnest_tokens(word, !! text_var) %>%
    dplyr::count(type, word) %>%
    dplyr::group_by(type) %>%
    dplyr::top_n(n = top_n) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(type = as.factor(type),
                  word =tidytext::reorder_within(word, n, type)) %>%
    ggplot2::ggplot(ggplot2::aes(word, n, fill = type)) +
    ggplot2::geom_col(alpha=.9, show.legend = FALSE) +
    ggplot2::facet_wrap(~type, scales = "free") +
    ggplot2::coord_flip() +
    tidytext::scale_x_reordered() +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::labs(x = NULL)

  topn
}
