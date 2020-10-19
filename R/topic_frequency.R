#' topic_frequency
#'
#' Displays the number of comments in the dataframe that each topic relates to.
#'
#' @param labelled_data A binary-labelled dataframe.
#'
#' @return A bar chart displaying each topic frequency.
#' @usage topic_frequency(labelled_data)
#' @importFrom magrittr "%>%"
#' @export
topic_frequency <- function(labelled_data){

  # find how many comments relate to each topic
  freq <- labelled_data %>%
    dplyr::summarise_if(is.numeric, sum) %>%
    tidyr::pivot_longer(dplyr::everything()) %>%
    dplyr::arrange(dplyr::desc(value)) %>%
    ggplot2::ggplot(ggplot2::aes(stats::reorder(name,value), value, fill = value)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::xlab('Topic')+
    ggplot2::ylab('Number of comments') +
    ggplot2::geom_text(ggplot2::aes(label = value), vjust = -.5)

  freq
}
