#' Preprocess_data
#'
#' Perform pre-processing tasks on the text data including removing punctuation and stopwords.
#' This function also produces a long styled data set in preparation for exploratory data analysis.
#'
#' @param labelled_raw A dataframe that details whether the comment belongs to a topic using a binary 0 or 1.
#' @param unlabelled_raw An unlabelled dataframe that will be used for predictions.
#' @param topics A list containing each topic.
#' @param text_vars A list containing each text variable to be processed.
#'
#' @return A pre-processed labelled dataframe, unlabelled dataframe, and long labelled dataframe.
#' @usage preprocess_data(
#' labelled_raw,
#' unlabelled_raw,
#' topics,
#' text_vars)
#' @importFrom magrittr "%>%"
#' @importFrom utils "head" "tail"
#' @export
preprocess_data <- function(labelled_raw,
                            unlabelled_raw,
                            topics,
                            text_vars){

  # merge both sets to make pre-processing easier
  all <- rbind(labelled_raw[text_vars], unlabelled_raw[text_vars])

  # for each text variable selected, pre-process the data such as removing punctuation,
  # numbers etc.
  cat("STEP 1 OF 2:")
  for (var in text_vars){
    cat("\nPreprocessing text for variable", var, "...\n")
    all[[var]] <- stringr::str_replace_all(all[[var]], pattern = "[[:punct:]]", " ")
    all[[var]] <- tolower(all[[var]])
    all[[var]] <- tm::removeNumbers(all[[var]])
    all[[var]] <- stringr::str_replace_all(all[[var]], "http", " ")
    all[[var]] <- tm::removeWords(x = all[[var]], tm::stopwords(kind = "SMART"))
  }

  # split again into labelled and unlabelled datasets
  labelled <- cbind(utils::head(all, nrow(labelled_raw)), labelled_raw[topics])
  unlabelled <- utils::tail(all, nrow(unlabelled_raw))

  labelled <- cbind(head(all, nrow(labelled_raw)), labelled_raw[topics])
  unlabelled <- tail(all, nrow(unlabelled_raw))

  # make long dataset in preparation for EDA
  cat("\nSTEP 2 OF 2:")
  cat("\nMaking long dataset...\n")

  longdf <- tidyr::gather(labelled, 'type', 'true', dplyr::all_of(topics))
  longdf <- tidyr::gather(labelled, 'type', 'true', dplyr::all_of(topics))

  longdf <- subset(longdf, true == 1)
  longdf <- subset(longdf, select=-true)
  rownames(longdf) <- NULL
  rownames(labelled) <- NULL
  rownames(unlabelled) <- NULL

  list("labelled_data" = labelled, "unlabelled_data" = unlabelled, "long_labelled_data" = longdf)
}
