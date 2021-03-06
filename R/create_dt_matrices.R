#' create_dt_matrices
#'
#' Creates sparse document term matrices using labelled and unlabelled data, ready for use in xgboost algorithm.
#'
#' @param labelled_data Pre-processed binary labelled dataframe.
#' @param unlabelled_data Pre-processed unlabelled dataframe.
#' @param text_vars List of text variables to include in analysis.
#' @param topics List of topics to include in analysis.
#' @param max_sparsity The maximum amount of sparsity the document term matrix should have. Default: 0.999
#' @param val_split The amount of training data that should be included in the validation set. Default: 0.2
#'
#' @return A complete labelled document-term matrix with corresponding labels,
#' a labelled document-term matrix split into training and validation sets with corresponding labels,
#' and an unlabelled document-term matrix used for predictions.
#'
#' @importFrom magrittr "%>%"
#' @export

create_dt_matrices <- function(labelled_data,
                               unlabelled_data,
                               text_vars,
                               topics,
                               max_sparsity = 0.999,
                               val_split = 0.2){

  # combine datasets to get one document term matrix
  cat("STEP 1 OF 4: Combining datasets...\n")

  if (length(text_vars)==1){
    labdata <- data.frame(labelled_data[[text_vars]])
    colnames(labdata) <- text_vars
    all <- rbind(labdata, unlabelled_data)
    colnames(all) <- 'text'

  } else {
    all <- rbind(labelled_data[, text_vars], unlabelled_data)

    all <- all %>%
      tidyr::unite("text", text_vars[[1]]:text_vars[[length(text_vars)]], sep = " ")
  }

  # create a corpus ready for document term matrix
  cat("STEP 2 OF 4: Creating corpus...\n")
  dtm <- tm::VCorpus(tm::VectorSource(all[,'text'])) %>%
    tm::tm_map(tm::stemDocument) %>%
    tm::tm_map(tm::stripWhitespace)

  # transform corpus into document term matrix
  cat("STEP 3 OF 4: Creating document term matrix...\n")
  if (max_sparsity == 1){
    dtm <- dtm %>%
      tm::DocumentTermMatrix() %>%
      as.matrix()
  } else {
    dtm <- dtm %>%
      tm::DocumentTermMatrix() %>%
      tm::removeSparseTerms(max_sparsity) %>%
      as.matrix()
  }

  dtm <- dtm[, colnames(dtm) != 'tree']

  # split into labelled and unlabelled matrices, with training and validation datasets for hyperparameter tuning
  cat("STEP 4 OF 4: Splitting into labelled/unlabelled sparse matrices...\n")
  full_labelled <- dtm[1:nrow(labelled_data),]
  full_labelled_dtm <- Matrix::Matrix(full_labelled, sparse = TRUE)

  trainsplit <- head(full_labelled, nrow(full_labelled)*(1-val_split))
  train_labelled_dtm <- Matrix::Matrix(trainsplit, sparse = TRUE)

  valsplit <- tail(full_labelled, nrow(full_labelled)*val_split)
  val_labelled_dtm <- Matrix::Matrix(valsplit, sparse = TRUE)

  # CHANGE TO DATAFRAME!!
  labels <- as.data.frame(labelled_data[, topics])
  colnames(labels) <- topics
  fulllabels <- as.matrix(labels)

  # the split might need to be changed so there's no bias?
  trainlabels <- as.data.frame(head(labels, nrow(labels)*(1-val_split)))

  trainlabels <- as.matrix(trainlabels)

  vallabels <- as.data.frame(tail(labels, nrow(labels)*val_split))
  vallabels <- as.matrix(vallabels)

  unlabelled_dtm <- Matrix::Matrix(tail(dtm, nrow(unlabelled_data)), sparse=TRUE)

  list('labelled_dtm' = full_labelled_dtm, 'train_labelled_dtm' = train_labelled_dtm, 'valid_labelled_dtm' = val_labelled_dtm,
       'unlabelled_dtm' = unlabelled_dtm, 'labels' = fulllabels, 'train_labels' = trainlabels, 'val_labels' = vallabels)
}
