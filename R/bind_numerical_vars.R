#' bind_numerical_vars
#'
#' Add numerical variables to the document term matrices such as follower count, favourites, and reach.
#'
#' @param labelled_raw Original labelled dataframe before any pre-processing.
#' @param unlabelled_raw Original unlabelled datafame before any pre-processing.
#' @param full_labelled_dtm Full labelled document term matrix.
#' @param train_labelled_dtm Training labelled document term matrix.
#' @param valid_labelled_dtm Validation labelled document term matrix.
#' @param unlabelled_dtm Unlabelled document term matrix.
#' @param numerical_vars List of numerical variables to include.
#' @param val_split The amount of training data that should be included in the validation set. Must be the same as in create_dt_matrices function. Default: 0.2.
#'
#' @return A complete labelled document-term matrix,
#' a labelled document-term matrix split into training and validation sets,
#' and an unlabelled document-term matrix used for predictions.
#'
#' @export

bind_numerical_vars <- function(labelled_raw,
                                unlabelled_raw,
                                full_labelled_dtm,
                                train_labelled_dtm,
                                valid_labelled_dtm,
                                unlabelled_dtm,
                                numerical_vars,
                                val_split = 0.2){

  # Split numerical data using validation split
  numerical_data <- labelled_raw[, numerical_vars]
  train_num <- utils::head(numerical_data, nrow(numerical_data) * (1-val_split))
  val_num <- utils::tail(numerical_data, nrow(numerical_data) * val_split)

  # convert data to matrix form
  numerical_data <- Matrix::Matrix(as.matrix(numerical_data), sparse=TRUE)
  train_num <- Matrix::Matrix(as.matrix(train_num), sparse = TRUE)
  val_num <- Matrix::Matrix(as.matrix(val_num), sparse = TRUE)

  # bind with text matrices
  full_labelled_dtm <- cbind(full_labelled_dtm, numerical_data)
  train_labelled_dtm <- cbind(train_labelled_dtm, train_num)
  valid_labelled_dtm <- cbind(valid_labelled_dtm, val_num)

  # bind numerical data with unlabelled dataset
  numerical_data_unlabelled <- unlabelled_raw[, numerical_vars]
  numerical_data_unlabelled <- Matrix::Matrix(as.matrix(numerical_data_unlabelled), sparse = TRUE)
  unlabelled_dtm <- cbind(unlabelled_dtm, numerical_data_unlabelled)

  list('full_labelled_dtm' = full_labelled_dtm, 'train_labelled_dtm' = train_labelled_dtm,
       'valid_labelled_dtm' = valid_labelled_dtm, 'unlabelled_dtm' = unlabelled_dtm)

}
