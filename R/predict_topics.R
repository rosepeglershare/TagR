#' predict_topics
#'
#' Trains an xgboost model for each topic and uses this to predict the probability that unlabelled comments belong to this topic or not.
#'
#' @param unlabelled_raw Original unlabelled dataframe before any pre-processing.
#' @param labelled_dtm Full labelled document-term matrix.
#' @param unlabelled_dtm Unlabelled document-term matrix used for predictions.
#' @param labels_matrix Labels matrix for labelled_dtm.
#' @param text_vars List of text variables.
#' @param num_vars List of numerical variables.
#' @param topics List of topics.
#' @param parameters Default list of parameters if user did not perform hyperparameter tuning.
#' @param parameters_df A dataframe with columns representing parameters and rows representing an optimal parameter set for each topic.
#' @param nrounds Number of rounds that the xgboost model should be trained for. Default: 1000
#'
#' @return A dataframe with the original comments, chosen attributes and probabilities that they belong to each topic.
#' @export

predict_topics <- function(unlabelled_raw,
                           labelled_dtm,
                           unlabelled_dtm,
                           labels_matrix,
                           text_vars,
                           num_vars,
                           topics,
                           parameters = list(booster = "gbtree",
                                                     objective = "binary:logistic",
                                                     max_depth = 6,
                                                     eta = 0.3,
                                                     subsample = 1,
                                                     colsample_bytree = 1,
                                                     min_child_weight = 1),
                           parameters_df = NULL,
                           nrounds = 1000){

  # create empty dataframe to store results
  finaldf <- data.frame(matrix(ncol = length(topics), nrow = nrow(unlabelled_dtm)))
  colnames(finaldf) <- topics
  rownames(finaldf) <- rownames(unlabelled_dtm)

  # if not using own parameters, use the default ones to train model for each topic
  if (length(is.na(parameters_df)) == 0){
    for (topic in topics){
      label <- as.integer(labels_matrix[,topic])

      cat("\nTraining model for topic: ", topic)
      xgbmodel <- xgboost::xgboost(labelled_dtm, label, params = parameters, nrounds = nrounds, verbose = 1, print_every_n = 100)

      cat("\nPredicting on test set\n")
      # predict on unlabelled set using trained model
      predicted <- stats::predict(xgbmodel, unlabelled_dtm)
      finaldf[[topic]] <- predicted
    }
  } else {
    # if using own parameters, iterate through each set to train for each topic
    parameters_df <- parameters_df[,-1]

    for (topic in topics){
      paramlist <- list()

      for (col in names(parameters_df)){
        paramlist[[col]] <- parameters_df[topic, col]
      }

      label <- as.integer(labels_matrix[,topic])

      cat("\nTraining model for topic: ", topic, "\n")
      xgbmodel <- xgboost::xgboost(labelled_dtm, label, params = paramlist, nrounds = nrounds, verbose = 1, print_every_n = 100)

      cat("\nPredicting on test set\n")
      # predict on unlabelled set using trained model
      predicted <- stats::predict(xgbmodel, unlabelled_dtm)
      finaldf[[topic]] <- predicted
    }

  }

  # reset rownames to match with original dataset
  rownames(finaldf) <- NULL
  finaldf <- cbind(unlabelled_raw[, text_vars], finaldf)
  finaldf <- cbind(unlabelled_raw[, num_vars], finaldf)
  colnames(finaldf) <- do.call(c, list(text_vars, topics))

  finaldf
}
