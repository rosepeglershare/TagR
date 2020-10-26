#' build_topic_explainer
#'
#' For the chosen topic, this function creates an explainer that helps the user to better understand how the xgboost model creates predictions.
#' The explainer is to be created before using the explain_comment function.
#'
#' @param unlabelled_raw Original unlabelled dataframe before any pre-processing.
#' @param labelled_dtm Full labelled document-term matrix.
#' @param unlabelled_dtm Unlabelled document-term matrix.
#' @param labels_matrix Labelled matrix for labelled_dtm.
#' @param topic The topic to explain.
#' @param parameters Default list of parameters if user did not perform hyperparameter tuning.
#' @param parameters_df A dataframe with columns representing parameters and rows representing an optimal parameter set for each topic.
#'
#' @return The trained xgboost model and explainer for the chosen topic
#' @export

build_topic_explainer <- function(unlabelled_raw,
                                  labelled_dtm,
                                  unlabelled_dtm,
                                  labels_matrix,
                                  topic,
                                  parameters = list(booster = "gbtree",
                                                    objective = "binary:logistic",
                                                    max_depth = 6,
                                                    eta = 0.3,
                                                    subsample = 1,
                                                    colsample_bytree = 1,
                                                    min_child_weight = 1),
                                  parameters_df = NULL){

  # the term 'tree' has to be removed otherwise it interferes with the explainer function
  labelled_dtm <- labelled_dtm[, colnames(labelled_dtm) != 'tree']
  unlabelled_dtm <- unlabelled_dtm[, colnames(unlabelled_dtm) != 'tree']

  # build an xgbmodel for the chosen topic using either the default parameters or the input parameter set
  cat("\nBuilding xgboost model...\n")
  if (length(is.na(parameters_df)) == 0){
    xgbmodel <- xgboost::xgboost(labelled_dtm, labels_matrix[, topic], params = parameters, nrounds = 100,
                                 verbose = 1, print_every_n = 10)
  } else {
    parameters_df <- parameters_df[,-1]
    paramlist <- list()

    for (col in names(parameters_df)){
      paramlist[[col]] <- parameters_df[topic, col]
    }

    xgbmodel <- xgboost::xgboost(labelled_dtm, labels_matrix[, topic], params = paramlist, nrounds = 100,
                                 verbose = 1, print_every_n = 10)

  }

  # build matrices ready for explainer function
  traindata <- xgboost::xgb.DMatrix(data = labelled_dtm, label = labels_matrix[, topic])
  rownames(unlabelled_dtm) <- rownames(unlabelled_raw)

  # build explainer using trained model and data
  explainer <- xgboostExplainer::buildExplainer(xgbmodel, traindata, type = 'binary')

  list('xgbmodel' = xgbmodel, 'explainer' = explainer)
}
