#' build_topic_explainer
#'
#' For the chosen topic, this function creates an explainer that helps the user to better understand how the xgboost model creates predictions.
#' The explainer is to be created before using the explain_comment function.
#'
#' @param full_labelled_dtm Full labelled document-term matrix.
#' @param full_labels Labelled matrix for labelled_dtm.
#' @param topic The topic to explain.
#' @param model Xgboost model
#'
#' @return The trained xgboost model and explainer for the chosen topic
#' @export

build_topic_explainer <- function(full_labelled_dtm,
                                  full_labels,
                                  topic,
                                  model){


  # build matrices ready for explainer function
  traindata <- xgboost::xgb.DMatrix(data = full_labelled_dtm, label = full_labels[, topic])

  # build explainer using trained model and data
  explainer <- xgboostExplainer::buildExplainer(model, traindata, type = 'binary')

  explainer
}
