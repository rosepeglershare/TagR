#' explain_comment
#'
#' Using the xgboost model and explainer from the build_topic_explainer function, this function explains how the algorithm reached its prediction.
#'
#' @param xgbmodel Xgbmodel output from the build_topic_explainer function.
#' @param explainer Explainer output from the build_topic_explainer function.
#' @param unlabelled_dtm Unlabelled document-term matrix.
#' @param comment_index The index of the comment in the unlabelled dataset that the user wants to explore.
#' @param threshold The waterfall chart will group all variables with absolute impact less than the threshold into a variable called 'Other'. Default: 0.02
#'
#' @return A waterfall chart that demonstrates how the inclusion/exclusion of certain words informed its decision.
#' @export

explain_comment <- function(xgbmodel, explainer, unlabelled_dtm, comment_index, threshold = 0.02){
  # build test matrix ready for waterfall function
  testdata <- xgboost::xgb.DMatrix(data = unlabelled_dtm)

  # the user must choose which comment they want the function to explain
  # a waterfall graph is output that details how the terms of the comment influenced the decision making
  waterfall <- xgboostExplainer::showWaterfall(xgbmodel, explainer, testdata, unlabelled_dtm , comment_index, type = "binary", threshold = threshold)

  waterfall
}
