#' hyperparameter_tuning
#'
#' Finds the best set of xgboost parameters for each topic using random search.
#'
#' Parameters:
#' \itemize{
#' \item{max_depth: Maximum depth of a tree. Increasing this value will make the model more complex and more likely to overfit.}
#' \item{eta: Step size shrinkage used in update to prevent overfitting. }
#' \item{subsample: Subsample ratio of the training instances. Setting it to 0.5 means that XGBoost would randomly sample half of the training data prior to growing trees and this will prevent overfitting. }
#' \item{colsample_bytree: The subsample ratio of columns when constructing each tree. Subsampling occurs once for every tree constructed.}
#' \item{min_child_weight: Minimum sum of instance weight (hessian) needed in a child. If the tree partition step results in a leaf node with the sum of instance weight less than min_child_weight, then the building process will give up further partitioning.}
#' }
#'
#' @param train_labelled_dtm Training labelled document-term matrix.
#' @param valid_labelled_dtm Validation labelled document-term matrix.
#' @param train_labels Training labels matrix.
#' @param val_labels Validation labels matrix.
#' @param topics List of topics.
#' @param num_its Number of iterations to run for each topic. Default: 1000
#'
#' @return A dataframe with columns representing parameters and rows representing an optimal parameter set for each topic.
#' @export

hyperparameter_tuning <- function(train_labelled_dtm, valid_labelled_dtm, train_labels, val_labels, topics,
                                  num_its = 1000){
  set.seed(100)

  parameters_list = list()

  # Create n rows with random hyperparameters
  for (iter in 1:num_its){
    param <- list(booster = "gbtree",
                  objective = "binary:logistic",
                  max_depth = sample(3:10, 1),
                  eta = runif(1, .01, .3),
                  subsample = runif(1, .7, 1),
                  colsample_bytree = runif(1, .6, 1),
                  min_child_weight = sample(0:10, 1)
    )
    parameters <- as.data.frame(param)
    parameters_list[[iter]] <- parameters
  }

  parameters_df = do.call(rbind, parameters_list)

  # create empty dataframe to store results
  finalresults <- data.frame(matrix(ncol = 8, nrow = 0))
  x <- c("val_acc", "booster","objective", "max_depth", "eta", "subsample","colsample_bytree",
         "min_child_weight")
  colnames(finalresults) <- x

  # iterate through each topic to find best parameter set
  for (topic in topics){
    cat("\nFinding best parameter set for topic: ", topic, "\n")

    # set up matrices
    trainlabel <- train_labels[,topic]
    vallabel <- val_labels[,topic]
    dtrain <- xgboost::xgb.DMatrix(data = train_labelled_dtm, label = trainlabel)
    dval <- xgboost::xgb.DMatrix(data = valid_labelled_dtm, label = vallabel)

    # random search
    lowest_error_list = list()
    pb <- progress_bar$new(total = nrow(parameters_df), clear = FALSE)

    # iterate through each row of the parameters dataset
    for (row in 1:nrow(parameters_df)){
      pb$tick()
      Sys.sleep(1 / nrow(parameters_df))

      set.seed(20)
      mdcv <- xgboost::xgb.train(data=dtrain,
                                 booster = "gbtree",
                                 objective = "binary:logistic",
                                 max_depth = parameters_df$max_depth[row],
                                 eta = parameters_df$eta[row],
                                 subsample = parameters_df$subsample[row],
                                 colsample_bytree = parameters_df$colsample_bytree[row],
                                 min_child_weight = parameters_df$min_child_weight[row],
                                 nrounds= 300,
                                 eval_metric = "error",
                                 early_stopping_rounds= 30,
                                 print_every_n = 100,
                                 verbose = 0,
                                 watchlist = list(train= dtrain, val= dval)
      )

      # store results
      lowest_error <- as.data.frame(1 - min(mdcv$evaluation_log$val_error))
      lowest_error_list[[row]] <- lowest_error
    }

    # Create object that contains all accuracies
    lowest_error_df = do.call(rbind, lowest_error_list)

    # Bind columns of accuracy values and random hyperparameter values
    # filter final dataframe so it contains a parameter set leading to the highest accuracy
    randomsearch = cbind(lowest_error_df, parameters_df)
    bestresult <- as.data.frame(randomsearch) %>%
      rename(val_acc = `1 - min(mdcv$evaluation_log$val_error)`) %>%
      filter(val_acc == max(val_acc))

    # return dataset with each topic and best parameter set
    finalresults <- rbind(finalresults, bestresult[1,])
  }

  rownames(finalresults) <- topics

  finalresults
}
