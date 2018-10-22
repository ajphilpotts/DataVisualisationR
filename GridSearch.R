### Gridsearch (Custom) ----------

GridSearch <- function(paramGrid,
                       features,
                       train,
                       test,
                       nTree = 100,
                       nfolds=5,
                       verbose=TRUE,
                       ...) {
  
  require(xgboost)
  require(data.table)
  
  add_params <- list(...)
  
  # Convert to DF to list for xgb param input
  g.params <- list(
    
    "booster"="gbtree",
    "objective" = "binary:logistic",
    "eta" = paramGrid[["eta"]],
    "max_depth" = paramGrid[["max_depth"]],
    "min_child_weight" = paramGrid[["min_child_weight"]],
    "eval_metric" = "auc",
    "subsample" = paramGrid[["subsample"]],
    "colsample_bytree" = paramGrid[["colsample_bytree"]],
    "gamma" = paramGrid[["gamma"]],
    "scale_pos_weight" = paramGrid[["scale_pos_weight"]],
    "max_delta_step" = paramGrid[["max_delta_step"]]
    
  )
  
  print.params <- c(g.params,add_params)
  
  # Display progress to terminal
  cat('\n#######################\n# CV iteration start #\n#######################\n\n\n')
  cat('Cross validating on the following XGB tuning parameters:\n\n')

  # print(kable(t(as.data.table(print.params)),
  #             col.names = c('Parameter','Trial Value')))
  print(kable(data.table(Parameter=names(print.params),
                            `Trial Value`=as.character(print.params))))
  
  cat("\n")
  
  param.CV <-  xgb.cv(data = trainMatrix,
                      watchlist = list(train = train, test = test),
                      feature_names=features,
                      params = g.params,
                      nrounds = nTree,
                      nfold = nfolds,
                      prediction = FALSE,
                      verbose = verbose,
                      ...
  ) 
  
  param.AUC <- as.data.table(Iteration=seq.int(nrow(param.CV$evaluation_log)),param.CV$evaluation_log)
  
  # Achieved AUC of final iteration for parameter combination
  
  g.eta = paramGrid[["eta"]]
  g.max_depth = paramGrid[["max_depth"]]
  g.min_child_weight = paramGrid[["min_child_weight"]]
  g.subsample = paramGrid[["subsample"]]
  g.colsample_bytree = paramGrid[["colsample_bytree"]]
  g.gamma = paramGrid[["gamma"]]
  
  BestIter <- head(arrange(param.AUC,-test_auc_mean),1)
  
  Train.AUC <-BestIter$train_auc_mean
  Test.AUC <- BestIter$test_auc_mean
  
  cat("\n\n** Grid step completed **\n\n")
  
  cat("\nBest Iteration: [",BestIter[[1]],"]",
      "\nTraining AUC: ",Train.AUC,
      "\nTest AUC: ",Test.AUC,
      "\n\n")
  
  return(data.table(
                    as.data.table(g.params),
                    Train.AUC,
                    Test.AUC))

}