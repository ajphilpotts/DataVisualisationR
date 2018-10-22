### Gridsearch (Custom) ----------

GridSearch_Wrapped <- function(xgb.grid,
                       features,
                       train,
                       test,
                       nTree = 100,
                       nfolds=5,
                       verbose=TRUE,
                       ...) {

  grid.AUC <- setDT(do.call(bind_rows,apply(xgb.grid,
                                            1,
                                            FUN=GridSearch,
                                            train=train,
                                            test=test,
                                            features=features,
                                            nfold=nfolds,
                                            verbose=verbose,
                                            nTree=nTree,
                                            ...
  )) %>% arrange(-Test.AUC))
  
  return(grid.AUC)
}