### Function version ------------------------

xgb.cv.printed <- function(data,
                           feature_names,
                           watchlist,
                           params,
                           nfold,
                           nrounds = 100,
                           early_stopping_rounds = 20,
                           verbose = TRUE,
                           maximise = TRUE,
                           log.path,
                           model.path,
                           varImp.path,
                           append=FALSE,
                           type="output",
                           split=TRUE,
                           ...) {
  
  require(xgboost)
  require(readr)
  require(knitr)
  require(dplyr)
  require(tidyr)
  require(ggplot2)
  
  c.time <- Sys.time()
  
  add_params <- list(...)
  
  ## Create print directory
  
  log.path.full <- paste(log.path,c.time,sep=" ")
  dir.create(path=log.path.full,showWarnings = FALSE)
  
  ## Output name
  XGBout.path <- paste(log.path.full,"/TrainOut.txt",sep="")
  
  
  # Write XGBoost output to txt file
  sink(file=XGBout.path,append=FALSE,type="output",split=TRUE)
  
  # Build model with verbose to txt (and printed to screen)
  
  print.params <- c(params,add_params)
  
  cat(paste("### XGBoost Training - ",Sys.time(),"\n",sep = ""))
  print(kable(data.table(Parameter=names(print.params),
                         Value=as.character(print.params))))
  cat("\n")
  
  
  model.Curr <- xgb.cv(data = trainMatrix,
                          watchlist = list(train = trainMatrix, test = testMatrix),
                          params = params,
                          nrounds = nrounds,
                          nfold = nfold,
                          prediction = FALSE,
                          early_stopping_rounds = early_stopping_rounds,
                          verbose = verbose,
                          maximise = maximise,
                          ...
                          ) 
  
  sink()
  
  cat(class(model.Curr))
  
  # Save Feature Importance
  varImp <- xgb.importance(feature_names = feature_names,model = model.Curr)
  write_csv(varImp, path = paste(varImp.path,"/FeatImp.csv",sep=""))
  write_csv(varImp, path = paste(log.path.full,"/FeatImp.csv",sep=""))
  
  # Save Model
  saveRDS(model.Curr,file = paste(model.path,"/XGBModel.RDS",sep=""))
  saveRDS(model.Curr,file = paste(varImp.path,"/XGBModel.RDS",sep=""))
  saveRDS(model.Curr,file = paste(log.path.full,"/XGBModel.RDS",sep=""))
  
  
  # model.perf <- model.CV.Curr %>%
  #   select(-contains("std")) %>%
  #   mutate(Iteration = 1:n()) %>%
  #   gather(key="Dataset",value="AUC",-Iteration)
  # 
  # auc.plot <- AUCPlot(model.perf,c.time)
  # 
  # ggsave(
  #   paste(dir.out.full,"/AUC Plot.png",sep=""),
  #   auc.plot,
  #   width=11.69,height=6,
  #   dpi=720
  # )
  
}
