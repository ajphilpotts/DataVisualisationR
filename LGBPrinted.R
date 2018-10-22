### Function version ------------------------

lgb.train.printed <- function(data,
                              feature_names,
                              watchlist,
                              params,
                              nrounds = 100,
                              early_stopping_rounds = 20,
                              verbose = TRUE,
                              maximise = TRUE,
                              log.path,
                              model.path,
                              varImp.path,
                              append=FALSE,
                              type="output",
                              split=TRUE) {
  
  require(xgboost)
  require(readr)
  require(knitr)
  require(stringr)
  require(tidyr)
  
  c.time <- Sys.time()
  
  ## Create print directory
  
  log.path.full <- paste(log.path,c.time,sep=" ")
  dir.create(path=log.path.full,showWarnings = FALSE)
  
  ## Output name
  out.path <- paste(log.path.full,"/TrainOut.txt",sep="")
  
  
  # Write XGBoost output to txt file
  sink(file=out.path,append=FALSE,type="output",split=TRUE)
  
  # Build model with verbose to txt (and printed to screen)
  
  cat(paste("### Light GBM Training - ",Sys.time(),"\n",sep = ""))
  print(kable(as.data.frame(parameters)))
  cat("\n\nModel Output: \n\n")
  
  
  model.Curr <- lgb.train(data = data,
                          valids = watchlist,
                          params = params,
                          nrounds = nrounds,
                          early_stopping_rounds = early_stopping_rounds,
                          verbose = verbose,
                          maximise = maximise
  ) 
  
  sink()
  
  # Save Feature Importance
  varImp <- lgb.importance(feature_names = feature_names,model = model.Curr)
  write_csv(varImp, path = paste(varImp.path,"/FeatImp.csv",sep=""))
  write_csv(varImp, path = paste(log.path.full,"/FeatImp.csv",sep=""))
  
  # Save Model
  saveRDS(model.Curr,file = paste(model.path,"/LGBModel.RDS",sep=""))
  saveRDS(model.Curr,file = paste(varImp.path,"/LGBModel.RDS",sep=""))
  saveRDS(model.Curr,file = paste(log.path.full,"/LGBModel.RDS",sep=""))
  
  # Feed output back into local environment to be printed to AUC plot
  # lastOut <- read.table(file = out.path,
  #                     skip = 10,
  #                     header = FALSE,
  #                     sep="\t",
  #                     col.names = c('rowno','Train.AUC.Raw','Test.AUC.Raw'))
  # 
  # # Prepare / melt data frame to feed into plot
  # model.perf <- lastOut %>%
  #   mutate(Iteration = 1:n(),
  #          Train.AUC = sapply(lastOut$Train.AUC,str_split_fixed,":",n=2)[2,],
  #          Test.AUC = sapply(lastOut$Test.AUC,str_split_fixed,":",n=2)[2,]) %>%
  #   select(Iteration,Train.AUC,Test.AUC) %>%
  #   gather(key="Dataset",value="AUC",-Iteration) %>%
  #   mutate(AUC = as.numeric(AUC))
  
  # # Save Plot (Plotting functions currently broken)
  # auc.plot <- AUCPlot(model.perf,c.time)
  # 
  #   ggsave(
  #   paste(log.path.full,"/AUC Plot.png",sep=""),
  #   auc.plot,
  #   width=11.69,height=6,
  #   dpi=720
  # )
  
}
