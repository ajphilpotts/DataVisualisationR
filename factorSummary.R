# With Timestamps
factorSummary <- function(dt) {
  
  require(data.table)
  
  #Check NA function for metadata tables
  countNA <- function(a) {
    return(sum(is.na(a)))
  }
  
  #Count levels in factor
  levelCount <- function(v) {
    ifelse(class(v)=="factor",length(levels(v)),0)
  }
  
  # Collect in Table
  summ <- data.frame(colname=names(dt),
                     NAs=sapply(dt,countNA),
                     class=do.call(rbind,sapply(dt,class))[,1],
                     levels=do.call(rbind,sapply(dt,levelCount))[,1]) %>%
    mutate(perc.NAs=round(NAs/nrow(dt),digits=2)) %>%
    select(colname,NAs,perc.NAs,class,levels)
  
  setDT(summ)
  
  return(summ)
}

# Without Timestamps
factorSummary2 <- function(dt) {
  
  require(data.table)
  
  #Check NA function for metadata tables
  countNA <- function(a) {
    return(sum(is.na(a)))
  }
  
  #Count levels in factor
  levelCount <- function(v) {
    ifelse(class(v)=="factor",length(levels(v)),0)
  }
  
  
  # Collect in Table
  summ <- data.frame(colname=names(dt),
                     NAs=sapply(dt,countNA),
                     class=sapply(dt,class),
                     levels=sapply(dt,levelCount)) %>%
    mutate(perc.NAs=round(NAs/nrow(dt),digits=2)) %>%
    select(colname,NAs,perc.NAs,class,levels)
  
  setDT(summ)
  
  return(summ)
}
