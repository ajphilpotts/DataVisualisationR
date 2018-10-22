# Feat.Source generation function
featureSource <- function(dt,sources,idvars) {
  out <- as.data.table(str_split_fixed(names(dt),"_",n=2))
  names(out) <- c('Source','Feature')
  out$Name <- names(dt)
  
  out <- out %>% 
    mutate(Source = ifelse(Source %in% sources,Source,'BASE'),
           Feature = ifelse(Source=='BASE',Name,Feature),
           VarClass = ifelse(Source=='TARGET','TARGET',
                             ifelse(Name %in% idvars,'ID','FEATURE')
           )
    )
  setDT(out)
  return(out)
}