
# Target uplift by ntile -----------------------

NTilePlot <- function(dt,ntiles=10,title="",caption="") {
  
  require(ggplot2)
  require(ggthemes)
  require(scales)
  
  scorePopulation <- nrow(dt)
  scoreSales <- sum(dt[,TARGET])
  
  liftList <- list()
  
  for (i in 1:ntiles) {
    ntile <- dt[(nrow(dt)*(1/ntiles)*(i-1)+1):(nrow(dt)*(1/ntiles)*i), ]
    ntilePopulation <- nrow(ntile)
    ntileSales <- sum(ntile[,TARGET])
    lift <- ((ntileSales/ntilePopulation)/(scoreSales/scorePopulation))
    liftList[[i]] <- lift
  }
  
  ntileUplift <- as.data.table(cbind(c(1:ntiles),unlist(liftList)))
  names(ntileUplift) <- c("ntile","Uplift")
  
  dt.ribbon <- ribbonDivide(ntileUplift$ntile,ntileUplift$Uplift,1)
  dt.upper <- dt.ribbon %>% select(ntile=x ,Uplift=shadeUpper)
  dt.lower <- dt.ribbon %>% select(ntile=x,Uplift=shadeLower)
    
  cat('\nntiles calculated\n')
  
  plot <- ggplot(data = ntileUplift, aes(x=ntile,y=Uplift)) +
    
    # Plot Elements
    geom_line(data=ntileUplift, stat="identity",col="#D31111",size=1.1) +
    
    # Shaded uplifts
    geom_ribbon(data=dt.upper,aes(ymax=Uplift,ymin=1),fill="#D31111",alpha=.3) +
    geom_ribbon(data=dt.lower,aes(ymin=1,ymax=Uplift),fill="#3E273E",alpha=.3) +
    
    
    geom_hline(yintercept = 1,alpha=0.8,colour="grey50",size=.5) +
    geom_label_repel(data=ntileUplift, stat="identity",aes(label=sprintf("%0.0f%%",Uplift*100))
               ,size=3,col="black",alpha=.7,nudge_y = .035,
               label.padding=unit(0.15,"lines"),label.r=unit(0.1,"lines")) +

    
    # Formatting
    ylab("Uplift") +
    labs(title = title,
         subtitle = paste(Sys.Date(),sep=" - "),
         caption = caption) +
    scale_x_continuous(breaks = seq(1,ntiles,by=1),
                       expand=c(.01,.01)) +
    scale_y_continuous(breaks=pretty_breaks(n=10),
                       labels = scales::percent) +    # Modify xy scales. Moved to (0,0) to remove gap between data and axis
    
    #scale_colour_manual(values=c("#812981","#7C9B22","#DA3232")) +
    
    #Theming
    theme_minimal() + 
    theme(plot.caption = element_text(family = "Arial",hjust = 0,size=6),
          plot.margin = margin(t=12,r=14,l=10,b=10.5),
          
          plot.title = element_text(face='bold',size=12),
          plot.subtitle = element_text(face='italic',size=9),
          
          axis.line = element_line(size=.5),
          axis.text.x = element_text(size=9),
          axis.ticks = element_blank(),
          axis.title.x = element_text(face='italic',margin = margin(t=10,b=5,l=5,r=5)),
          axis.title.y = element_text(face='italic',margin = margin(t=5,b=5,l=5,r=10)),
          
          #Legend
          legend.position = 'bottom',
          legend.direction = 'horizontal',
          # legend.key.height = unit(2,'line'),
          # legend.key.width=unit(.5,'line'),
          legend.title = element_text(face='italic')
          # legend.box.margin = margin(l=150)
    ) +
    guides(fill = guide_legend(nrow = 1))
  
  cat('\nntile plot created\n\n')
  
  return(list(Uplift=ntileUplift,Plot=plot))
  
}


# Actual target frequency by ntile -----------------------

NTilePlot_Actuals <- function(dt,ntiles=10,title="",caption="",target_name="Target") {
  
  require(ggplot2)
  require(ggthemes)
  require(scales)
  require(stringr)
  
  scorePopulation <- nrow(dt)
  scoreSales <- sum(dt[,TARGET])
  
  ntileList <- list()
  
  for (i in 1:ntiles) {
    ntile <- dt[(nrow(dt)*(1/ntiles)*(i-1)+1):(nrow(dt)*(1/ntiles)*i), ]
    ntilePopulation <- nrow(ntile)
    ntileSales <- sum(ntile[,TARGET])
    ntileList[[i]] <- ntileSales
  }
  
  c.date <- Sys.Date()
  c.month <- substr(c.date,1,7)
  
  ntileActuals <- as.data.table(cbind(c(1:ntiles),unlist(ntileList)))
  names(ntileActuals) <- c("ntile","Actuals")
  
  avg=scoreSales/ntiles
  
  dt.ribbon <- ribbonDivide(x=ntileActuals$ntile,y=ntileActuals$Actuals,d=avg)
  dt.upper <- dt.ribbon %>% select(ntile=x,Actuals=shadeUpper)
  dt.lower <- dt.ribbon %>% select(ntile=x,Actuals=shadeLower)
  
  cat('\nntiles calculated\n')
  
  plot <- ggplot(data = ntileActuals, aes(x=ntile,y=Actuals)) +
    
    # Plot Elements
    geom_line(data=ntileActuals, stat="identity",col="#D31111",size=1.1) +
    
    
    # Shaded uplifts
    geom_ribbon(data=dt.upper,aes(ymax=Actuals,ymin=avg),fill="#D31111",alpha=.3) +
    geom_ribbon(data=dt.lower,aes(ymin=avg,ymax=Actuals),fill="#3E273E",alpha=.3) +
    
    
    geom_hline(yintercept = avg,alpha=0.8,colour="grey50",size=.5) +
    geom_label_repel(data=ntileActuals, stat="identity",aes(label=Actuals)
                     ,size=3,col="black",alpha=.7,nudge_y =  mean(ntileActuals$Actuals/50),
                     label.padding=unit(0.15,"lines"),label.r=unit(0.1,"lines")) +
    
    annotate("text",x=ntiles*0.98,y=avg*1.01,hjust=1,vjust=0,size=3.2,
             label=paste("Mean",target_name,"per ntile =",avg),
             fontface="italic") +
    
      
    # Formatting
    ylab("Target Frequency") +
    labs(title = title,
         subtitle = paste(Sys.Date(),sep=" - "),
         caption = caption) +
    scale_x_continuous(breaks = seq(1,ntiles,by=1),
                       expand=c(.01,.01)) +
    scale_y_continuous(breaks=pretty_breaks(n=10)) +   # Modify xy scales. Moved to (0,0) to remove gap between data and axis
    
    
    #Theming
    theme_minimal() + 
    theme(plot.caption = element_text(family = "Arial",hjust = 0,size=6),
          plot.margin = margin(t=12,r=14,l=10,b=10.5),
          
          plot.title = element_text(face='bold',size=12),
          plot.subtitle = element_text(face='italic',size=9),
          
          axis.line = element_line(size=.5),
          axis.text.x = element_text(size=9),
          axis.ticks = element_blank(),
          axis.title.x = element_text(face='italic',margin = margin(t=10,b=5,l=5,r=5)),
          axis.title.y = element_text(face='italic',margin = margin(t=5,b=5,l=5,r=10)),
          
          #Legend
          legend.position = 'bottom',
          legend.direction = 'horizontal',
          # legend.key.height = unit(2,'line'),
          # legend.key.width=unit(.5,'line'),
          legend.title = element_text(face='italic')
          # legend.box.margin = margin(l=150)
    ) +
    guides(fill = guide_legend(nrow = 1))
  
  cat('\nntile plot created\n\n')
  
  return(list(data=ntileActuals,Plot=plot))
  
}


# Cumulative uplift by target % -----------------------


cumUplift <- function(dt,buckets=20,title="",caption="",span=.75) {
  
  require(ggplot2)
  require(ggthemes)
  require(scales)
  require(stringr)
  
  c.date <- Sys.Date()
  c.month <- substr(c.date,1,7)
  
  scorePopulation <- nrow(dt)
  scoreSales <- sum(dt[,TARGET])
  
  bucketList <- list()
  
  for (i in 1:buckets) {
    ntile <- dt[(nrow(dt)*(1/buckets)*(i-1)+1):(nrow(dt)*(1/buckets)*i), ]
    ntilePopulation <- nrow(ntile)
    ntileSales <- sum(ntile[,TARGET])
    bucketList[[i]] <- ntileSales
  }
  
  bucketActuals <- as.data.table(cbind(c(0:buckets),c(0,unlist(bucketList))))
  names(bucketActuals) <- c("bucket","actuals")
  
  bucketCum <- bucketActuals %>%
    arrange(bucket) %>%
    mutate(popPerc=bucket/buckets,
           cumSales=cumsum(actuals),
           cumSalesPerc=cumSales/scoreSales)
  
  cat('\nBucket sales calculated\n')
  
  plot <- ggplot(data = bucketCum, aes(x=popPerc,y=cumSalesPerc)) +
    
    # Plot Elements
    stat_smooth(col="#D31111",size=1.1,span=span) +
    #geom_point(stat="identity",col="#D31111",size=1.1) +
    geom_abline(slope=1,intercept = 0,alpha=0.8,colour="grey50",size=.5) +
    geom_text_repel(data = bucketCum[bucketCum$popPerc<1,], stat="identity",aes(x=popPerc,y=cumSalesPerc,label=sprintf("%0.0f%%",cumSalesPerc*100)),size=3,col="black",alpha=.7,nudge_y = .03) +
    
    # Formatting
    ylab("% Target Achieved") +
    xlab("% Audience Targeted") +
    labs(title = title,
         subtitle = paste(Sys.Date(),sep=" - "),
         caption = caption) +
    scale_x_continuous(expand=c(.01,.01),limits=c(0,1),breaks = seq(0,1,by=.1),labels = scales::percent) +
    scale_y_continuous(expand=c(.01,.01),limits=c(0,1),breaks = seq(0,1,by=.1),labels = scales::percent) + 
    
    
    #Theming
    theme_minimal() + 
    theme(plot.caption = element_text(family = "Arial",hjust = 0,size=6),
          plot.margin = margin(t=12,r=14,l=10,b=10.5),
          
          plot.title = element_text(face='bold',size=12),
          plot.subtitle = element_text(face='italic',size=9),
          
          axis.line = element_line(size=.5),
          axis.text.x = element_text(size=9),
          axis.ticks = element_blank(),
          axis.title.x = element_text(face='italic',margin = margin(t=10,b=5,l=5,r=5)),
          axis.title.y = element_text(face='italic',margin = margin(t=5,b=5,l=5,r=10)),
          
          #Legend
          legend.position = 'bottom',
          legend.direction = 'horizontal',
          # legend.key.height = unit(2,'line'),
          # legend.key.width=unit(.5,'line'),
          legend.title = element_text(face='italic')
          # legend.box.margin = margin(l=150)
    ) +
    guides(fill = guide_legend(nrow = 1))
  
  cat('\nntile plot created\n\n')
  
  return(list(data=bucketActuals,Plot=plot))
  
}