

# Tile Plot (ventiles)

CrossTab_Tile <- function(x,y,freq) {
  
  dt <- data.frame(x,y,freq)

  plot <- ggplot(dt,aes(y=y,x=x)) +
    
    geom_tile(aes(fill=freq),color="white",size=1) +
    
    # Formatting
    ylab('Quad Play Ventile') +
    xlab('Acquisition Model Ventile') +
    
    labs(title="Quad Play Model vs Acquisition Model - Cross Tabulated Frequency",
         subtitle=paste(Sys.Date())) +
    
    scale_y_continuous(expand = c(0,0),
                       breaks=pretty_breaks(n=20)) +    # Modify xy scales. Moved to (0,0) to remove gap between data and axis
    scale_x_continuous(expand = c(0,0),
                       breaks=pretty_breaks(n=20)) +
    
    #scale_fill_continuous_tableau(palette = "Blue-Green Sequential") +
    #scale_fill_continuous_tableau(palette = "Area Red") +
    scale_fill_continuous(low="#3EA5A5",high="#FC2E2E") +
    
    #Theming
    theme_minimal() + 
    theme(plot.caption = element_text(family = "Arial",hjust = 0,size=6),
          plot.margin = margin(t=12,r=14,l=10,b=10.5),
          
          plot.title = element_text(face='bold',size=12),
          plot.subtitle = element_text(face='italic',size=9),
          
          axis.line = element_line(size=.5),
          axis.text.x = element_text(size=9),
          axis.text.y = element_text(size=9),
          axis.ticks = element_blank(),
          axis.title.x = element_text(face='italic',margin = margin(t=10,b=5,l=5,r=5)),
          axis.title.y = element_text(face='italic',margin = margin(t=5,b=5,l=5,r=10))
          
          #Legend
          #legend.position = 'bottom',
          #legend.direction = 'horizontal',
          #legend.title = element_text(face='italic')
          # legend.box.margin = margin(l=150)
    )

  return(plot)
  
}

# Density Plot (scores)

CrossTab_Density <- function(x,y,samples=1000,percent.rank=TRUE) {
  
  dt <- data.table(x,y)
  
  # Standardise
  #dt <- as.data.frame(apply(dt,2,function(x) {(x-mean(x,na.rm = TRUE))/(sd(x,na.rm = TRUE))}))
    
  # Density sample
  samplerows <- sample(1:nrow(dt),10000)
  sampledensity <- dt[samplerows,] 
  sampledensity <- sampledensity %>%
    arrange(x) %>%
    mutate(x.rank=row_number(),
           x.perc=x.rank/length(samplerows)) %>%
    arrange(y) %>%
    mutate(y.rank=row_number(),
           y.perc=y.rank/length(samplerows))
  
  # Points sample
  samplerows <- sample(1:nrow(dt),samples)
  samplepoints <- dt[samplerows,]
  
  plot <- ggplot(sampledensity,aes(y=y.perc,x=x.perc)) +
    
    stat_density2d(aes(alpha=..level..),geom="polygon") +
    geom_point(data=sampledensity,aes(y=y.perc,x=x.perc),color="#D31111",size=1,alpha=.02) +
    
    scale_alpha_continuous(limits=c(0,2),breaks=seq(0,2,by=0.25)) +
    scale_x_continuous()
  
  return(plot)
  
}