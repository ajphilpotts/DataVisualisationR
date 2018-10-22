
###### Top features plot #######

featImpPlot <- function(featImp) {
  
  setDT(featImp)
  
  # Palette ----------------------------
  
  # Base palette
  base.palette <- c("#D31111","#3E273E","#E1CA96","#ACA885")
  # Add grey for "OTHER" vars
  plot.palette <- c(base.palette[1:nrow(featImp[,.N,by=Source])-1],"#81867E")
  
  # Create Plot ----------------------------
  
  plot <- ggplot(featImp) +  # Initialize ggplot2. Specify date set, X and Y columns
    
    # Plot Elements
    geom_bar(aes(x=reorder(Feature,Gain),y=Gain,fill=Source),stat='identity',position = "dodge") +
    coord_flip() +
    
    # Formatting
    ylab('Gain') +
    xlab('Feature') +
    
    labs(title=paste(dir.project,"Feature Importance - Top 25",sep=" "),
         subtitle=paste(Sys.Date())) +
    
    scale_y_continuous(labels = scales::percent,
                       expand = c(0,0),
                       breaks=pretty_breaks(n=10)
    ) +    # Modify xy scales. Moved to (0,0) to remove gap between data and axis
    
    # Colours
    scale_fill_manual(values=plot.palette) +
    
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


return(plot)


}

###### blank features plot #######

featImpBlank <- function(featImp) {
  
  setDT(featImp)
  
  # Palette ----------------------------
  
  # Base palette
  base.palette <- c("#D31111","#3E273E","#E1CA96","#ACA885")
  # Add grey for "OTHER" vars
  plot.palette <- c(base.palette[1:nrow(featImp[,.N,by=Source])-1],"#81867E")
  
  # Create Plot ----------------------------
  
  plot <- ggplot(featImp) +  # Initialize ggplot2. Specify date set, X and Y columns
    
    # Plot Elements
    geom_bar(aes(x=reorder(Feature,-Gain),y=Gain,fill=Source),stat='identity',position = "dodge") +
    
    # Formatting
    ylab('Gain') +
    xlab('Feature') +
    
    labs(title=paste(dir.project,"Feature Importance - All",sep=" ")) +
    
    scale_y_continuous(labels = scales::percent,
                       expand = c(0,0),
                       breaks=pretty_breaks(n=10)
    ) +    # Modify xy scales. Moved to (0,0) to remove gap between data and axis

    # Colours
    scale_fill_manual(values=plot.palette) +
    
    #Theming
    
    theme_minimal() + 
    theme(plot.caption = element_blank(),
          plot.margin = margin(t=12,r=14,l=10,b=10.5),
          
          plot.title = element_text(face='bold',size=20),
          plot.subtitle = element_blank(),
          
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #panel.border = element_rect(size=1),
          panel.background = element_blank(),
          
          axis.line = element_line(size=.5),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_text(face='italic',size=20,margin = margin(t=10,b=5,l=5,r=5)),
          axis.title.y = element_text(face='italic',size=20,margin = margin(t=5,b=5,l=5,r=10)),
          
          #Legend
          legend.position = 'bottom',
          legend.direction = 'horizontal',
          # legend.key.height = unit(2,'line'),
          # legend.key.width=unit(.5,'line'),
          legend.title = element_text(face='italic')
          # legend.box.margin = margin(l=150)
    ) +
    guides(fill=FALSE)
  
  
  return(plot)
  
  
}