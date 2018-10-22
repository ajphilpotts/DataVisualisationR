
###### Postcode propensity plot #######

areaPropensityPlot <- function(dt,title,limits,fill,direction) {
  
  d=1
  if(direction=="asc") d=-1
  
  setDT(dt)
  
  plot <- ggplot(dt,aes(x=reorder(POSTCODE_AREA,d*Mean.Score),y=Mean.Score)) +  # Initialize ggplot2. Specify date set, X and Y columns
    
    # Plot Elements
    geom_bar(stat='identity',position = "dodge",fill=fill,width=.8) +
    geom_text(aes(label=paste0("  ",POSTCODE_NAME)),
              fontface="italic",hjust=0,col="grey30",size=5) +
    coord_flip() +
    
    # Formatting
    ylab('') +
    xlab('') +
    
    labs(title=title) +
    
    scale_y_continuous(labels = scales::percent,
                       expand = expand_scale(mult=c(0,0)),
                       limits = limits,
                       oob=rescale_none,
                       breaks=pretty_breaks(n=10),
                       position="right"
    ) +    # Modify xy scales. Moved to (0,0) to remove gap between data and axis
    
    # Colours
    #scale_fill_manual(values=plot.palette) +
    
    #Theming
    
    theme_minimal() + 
    theme(plot.caption = element_text(family = "Arial",hjust = 0,size=6),
          plot.margin = margin(t=8,r=14,l=10,b=10.5),
          
          panel.grid = element_blank(),
          
          plot.title = element_text(face='bold',size=16),
          plot.subtitle = element_text(face='italic',size=9),
          
          axis.line = element_line(size=.5),
          axis.text.x = element_text(size=11),
          axis.text.y = element_text(size=11),
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


