
xgbExplainerFaceted <- function(dt,title,subtitle=paste(Sys.Date()),caption) {
  
  dt <- dt %>%
    mutate(Impact = ifelse(Action<0,"Negative","Positive"),
           Impact = as.factor(Impact))

  scatter.facet <- ggplot(dt,aes(x=Value,y=Action)) +
    
    stat_identity(yintercept=0, color="grey50", size=.7, geom='hline', inherit.aes=TRUE) +
    
    geom_point(aes(col=Impact),alpha=.1,
               na.rm=TRUE) +
    
    facet_wrap(~Feature,
               ncol=4,nrow=3,scales="free") +
    
    xlab("Feature Value") +
    ylab("Impact on Model") +
    
    labs(title = title,
         subtitle = subtitle,
         caption = caption) +
    
    scale_colour_manual(values=c("#3E273E","#D31111")) +
    scale_y_continuous(expand = expand_scale(mult=c(.2,.2))) +
    scale_x_continuous(expand = expand_scale(mult=c(.2,.2))) +
    
    #Theming
    theme_minimal() + 
    theme(plot.caption = element_text(family = "Arial",hjust = 0,size=6),
          plot.margin = margin(t=12,r=14,l=10,b=10.5),
          
          plot.title = element_text(face='bold',size=12),
          plot.subtitle = element_text(face='italic',size=9),
          
          #axis.line = element_line(size=.5),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_text(face='italic',margin = margin(t=10,b=5,l=5,r=5)),
          axis.title.y = element_text(face='italic',margin = margin(t=5,b=5,l=5,r=10)),
          
          #Legend
          legend.position = 'bottom',
          legend.direction = 'horizontal',
          # legend.key.height = unit(2,'line'),
          # legend.key.width=unit(.5,'line'),
          legend.title = element_text(face='italic'),
          # legend.box.margin = margin(l=150)
          
          #Facet title
          strip.text.x = element_text(size=9,colour = "white",face="bold"),
          strip.background = element_rect(fill="#686666",colour = "transparent")
          
          #Remove grid,
          #panel.grid.major = element_blank(),
          #panel.grid.minor = element_blank()
          
          
    ) +
    guides(color = FALSE)
  
  return(scatter.facet)
  
}


