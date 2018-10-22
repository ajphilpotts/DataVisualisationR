ggROC_plotROC <- function(dt,title=''){

plot <- ggplot(dt,aes(d=TARGET,m=PREDICTION)) +
  
  geom_abline(slope = 1,intercept = 0,color = "#322332", size=.8) +
  geom_roc(color = "#FF0000",size=1) +

# Formatting
  ylab('True Positive Rate') +
  xlab('False Positive Rate') +
  
  labs(title=title,
       subtitle=paste(Sys.Date())) +
  
  scale_y_continuous(labels = scales::percent,
                     expand = c(0,0),
                     breaks=pretty_breaks(n=20)) +    # Modify xy scales. Moved to (0,0) to remove gap between data and axis
  scale_x_continuous(labels = scales::percent,
                     expand = c(0,0),
                     breaks=pretty_breaks(n=20)) +
  
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
  guides(fill = FALSE)

return(plot)

}

ggROC_pROC <- function(roc,title='',caption=""){
  
  plot <- ggroc(roc,color = "#FF0000",size=1) +
    
    geom_abline(slope = 1,intercept = 1,color = "#322332", size=.8,linetype="dotdash") +
    
    
    # Formatting
    #ylab('True Positive Rate') +
    #xlab('False Positive Rate') +
    
    labs(title=title,
         subtitle = paste(Sys.Date(),sep=" - "),
         caption = caption) +
    
    #scale_y_continuous(breaks=pretty_breaks(n=10)) +    # Modify xy scales. Moved to (0,0) to remove gap between data and axis
    #scale_x_continuous(breaks=pretty_breaks(n=10)) +
    
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
    guides(fill = FALSE)
  
  return(plot)
  
}

