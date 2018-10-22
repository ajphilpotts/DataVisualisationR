
AUCPlot <- function(plot.dt,c.time) {

plot <-
  ggplot(plot.dt) +  # Initialize ggplot2. Specify date set, X and Y columns
  
  # Plot Elements
  geom_step(aes(x=Iteration,y=AUC,colour=Dataset,group=Dataset),size=1,stat='identity') +
  
  # Formatting
  ylab('(Mean) AUC') +
  xlab('XGBoost Iteration') +
  
  labs(title="Pay TV Model AUC",
       subtitle=paste(c.time)) +

  scale_x_continuous(expand=c(0,0),breaks=pretty_breaks(n=20)) +
  scale_y_continuous(labels = function(x){sprintf("%.3f", x)},
                     expand = c(0,0),
                     breaks=pretty_breaks(n=20)
                     ) +    # Modify xy scales. Moved to (0,0) to remove gap between data and axis
  #expand_limits(y=0.5) +
  
  scale_colour_manual(values=c("#FF0000","#5B395B")) +
  #scale_color_tableau() +
  
  #Theming
  theme_minimal() + 
  theme(plot.caption = element_text(family = "Arial",hjust = 0,size=6),
        plot.margin = margin(t=12,r=14,l=10,b=10.5),
        
        plot.title = element_text(face='bold',size=12),
        plot.subtitle = element_text(face='italic',size=9),
        
        axis.line = element_line(size=.5),
        axis.text.x = element_text(size=7),
        axis.ticks = element_blank(),
        axis.title.x = element_text(face='italic',margin = margin(t=10,b=5,l=5,r=5)),
        axis.title.y = element_text(face='italic',margin = margin(t=5,b=5,l=5,r=10)),
        
        #Legend
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        # legend.key.height = unit(2,'line'),
        # legend.key.width=unit(.5,'line'),
        legend.title = element_blank()
        # legend.box.margin = margin(l=150)
  )

# gt <- ggplot_gtable(ggplot_build(plot))
# gt$layout$clip[gt$layout$name == "panel"] <- "off"

return(plot)

}