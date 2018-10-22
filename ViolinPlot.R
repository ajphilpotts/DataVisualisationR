
ViolinPlot <- function(dt) {

# Create Plot
Violin.Plot <- ggplot(dt, aes(x=FeatureDesc, y=Value, fill=PayTV)) +
  
  # Plot Elements
  geom_split_violin(na.rm=TRUE,draw_quantiles = c(.5)) +
  
  # Formatting
  ylab('Standardised Distribution') +
  xlab('Feature') +
  
  labs(title="Comparison of variable distributions",
       subtitle=paste(Sys.Date())) +
  
  # Axes
  scale_y_continuous(breaks=pretty_breaks(n=10)) +
  
  # Colours
  scale_fill_manual(values=c("#E40B03","#A5D2EB")) +
  
  #Theming
  theme_minimal() + 
  theme(plot.caption = element_text(family = "Arial",hjust = 0,size=6),
        plot.margin = margin(t=12,r=14,l=10,b=10.5),
        
        plot.title = element_text(face='bold',size=12),
        plot.subtitle = element_text(face='italic',size=9),
        
        axis.line = element_line(size=.5),
        axis.text.x = element_text(size=11,hjust=1,angle=90),
        #axis.text.y = element_text(size=9),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        #axis.title.x = element_text(face='italic',margin = margin(t=10,b=5,l=5,r=5)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face='italic',margin = margin(t=5,b=5,l=5,r=10)),
        
        #Legend
        #legend.position = 'bottom',
        #legend.position = 'top',
        legend.position = c(1,1),
        legend.justification = c(1,1),
        legend.direction = 'horizontal',
        # legend.key.height = unit(2,'line'),
        # legend.key.width=unit(.5,'line'),
        #legend.title = element_text(face='bold')
        # legend.box.margin = margin(l=150)
        legend.text = element_text(size=10),
        legend.title = element_text(size=10),
        legend.background = element_rect(fill="white",colour = "black",size=.5),
        legend.key.size = unit(1,'line')
  ) +
  guides(fill = guide_legend())

return(Violin.Plot)

}