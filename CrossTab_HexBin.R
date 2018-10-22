
# Hexagonal bin density plot


CrossTab_HexBin <- function(x,y,samples=10000,rescale=TRUE,
                            title="",subtitle=Sys.Date(),caption="",xlabel="x",ylabel="y") {
  
  #Create table for manipulation
  dt <- data.table(x,y)
  
  #Downsample for clarity
  samplerows <- sample(1:nrow(dt),samples)
  dt <- dt[samplerows,]
  
  dt <- dt %>%
    arrange(x) %>% mutate(x.scaled=row_number()/nrow(dt)) %>%
    arrange(y) %>% mutate(y.scaled=row_number()/nrow(dt))
  
  if (rescale) {
    dt <- select(dt,x=x.scaled,y=y.scaled)
  }
  
  # Create colour ramp
  Colour.Ramp <- colorRampPalette(c("#3E273E","#BB1010","#FC2E2E"))
  
  # Palettes
  disc_colours <- Colour.Ramp(11)
  #disc_colours <- c("#3E273E",rev(brewer.pal(8,"Spectral")),"#BB1010","#FC2E2E")
  #disc_colours <- rev(brewer.pal(8,"Spectral"))
    
  plot <- ggplot(dt,aes(x=x,y=y)) +
    
    # Hexagonal bins
    stat_bin_hex(bins=c(60,40)) +
    
    # Colour scales
    scale_fill_gradientn(colours=disc_colours,trans="sqrt",
                         limits=c(4,NA),
                         na.value=rgb(0, 0, 0, alpha=0.2)
                         ) +
    scale_x_continuous(breaks=pretty_breaks(10)) +
    scale_y_continuous(breaks=pretty_breaks(10)) +
    
    # Plot labels
    labs(title=title,
         subtitle=subtitle,
         caption=caption,
         x=xlabel,
         y=ylabel,
         fill="Frequency") +
    
    #Theming
    theme_minimal() +
    
    theme(
      #Plot Area
      plot.caption = element_text(family = "Arial",hjust = 0,size=6,colour="grey50"),
      plot.margin = margin(t=12,r=14,l=10,b=10.5),
      plot.title = element_text(face='bold',size=12,hjust=0),
      plot.subtitle = element_text(face='italic',size=9,hjust=0),
      
      #Axes
      axis.line = element_line(size=.5),
      axis.text.x = element_text(size=9),
      axis.text.y = element_text(size=9),
      axis.ticks = element_blank(),
      axis.title.x = element_text(face='italic',margin = margin(t=10,b=5,l=5,r=5)),
      axis.title.y = element_text(face='italic',margin = margin(t=5,b=5,l=5,r=10),angle=90),
      
      
      #Legend
      #legend.position = 'bottom',
      #legend.direction = 'horizontal',
      legend.title = element_text(face='italic',size=9)
      # legend.box.margin = margin(l=150)
    )
  
  return(plot)
    
}
