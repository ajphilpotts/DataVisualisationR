
###### Feature Importance Map #######

featImportanceMap <- function(dt) {
  
  dt <- dt  %>%
    mutate(Feature = str_replace_all(Feature,'_',' ')) %>%
    arrange(Source)
  setDT(dt)
  
  # Base palette
  base.palette <- c("#D31111","#3E273E","#3EA5A5","#A58053","#3E5641")
  # Add grey for "OTHER" vars
  plot.palette <- c(base.palette[1:nrow(dt[,.N,by=Source])-1],"#81867E")
  
  Colors <- data.frame(Color=plot.palette) %>% mutate(ColorRank=row_number())
  
  SourceColour <- dt %>% group_by(Source) %>%
    summarise(Gain=sum(Gain)) %>% ungroup() %>%
    mutate(ColorRank = dense_rank(-Gain)) %>%
    select(Source,ColorRank) %>%
    left_join(Colors,by='ColorRank') %>%
    select(Source,Color)
  
  dt <- dt %>%
    left_join(SourceColour,by='Source')
  
  col <- as.character(dt$Color)
  names(col) <- as.character(dt$Source)
    
  
  # Create Plot ----------------------------
  
  plot <- ggplot(dt,aes(area = Gain, fill=Source, subgroup=Source)) +
    
    # Feature Tier
    geom_treemap(size=1,
                 colour="white") +
    
    geom_treemap_text(aes(label=Feature),
                      place = "topleft",
                      grow = TRUE,
                      reflow = TRUE,
                      color="lightgrey"
    ) +
    
    geom_treemap_subgroup_border(size=3,
                                 colour="white") +
    
    #geom_treemap_subgroup_text
    
    # Source Tier
    
    
    # Fill colours
    #scale_fill_continuous(high="#D31111",low="#3E273E") +
    scale_fill_manual(values=col) +
    
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
    guides(fill = guide_legend(nrow=1))
  
  return(plot)
  
}