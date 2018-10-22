
# Function to upper & lower limit a line function around a split d via linear interpolation


ribbonDivide <- function(x,y,d) {
  
  shadeBase <- data.frame(x,y) %>%
    arrange(x) %>%
    mutate(lead.y = replace_na(lead(y,1),0),
           crossAxisDesc = ifelse(y>d & lead.y<d,1,0),
           crossAxisAsc = ifelse(y<d & lead.y>d,1,0),
           shadeUpper = ifelse(y<d,d,y),
           shadeLower = ifelse(y>d,d,y))
  
  shadeIntDesc <- shadeBase %>%
    filter(crossAxisDesc==1) %>%
    mutate(g=(lead.y-y),
           int.x=x+((d-y)/g),
           shadeUpper=d,
           shadeLower=d)
  
  shadeIntAsc <- shadeBase %>%
    filter(crossAxisAsc==1) %>%
    mutate(g=(lead.y-y),
           int.x=x+((d-y)/g),
           shadeUpper=d,
           shadeLower=d )
  
  
  shade <- rbind(
    
    shadeBase %>% select(x,shadeUpper,shadeLower),
    shadeIntDesc %>% select(x=int.x,shadeUpper,shadeLower),
    shadeIntAsc %>% select(x=int.x,shadeUpper,shadeLower)
    
  ) %>% arrange(x)
  
  return(shade)
  
}