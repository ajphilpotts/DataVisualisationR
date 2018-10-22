######################################
#------ Region score mapping --------#
######################################

# Prepare Data Frame and Load Map ---------------------------
scoreMap <- function(postcode,
                     scores,
                     pc.ref = read_csv(file = paste(dir.input,"ukpostcodes.csv",sep="/")),
                     map = readRDS(file = paste(dir.input,"ukLocalAuthoritySPDF.rds",sep="/")),
                     with_scaling = FALSE
) {
  
  cat("\n-----|| STEP 1 ||-----\n\nPreparing Dataset and Map...\n\n")
  
  # Prepare data frame
  if (length(postcode)!=length(scores))
    stop("INPUT ERROR: postcode and score arguments do not have same length")
  if (!is.numeric(scores))
    stop("INPUT ERROR: scores should by of type NUMERIC")
  
  pc.prep <-
    pc.ref %>%
    rename(postcode.old=postcode,
           latitude.old=latitude,
           longitude.old=longitude) %>%
    mutate(postcode=gsub(" ","",as.character(postcode.old)), ## Convert POSTCODE to character and remove spaces
           latitude=as.numeric(latitude.old), ## Convert LATITUDE to character and remove spaces
           longitude=as.numeric(longitude.old) ## Convert LONGITUDE to character and remove spaces
    ) %>%
    select(postcode,latitude,longitude)
  
  scores.pc <-
    data.frame(postcode.old=postcode,
               scores=scores) %>%
    mutate(postcode=gsub(" ","",as.character(postcode.old))) %>%
    select(postcode,
           scores)
  
  data <- inner_join(scores.pc,pc.prep,by='postcode')
  
  
  cat("\n-----|| STEP 2 ||-----\n\nCalculating and assigning \nnew data points to polygons...\n\n")
  
  require(sp)
  require(dplyr)
  
  data <- data[!is.na(data$latitude),] # Remove NAs
  
  coordinates(data) <- c("longitude","latitude")  # Coerces to a SpatialPointsDataFrame
  proj4string(data) <- proj4string(map) # Inherit proj4string from Spatial Map
  
  Points <- as(data,"SpatialPoints")   # Coerce SpatialPointsDataFrame to just the SpatialPoints we can match to polygons
  Polygons <- as(map,"SpatialPolygons")   # Pull polygons from shapefile
  
  Polygons <- over(Points,Polygons)  # {sp} function - Match points to polygons
  
  # Calculate what to display as a data point for each polygon
  data_table <- setDT(data@data %>%
                        mutate(poly=Polygons) %>%
                        group_by(poly) %>%
                        summarise(Mean.Score=mean(scores)) %>%
                        ungroup() %>%
                        arrange(Mean.Score) %>%
                        mutate(Mean.Score.Scaled=row_number()/n()) %>%
                        right_join(data.frame(poly=1:length(map)),by='poly') #%>%
                      #mutate(Mean.Score=replace_na(Mean.Score,0))
  )
  
  #freq_table <- data.frame(tabulate(Polygons,nbins=length(map))) ## -- T-19seconds
  #names(freq_table)[1] <- "Counts"
  
  poly.val <- 2
  if (with_scaling) { poly.val <- 3 }
  
  map1<-spChFIDs(map,as.character(1:length(map)))  # Convert feature IDs to characters. Not sure why this is required as of yet
  
  ## Rebuild the Spatial Polygon data frame, this time with calculated frequency as the attached data set
  spdf <- SpatialPolygonsDataFrame(map1,data_table[,..poly.val],match.ID = FALSE)  # Polygons, New Data, prevent {sp} from matching Feature IDs to data
  
  
  
  # Create spatial plot ---------------------------
  
  spdf_map <- spdf[[1]]
  dat <- data_table
  
  cat("\n-----|| STEP 3 ||-----\n\nCreating Map...\n\n")
  
  ## Colour Palette for the chart (blue to red = cold to hot)
  Colour.Ramp <- colorRampPalette(c("#3E273E","#BB1010","#FC2E2E"))
  palette <- Colour.Ramp(nrow(dat))
  
  # Map plot
  modelmap <- 
    spplot(spdf,
           # main="Sky Sports Model Distribution",
           scales=list(draw=F),
           #col.regions=c(color.pad,Colour.Ramp(nrow(score.Polys))),
           col.regions=c("lightgrey",Colour.Ramp(nrow(dat))),
           colorkey=list(space="bottom",axis.line=list(col=NA),length.out = 10),
           #regions=c(pad.colour,ramp.colour),
           #at=c(0,0.01,val.range),
           #cuts=nrow(score.Polys),
           cuts=nrow(dat),
           xlim=c(-10,3.5),
           ylim=c(49,60),
           col="transparent",
           #height=1500,
           #width=900,
           edge.col="white",
           par.settings=list(axis.line=list(col="transparent"),
                             layout.heights=list(key.top=.5,
                                                 key.bottom=.5))
    )
  
  modelmap <- modelmap + layer_(sp.polygons(spdf,fill="lightgrey",col=NA))
  
  return(modelmap)
  
}
