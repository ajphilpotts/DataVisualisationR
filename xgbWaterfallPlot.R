#-------------------------------------------#
# REPLICATED FROM XGBOOST EXPLAINER PACKAGE #
#-------------------------------------------#


xgbWaterfallData <- function(xgb.model,
                             explainer,
                             DMatrix,
                             data.matrix, 
                             idx, 
                             type = "binary",
                             threshold = 0.0001,
                             label_translate = FALSE,
                             label_names = NULL,
                             fixed_features = NULL){
  
  # Predictions for the given ID
  breakdown = explainPredictions(xgb.model, explainer, xgboost::slice(DMatrix,as.integer(idx)))
  
  # Type is going to  be binary, so logistic regression. Calc probabilties from log-loss regression
  weight = rowSums(breakdown)
  if (type == 'regression'){
    pred = weight
  }else{
    pred = 1/(1+exp(-weight))
  }
  
  # replace labels with new labels from label_names
  if(label_translate) {
    names(breakdown) <- left_join(data.table(label=as.character(names(breakdown))),
                                          label_names,
                                          by='label')$new_label
  }
  
  breakdown_summary = as.matrix(breakdown)[1,]

  data_for_label = data.matrix[idx,]
  
  i = order(abs(breakdown_summary),decreasing=TRUE)
  
  breakdown_summary = breakdown_summary[i]
  data_for_label = data_for_label[i]
  
  intercept = breakdown_summary[names(breakdown_summary)=='intercept']
  data_for_label = data_for_label[names(breakdown_summary)!='intercept']
  breakdown_summary = breakdown_summary[names(breakdown_summary)!='intercept']
  
  # Replace vals below threshold with other
  if (!is.null(fixed_features)) {
    i_other = seq(fixed_features+1,length(breakdown_summary))
  } else {
  i_other = which(abs(breakdown_summary)<threshold)
  }
  other_impact = 0
  
  if (length(i_other > 0)){
    other_impact = sum(breakdown_summary[i_other]) #val of "other" impact
    names(other_impact) = 'other'
    breakdown_summary = breakdown_summary[-i_other] #remove below threshold vals
    data_for_label = round(data_for_label[-i_other],2)
  }
  
  # Create data labels (variables with impact), might get rid of this
  if (abs(other_impact) > 0){
    breakdown_summary = c(intercept, breakdown_summary, other_impact)
    data_for_label = c("", data_for_label,"")
    labels = paste0(names(breakdown_summary)," = ", data_for_label) #Remove vals for stakeholders
    #labels = paste0(names(breakdown_summary))
    labels[1] = 'intercept'
    labels[length(labels)] = 'other'
  }else{
    breakdown_summary = c(intercept, breakdown_summary)
    data_for_label = c("", data_for_label)
    labels = paste0(names(breakdown_summary)," = ", data_for_label)
    #labels = paste0(names(breakdown_summary))
    labels[1] = 'intercept'
  }
  
  actual=NA
  
  if (!is.null(xgboost::getinfo(DMatrix,"label"))){ # Retrieve target from DMatrix
    actual=xgboost::getinfo(xgboost::slice(DMatrix,as.integer(idx)),"label")
    cat("\nActual: ", actual) # Print targ
  }
  cat("\nPrediction: ", pred)
  cat("\nWeight: ", weight)
  cat("\nBreakdown")
  cat('\n')
  print(breakdown_summary)
  
  return(list("breakdown_summary"=breakdown_summary,
              "labels"=labels,
              "weight"=weight,
              "prediction"=pred,
              "actual"=actual
  ))
  
}






# just classification for now

xgbWaterfallPlot = function(breakdown_summary,
                            labels,
                            weight,
                            prediction,
                            limits = c(NA, NA),
                            title,
                            subtitle = paste(Sys.Date(),sep=" - "),
                            caption) {
  
  inverse_logit_trans <- scales::trans_new("inverse logit",transform = plogis,inverse = qlogis)
  
  inverse_logit = function(x){return (1/(1+exp(-x)))} # True probabilities from logit
  inverse_logit_labels_perc = function(x){return (scales::percent(inverse_logit(x),accuracy=1))}
  logit = function(x){return(log(x/(1-x)))} # Logit from probs
  
  # Build cumulative %
  cumBreakdown = cumsum(breakdown_summary)
  cumPerc = inverse_logit(cumBreakdown)
  percBreakdown = cumPerc-replace_na(lag(cumPerc),.5)
  
  # Build colour palette
  signs = breakdown_summary < 0
  colours = replace_na(ifelse(signs,"#3E273E","#D31111"),"#D31111")
  
  ybreaks<-logit(seq(2,98,2)/100)
  #ybreaks<-seq(.02,.98,.02)
  
  ### GGPLOT Section
  
  # Plot init
  #waterfalls::waterfall(values = breakdown_summary,
  waterfall_intercept(values = breakdown_summary,
                        #rect_text_labels = round(breakdown_summary, 2),
                        rect_text_labels = c("",scales::percent(percBreakdown[-1],accuracy=.1)),
                        labels = c("",labels[-1]),
                        #total_rect_text = round(weight, 2),
                        total_rect_text = scales::percent(prediction,accuracy=.1),
                      rect_text_size = 1.3,
                      rect_text_fontface="plain",
                        calc_total = TRUE,
                        total_rect_color = "#A58053",
                      total_rect_text_color = "white",
                        draw_lines = TRUE,
                        fill_colours = colours,
                        fill_by_sign = FALSE,
                      #put_rect_text_outside_when_value_below=.07,
                        linetype="solid",
                        lines_anchors = c("left","right"),
                        rect_border = NA,
                        rect_width=.8,
                        total_axis_text = "Prediction",
                        print_plot = FALSE,
                      intercept = breakdown_summary[1],
                      intercept_colour = "grey50",
                      alpha=1,
                      text_inside_rect_col = "white",
                      text_outside_rect_col = "black")  +
    
    annotate("text",x=1,y=breakdown_summary[1],
             #label=paste("  Base Score =",scales::percent(percBreakdown[1],accuracy=1),"  "),
             label=paste("  Base Score =",inverse_logit_labels_perc(breakdown_summary[1]),"  "),
             angle=90,fontface="italic",hjust=ifelse(cumBreakdown[length(cumBreakdown)]>0,0,1),
             size=4) +
    
    # Formatting
    ylab("Probability") +
    xlab("") +
    
    labs(title = title,
         subtitle = subtitle,
         caption = caption) +
    
    
    scale_y_continuous(labels = inverse_logit_labels_perc,
                       breaks = ybreaks, limits = limits) +
    
    scale_fill_manual(values=c("#D31111","#3E273E")) +
    
    #theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    #Theming
    
    theme_minimal() + 
    theme(plot.caption = element_text(family = "Arial",hjust = 0,size=6),
          plot.margin = margin(t=12,r=14,l=10,b=10.5),
          
          plot.title = element_text(face='bold',size=12),
          plot.subtitle = element_text(family="Courier",size=9),
          
          axis.line = element_line(size=.5),
          axis.text.x = element_text(size=9, angle = 45, hjust = 1),
          axis.ticks = element_blank(),
          #axis.title.x = element_text(face='italic',margin = margin(t=10,b=5,l=5,r=5)),
          axis.title.x=element_blank(),
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
  
  
  
}
