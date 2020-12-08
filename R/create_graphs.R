#'Histogram for the exposure statistics
#'@param data The Exposure table by subject
#'@param var_exp Variaable that holds the individuaal exposure
#'@param bins How many bins?
#'@param accuracy Rounding digits in percentages
#'@param digits Round digits in exposure values in x - axis
pdf_exposure <- function(data, 
                         var_exp, 
                         bins, 
                         accuracy = 0.1, 
                         #ref_line = NULL,
                         digits = 3
){
  
  
  range_exp <- range(data[[var_exp]])
  
  binsize   <- diff(range_exp)/bins
  
  breaks    <- seq(from = range_exp[1], to = range_exp[2], by =  binsize)
  
  
  data %>% 
    ggplot(aes(.data[[var_exp]]))+
    geom_histogram(
      
      position = "identity"
      , colour= "grey90"
      , alpha  = 1
      , boundary = 0
      , bins = bins
      , breaks = breaks
      , aes(y= ..count../sum(..count..))
      , fill =  impro_colours[2]
    )+
    stat_bin(
      position = "identity"
      , bins = bins
      , breaks = breaks,geom= "text"
      , aes(label = percent(..count../ sum(..count..),
                            accuracy = accuracy
      ),
      y = ..count../sum(..count..)
      )
      , vjust = -0.5
    )+
    scale_x_continuous(breaks = breaks, labels = round(breaks, digits = digits))+
    scale_y_continuous(labels = percent)+
    theme(
      panel.grid.minor.x = element_blank()
    )
  
}



#'Plot of Cummulative exposure distribution
#'@details Plots either single or multigroup CDF depending if you
#'supply a `var_group`
#'@param data The tbl_exposure() for Individuals
#'@param var_exp  String. The variable name for the exposure values. This depends on the scenario
#'@param var_group   String.Is there a grouping in the data? Usually demographic
#'@param ref_value Numeric. The reference value if any


cdf_exposure <- function(data,
                         var_exp,
                         var_group = NULL,
                         ref_value
                         
){
  
  if(is.null(var_group)){
    p <- data %>% 
      ggplot(aes(.data[[var_exp]]))
    
  } else {
    p <- data %>% 
      ggplot(aes(.data[[var_exp]], colour = .data[[var_group]]))
    
  }
  
  p +
    stat_ecdf(pad = FALSE)+
    scale_y_continuous(labels = scales::percent)+
    geom_vline(xintercept = ref_value, linetype = "dashed" )+
    annotate("text", x = ref_value+0.05*ref_value, y = 0.51,
             hjust = 0, label = label_ref_value(ref_value, width  =  16)
    )
  
}
