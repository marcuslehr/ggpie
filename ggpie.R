ggpie <- function(x, title = NULL) {
  
  #' This function is a ggplot wrapper that takes a vector and creates
  #' a pie chart
  #' 
  #' @param x A vector
  #' @param title A title to add to the finished plot
  
  if ( !(require(dplyr) & require(ggplot2)) ) 
    stop("This function requires the ggplot2 and dplyr packages")
  
  # Make column counts and other data for plots
    counts = as.data.frame(x) %>% dplyr::select(1) %>% # Drop additional columns if passed
      dplyr::group_by(!!dplyr::sym(names(.))) %>% # Col must be grouped to count properly
      dplyr::summarise(count = dplyr::n()) %>% # Count items in col
      dplyr::mutate(prop = (count/sum(count))*100, # Create data for labels
                    ypos = 100 - cumsum(prop) + prop/2,
                    angle = (cumsum(prop)-prop/2)*3.6-90) %>% 
      as.data.frame()
  
  # Create plots
    suppressWarnings(
      ggplot2::ggplot(counts, ggplot2::aes(x='x', y=prop, fill=counts[[1]])) +
        ggplot2::geom_bar(width = 1, stat = 'identity', color = 'white') +
        ggplot2::geom_text(ggplot2::aes(x = 1.1, y = ypos, label = counts[[1]]), 
                           color = 'white', angle = counts$angle) +
        ggplot2::coord_polar('y') +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = 'none') +
        ggplot2::labs(title = title)
    )
}
