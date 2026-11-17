ggpie <- function(x, 
                  title = NULL, 
                  text_color = 'white',
                  text_position = 1.1,
                  text_label = c('identity','percent','both'),
                  legend.position = 'none'
                  ) {
  
  #' This function is a ggplot wrapper that takes a vector and returns
  #' a pie chart using ggplot2. 
  #' 
  #' @param x A vector
  #' @param title A title to add to the finished plot
  #' @param text_color Color of label text
  #' @param text_position Position of label text, interpreted as distance from center (0)
  #' @param text_label Label content
  #' @param legend.position Passed to ggplot
  
  if ( !(require(dplyr) & require(ggplot2)) ) 
    stop("This function requires the ggplot2 and dplyr packages")
  
  text_label = match.arg(text_label)  
  
  data = 
      as.data.frame(x) %>% 
      dplyr::select(1) %>% # Drop additional columns if passed
      setNames('categories') %>% 
      dplyr::count(categories) %>% # Count items in passed vector
      dplyr::mutate(
        prop = proportions(n),
        ypos = 1 - cumsum(prop) + prop/2,
        angle = 90 - (360 * ypos),
        angle = ifelse(ypos > 0.5, angle + 180, angle)
      ) 
    # rowwise() %>% 
    # mutate(
    #     label = ifelse(text_label=='identity', categories,
    #                ifelse(text_label=='percent', prop*100,
    #                   ifelse(text_label=='both', 
    #                          paste0(categories, ' - ', round(prop*100,1), '%'),
    #                          NA)))
    #   )
  
  if(text_label=='identity') data = data %>% mutate(label = categories)
  if(text_label=='percent') data = data %>% mutate(label = prop*100)
  if(text_label=='both') data = data %>% mutate(label = paste0(categories, ' - ', round(prop*100,1), '%'))

  
    suppressWarnings(
      ggplot2::ggplot(data, ggplot2::aes(x='x', y=prop, fill=categories)) +
        ggplot2::geom_bar(stat = 'identity') +
        ggplot2::geom_text(
          ggplot2::aes(x=text_position, y=ypos, label=label, angle=angle), 
          color = text_color
          ) +
        ggplot2::coord_polar('y') +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = legend.position) +
        ggplot2::labs(title = title)
    )
}
