# Auxiliar functions for visualizer

produceGapminderPlot <- function(indata, 
                                 makeSemiTransparent = FALSE,
                                 x = gdpPerCap,
                                 y = lifeExp){
  x <- ensym(x)
  y <- ensym(y)
  
  continent_colours <-c(Africa = "#BF590CFF", Americas = "#F80039FF", Asia = "#600071FF", 
                        Europe = "#3B9626FF", Oceania = "#4A51E0FF")
  

  axislimits <- list(tfr = c(0.5, 9),
                     lifeExp = c(20, 88),
                     gdpPerCap = c(250, 60000))
  
  axislabels <- list(tfr = "Total Fertility Rate",
                     lifeExp = "Life Expectancy (years)",
                     gdpPerCap = "GDP per capita (ppp dollars)")
  

  
  plotYear <- min(indata$year)
  
  gapminderPlot <- ggplot(data = indata) +
    coord_cartesian(xlim = axislimits[[toString(x)]],
                    ylim = axislimits[[toString(y)]]) 
  

  if(toString(x) == "gdpPerCap"){
    gapminderPlot <- gapminderPlot +
      scale_x_log10(breaks = 4*10**(2:4)) 
  }
  
  if(toString(y) == "gdpPerCap"){
    gapminderPlot <- gapminderPlot +
      scale_y_log10(breaks = 4*10**(2:4)) 
  }
  
  if (nrow(indata) == 0){
    return(gapminderPlot)
  }
  
  
  if (makeSemiTransparent) {
    pointAlpha <- 0.3
  } else {
    pointAlpha <- 1
  }
  
  ## Plot the data
  gapminderPlot <- gapminderPlot +  
    geom_point(aes(x = !!x,
                   y = !!y, 
                   colour = continent,
                   size = population),
               alpha = pointAlpha) + 
    labs(title = plotYear,
         x = axislabels[[toString(x)]],
         y = axislabels[[toString(y)]],
         colour = "Continent") +
    scale_colour_manual(values = continent_colours) +
    scale_alpha_manual() +
    guides(size = FALSE) 
  
  return(gapminderPlot)
  
}


addHistoricPlot <- function(inplot, indata, activeYear = NULL){
  
  if (length(unique(indata$country)) > 1 ) {
    stop("More than one county's data passed")
  }
  
  if (is.null(activeYear)) {
    activeYear = max(indata$year)  
  }  
  
  xdata = getAxis(inplot, "x")
  ydata = getAxis(inplot, "y")
  
  outplot <- inplot + geom_path(data = indata,
                                aes(x = !!xdata,
                                    y = !!ydata,
                                    colour = continent),
                                show.legend = FALSE) +
    geom_point(data = indata[indata$year == activeYear,],
               aes(x = !!xdata,
                   y = !!ydata,
                   colour = continent,
                   size = population),
               show.legend = FALSE)
  
  return(outplot)
  
}

getRichestCountry <- function(indata) {
  
  if (max(indata$year) != min(indata$year) ) {
    warning("More than one year of data passed in")
  }
  
  outdata <- indata %>% 
    arrange(desc(gdpPerCap)) %>% 
    mutate(rn = row_number()) %>% 
    filter(rn == 1) %>% 
    select(country, gdpPerCap)
  
  return(outdata)
  
} 
