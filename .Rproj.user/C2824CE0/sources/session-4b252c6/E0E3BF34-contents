plattice <- function(x, rows, pcol = "blue", psize = 2, lcol = "red", lsize = 1,
                     legcol = "orange", xlabel = NULL, ylabel = NULL, y_labels = NULL) {

  ## Lattice graph with ggplot2
  colnames(x) <- c( "country", "year", "y", "y_hat")
  year <- unique(x[, 2])
  len <- length(year)
  y <- x[, 3]   ;   y_hat <- x[, 4]
  x_labels <- rep( c("", year, ""), each = len )
  if ( is.null(y_labels) )  y_labels <- round( seq( min(x[, 3:4]), max(x[, 3:4]), length = 10 ), 3)
  # Create ggplot object
  ggplot() +
    # Add points
    geom_point(data = x, aes( x = year, y = y ), color = "blue", size = psize) +
    # Add lines
    geom_line( data = x, aes( x = year, y = y_hat ), color = "red", size = lsize) +

    facet_wrap( ~country, nrow = rows ) +

    theme( panel.spacing.x = unit( .5, "lines" ),  ## change the 0.5 to bring the boxes closer
           strip.background = element_rect( fill = "orange" ),
           strip.text = element_text( colour = "black" ),
           axis.text.x = element_text( angle = 45, vjust = 1, hjust = 1 ),
           text = element_text( face = "bold" ),
           axis.text = element_text( color = "black" )
    ) +

    scale_x_continuous(xlabel, labels = x_labels, breaks = as.numeric(x_labels) ) +
    scale_y_continuous(ylabel, labels = as.character(y_labels), breaks = y_labels)
}
