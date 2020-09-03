#' Create a ggplot2 theme in the style of modern, new school BCG charts
#' @name bcg_theme_modern
#' @param base_size Size for text elements. Defaults to 18
#' @param base_family Font family for text elements. Defaults to "sans",
#'   indistinguishable from Arial.
#' @param legend "bottom" by default.
#' @param y_axis FALSE by default. Set TRUE to add a y axis
#' @param flipped FALSE by default.
#' @import ggplot2
#' @export

bcg_theme_modern <- function(base_size = 16,
                               base_family = "sans",
                               legend = "bottom",
                               y_axis = FALSE,
                               flipped = FALSE) {

ret <- if(y_axis == FALSE) {

ggplot2::theme_classic() +
    theme(text = element_text(family= base_family,
                              size = base_size),
          axis.text.x = element_text(colour = "black", size = base_size),
          axis.line = element_line(color = bcggtheme::bcg_grey),
          axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(size=(base_size+2)),
          plot.subtitle = element_text(size=base_size),
          plot.caption = element_text(hjust = 0,
                                      size = (base_size-6))
    ) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = (base_size-4))
    ) +
    theme(strip.background = element_blank())

}

else if (y_axis == TRUE) {

  ggplot2::theme_classic() +
    theme(text = element_text(family= base_family,
                              size = base_size),
          axis.text.x = element_text(colour = "black", size = base_size),
          axis.line = element_line(color = bcggtheme::bcg_grey),
          #axis.line.y = element_blank(),
          axis.text.y = element_text(colour = "black", size = base_size),
          axis.ticks = element_blank(),
          plot.title = element_text(size=(base_size+2)),
          plot.subtitle = element_text(size=base_size),
          plot.caption = element_text(hjust = 0)
    ) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = (base_size-4))
    )

}

else {"y_axis only takes the values TRUE and FALSE"}

# Now accounting for the flipped function
if(flipped == TRUE) {

  ret <- ret +
    theme(axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
    )
}

if(flipped == FALSE) {

  ret <- ret
}


# Calling an internal function to update the geoms
update_bcg_geoms()

return(ret)

}
