#' Create a ggplot2 theme in the style of classic, old-school BCG charts
#' @name bcg_theme_classic
#' @param base_size Size for text elements. Defaults to 18
#' @param base_family Font family for text elements. Defaults to "sans",
#'   indistinguishable from Arial.
#' @param legend "bottom" by default.
#' @import ggplot2
#' @export

bcg_theme_classic <- function(base_size = 16,
                               base_family = "sans",
                               legend = "bottom") {


  ret <- ggplot2::theme_classic() +
    theme(text = element_text(family= base_family,
                              size = base_size),
          axis.text.x = element_text(colour = "black", size = base_size),
          axis.text.y = element_text(colour = "black", size = base_size),
          axis.line = element_line(color = bcggtheme::bcg_grey),
          axis.ticks = element_line(colour = bcggtheme::bcg_grey),
          axis.ticks.length=unit(.2, "cm"),
          plot.title = element_text(size=(base_size + 2)),
          plot.subtitle = element_text(size=base_size),
          plot.caption = element_text(hjust = 0,
                                      size = base_size - 6)
    ) +
    theme(legend.position = legend,
          legend.title = element_blank(),
          legend.text = element_text(size = base_size - 4)) +
    theme(strip.background = element_blank())

# Calling an internal function to update the geoms
update_bcg_geoms()

return(ret)

}
