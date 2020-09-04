#' Create a ggplot2 theme in the style of classic, old-school BCG charts
#' @name bcg_theme_classic
#' @param base_size Size for text elements. Defaults to 16
#' @param legend "bottom" by default.
#' @param flipped "FALSE" by default
#' @import ggplot2
#' @import extrafont
#' @export

bcg_theme_classic <- function(base_size = 16,
                               legend = "bottom",
                               flipped = FALSE) {

  extrafont::loadfonts(device = "win", quiet = TRUE)

  ret <- ggplot2::theme_classic() +
    ggplot2::theme(text = element_text(family="Trebuchet MS",
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
    ggplot2::theme(legend.position = legend,
          legend.title = element_blank(),
          legend.text = element_text(size = base_size - 4)) +
    ggplot2::theme(strip.background = element_blank())


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
