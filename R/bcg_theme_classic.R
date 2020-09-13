#' Create a ggplot2 theme in the style of classic, old-school BCG charts
#' @name bcg_theme_classic
#' @param base_size Size for text elements. Defaults to 16
#' @param legend "bottom" by default.
#' @param flipped "FALSE" by default
#' @param background "white" by default
#' @import ggplot2
#' @import extrafont
#' @export

bcg_theme_classic <- function(base_size = 16,
                              background = "white",
                              legend = "bottom",
                              flipped = FALSE) {

  extrafont::loadfonts(device = "win", quiet = TRUE)

  ret <- ggplot2::theme_classic() +
    ggplot2::theme(text = element_text(family="Trebuchet MS",
                              size = base_size,
                              colour = bcggtheme::bcg_grey_text
                              ),
          axis.text.x = element_text(size = base_size),
          axis.title.x = element_text(hjust = 1),
          axis.text.y = element_text(size = base_size),
          axis.line = element_line(color = bcggtheme::bcg_grey_axis),
          axis.ticks = element_line(colour = bcggtheme::bcg_grey_axis),
          axis.ticks.length=unit(.2, "cm"),
          plot.title = element_text(size=(base_size + 2)),
          plot.subtitle = element_text(hjust = 0,
                                       size=base_size),
          plot.caption = element_text(hjust = 0,
                                      size = base_size - 6),
          plot.title.position = "plot"
    ) +
    ggplot2::theme(legend.position = legend,
          legend.title = element_blank(),
          legend.text = element_text(size = base_size - 4)) +
    ggplot2::theme(strip.background = element_blank())

  # Account for flipped

  if(flipped == TRUE) {

ret <- ret +
  ggplot2::theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
        )
  }

  if(flipped == FALSE) {

    ret <- ret
  }

  # Accounting for background
  if(background == "white") {

    ret <- ret
  }

  if(background == "grey") {
    ret <- ret +
      ggplot2::theme(
        plot.background = element_rect(fill = bcggtheme::bcg_grey_background),
        panel.background = element_rect(fill = bcggtheme::bcg_grey_background),
        legend.background = element_rect(fill = bcg_grey_background)
      )
  }

# Calling an internal function to update the geoms
update_bcg_geoms()

return(ret)

}
