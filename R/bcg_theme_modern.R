#' Create a ggplot2 theme in the style of modern, new school BCG charts
#' @name bcg_theme_modern
#' @param base_size Size for text elements. Defaults to 18
#' @param base_family Font family for text elements. Defaults to "sans",
#'   indistinguishable from Arial.
#' @param legend "bottom" by default.
#' @param y_axis FALSE by default. Set TRUE to add a y axis
#' @param flipped FALSE by default.
#' @param background "white" by default.
#' @import ggplot2
#' @import extrafont
#' @export

bcg_theme_modern <- function(base_size = 16,
                             background = "white",
                             legend = "bottom",
                             y_axis = FALSE,
                             flipped = FALSE) {

  extrafont::loadfonts(device = "win", quiet = TRUE)

  ret <- if(y_axis == FALSE) {

ggplot2::theme_classic() +
    theme(text = element_text(family="Trebuchet MS",
                              size = base_size,
                              colour = bcggtheme::bcg_grey_text),
          axis.text.x = element_text(size = base_size),
          axis.title.x = element_text(hjust = 1),
          axis.line = element_line(color = bcggtheme::bcg_grey_axis),
          axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(size=(base_size+2)),
          plot.subtitle = element_text(hjust = 0,
                                       size=base_size),
          plot.caption = element_text(hjust = 0,
                                      size = (base_size-6)),
          plot.title.position = "plot"
    ) +
    theme(legend.position = legend,
          legend.title = element_blank(),
          legend.text = element_text(size = (base_size-4))
    ) +
    theme(strip.background = element_blank())

}

else if (y_axis == TRUE) {

  ggplot2::theme_classic() +
    theme(text = element_text(family= "Trebuchet MS",
                              size = base_size,
                              colour = bcggtheme::bcg_grey_text),
          axis.text.x = element_text(size = base_size),
          axis.title.x = element_text(hjust = 1),
          axis.line = element_line(color = bcggtheme::bcg_grey_axis),
          #axis.line.y = element_blank(),
          axis.text.y = element_text(size = base_size),
          axis.ticks = element_blank(),
          plot.title = element_text(size=(base_size+2)),
          plot.subtitle = element_text(hjust = 0,
                                       size=base_size),
          plot.caption = element_text(hjust = 0),
          plot.title.position = "plot"
    ) +
    theme(legend.position = legend,
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
