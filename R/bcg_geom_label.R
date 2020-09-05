#' Add data labels in a grey textbox style
#' @name bcg_geom_label
#' @import ggplot2
#' @export

bcg_geom_label <- function(...) {


  ret <- ggplot2::geom_label(...,
                             fill = bcggtheme::bcg_grey_soft,
                             colour = "white",
                             label.padding = unit(0.2, "cm"),
                             label.r = unit(0,"cm")
                                     )

return(ret)

}
