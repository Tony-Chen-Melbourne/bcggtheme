#' Reduce the bounds of the y-axis, in BCG style
#' @name bcg_scale_y_continuous
#' @import ggplot2
#' @export

bcg_scale_y_continuous <- function(...) {


  ret <- ggplot2::scale_y_continuous(...,
                                     expand = expansion(mult = c(0,0))
                                     )

return(ret)

}
