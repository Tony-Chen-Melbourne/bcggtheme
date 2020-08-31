#' Easily colour your ggplot data using the BCG palette, up to 6 colours
#' @name bcg_colour_manual
#' @import ggplot2
#' @export

bcg_colour_manual <- function(...) {


  ret <- ggplot2::scale_colour_manual(...,
                                      values =
                                        c(bcggtheme::bcg_green_1,
                                          bcggtheme::bcg_green_2,
                                          bcggtheme::bcg_green_3,
                                          bcggtheme::bcg_green_4,
                                          bcggtheme::bcg_yellow,
                                          bcggtheme::bcg_grey)
  )



return(ret)

}
