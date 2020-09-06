#' Easily colour your ggplot data using the BCG palette
#' @name bcg_colour_manual
#' @param pal Choose the colour palette, either "base" or "traffic2" or "traffic3". Defaults to "base".
#' @param reverse Flip the order of colours in the palette. TRUE or FALSE. Defaults to FALSE.
#' @import ggplot2
#' @export

bcg_colour_manual <- function(...,
                              pal = "base",
                              reverse = FALSE) {

palette <- bcggtheme::bcg_palette(pal = pal)

if(reverse == TRUE) {
  palette <- palette %>% rev()

}

if(reverse == FALSE) {

palette <- palette

}

  ret <- ggplot2::scale_colour_manual(...,
                                      values =
                                        palette
  )



return(ret)

}
