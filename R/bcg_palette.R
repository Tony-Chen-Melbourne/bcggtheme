#' Output ggplot charts to pdf or png files
#' @name bcg_palette
#' @param pal Type of palette to pick, "base" or "traffic" - defaults to base
#' @import ggplot2
#' @export
bcg_palette <- function(pal = "base") {


  if(pal == "base") {

    ret <- c(bcggtheme::bcg_green_mint,
             bcggtheme::bcg_green_jade,
             bcggtheme::bcg_green_forest,
             bcggtheme::bcg_yellow,
             bcggtheme::bcg_blue_true,
             bcggtheme::bcg_red_cranberry
             )

  }

  else if(pal == "traffic") {

    ret <- c(bcggtheme::bcg_green_bright,
             bcggtheme::bcg_yellow,
             bcggtheme::bcg_red_magenta
    )

  }

else {ret <- "pal only takes the values of base or traffic"}

  return(ret)

}

