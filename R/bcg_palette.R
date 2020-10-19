#' Output ggplot charts to pdf or png files
#' @name bcg_palette
#' @param pal Type of palette to pick, "ee" or "multi" or "traffic" or "bright" or "dark" - defaults to bright
#' @import ggplot2
#' @export
bcg_palette <- function(pal = "bright") {


  if(pal == "ee") {

    ret <- c(
             bcggtheme::bcg_green_bright,
             bcggtheme::bcg_green_mint,
             bcggtheme::bcg_green_jade,
             bcggtheme::bcg_green_forest,
             bcggtheme::bcg_yellow,
             bcggtheme::bcg_blue_true,
             bcggtheme::bcg_red_cranberry,
             bcggtheme::bcg_blue_bright,
             bcggtheme::bcg_yellow_dark
             )

  }

  else if(pal == "multi") {

    ret <- c(bcggtheme::bcg_green_bright,
             bcggtheme::bcg_yellow,
             bcggtheme::bcg_blue_bright,
             bcggtheme::bcg_violet,
             bcggtheme::bcg_red_orange
    )

  }


  else if(pal == "traffic") {

    ret <- c(bcggtheme::bcg_green_bright,
             bcggtheme::bcg_yellow,
             bcggtheme::bcg_red_magenta
    )

  }


  else if(pal == "bright") {

    ret <- c(bcggtheme::bcg_green_bright,
             bcggtheme::bcg_green_jade,
             bcggtheme::bcg_green_bright_2,
             bcggtheme::bcg_green_bright_3,
             bcggtheme::bcg_green_bright_1
    )

  }

  else if(pal == "dark") {

    ret <- c(bcggtheme::bcg_green_bright,
             bcggtheme::bcg_green_bright_4,
             bcggtheme::bcg_green_bright_5,
             bcggtheme::bcg_green_bright_6,
             bcggtheme::bcg_yellow
    )

  }


else {ret <- "pal only takes the values of ee, multi, traffic, bright, or dark"}

  return(ret)

}

