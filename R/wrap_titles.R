#' Helper function for use in bcg_save and bcg_save_pptx
#' @name wrap_titles
#' @param base_plot The ggplot object to wrap, called internally within save functions
#' @param subtitle Subtitle from the ggplot object to wrap, called internally within save functions
#' @param caption Caption from the ggplot object to wrap, called internally within save functions
#' @param width Width in cm
#' @import ggplot2
#' @import stringr
#' @export


wrap_titles <- function(base_plot,
                        title,
                        subtitle,
                        caption,
                        width) {

  # Function for creating different chart versions for outputting
  # Note: For a 30cm wide slide, 95 characters fit on the subtitle and 130 on the caption
  # Str_wrap widths are calculated off this basis, subtracting 3cm for the LHS white space

    ret <- base_plot +
      ggplot2::labs(title = NULL,
                    subtitle = subtitle %>%
                    stringr::str_wrap(width = 95*(width-3)/(30-3)),
                    caption = caption %>%
                    stringr::str_wrap(width = 130*(width-3)/(30-3))
                    )
   return(ret)
  }

