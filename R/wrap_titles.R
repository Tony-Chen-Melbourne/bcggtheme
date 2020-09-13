#' Helper function for use in bcg_save and bcg_save_pptx
#' @name wrap_titles
#' @param base_plot The ggplot object to wrap, called internally within save functions
#' @param subtitle Subtitle from the ggplot object to wrap, called internally within save functions
#' @param caption Caption from the ggplot object to wrap, called internally within save functions
#' @param x x-axis label from the ggplot object to wrap, called internally within save functions
#' @param width Width in cm
#' @import ggplot2
#' @import stringr
#' @export


wrap_titles <- function(base_plot,
                        subtitle,
                        caption,
                        x,
                        width) {

  # Function for creating different chart versions for outputting
  # Note: For a 30cm wide slide, 95 characters fit on the subtitle and 130 on the caption
  # Str_wrap widths are calculated off this basis, subtracting 3cm for the LHS white space
  # This function also adds a line-break after the subtitle, and before the x-axis label, replicating ThinkCell spacing

    ret <- base_plot +
      ggplot2::labs(title = NULL,
                    subtitle = subtitle %>%
                    stringr::str_wrap(width = 95*(width-3)/(30-3)) %>%
                      paste0("\n"),
                    caption = caption %>%
                    stringr::str_wrap(width = 130*(width-3)/(30-3)),
                    x = "\n" %>% paste0(x)
                    )
   return(ret)
  }

