#' Output ggplot charts to pdf or png files
#' @name bcg_save
#' @param filename Name for output file
#' @param type Type of chart to output, either "halfslide" or "fullslide" or "all"
#' @import ggplot2
#' @import stringr
#' @export
bcg_save <- function(...,
                     filename,
                     type) {

  # Last plot
  plot <- ggplot2::last_plot()

  # Getting the text labels
  subtitle <- plot$labels$subtitle
  caption <- plot$labels$caption

  # Creating the right names for outputting first
  dir <- tools::file_path_sans_ext(filename)
  filetype <- tools::file_ext(filename)
  base_name <- tools::file_path_sans_ext(basename(filename))

  # Creating all the file names
  half_slide_name <- stringr::str_c(dir,"/", base_name, "_halfslide.", filetype)
  full_slide_name <- stringr::str_c(dir,"/", base_name, "_fullslide.", filetype)

  # Creating the file directory if it doesn't already exist
  if(!dir.exists(dir)) {

    dir.create(dir, recursive = TRUE)

  }

# Creating different versions of the chart

  # Half slide
  plot_half <- plot + labs(title = NULL,
                           subtitle = subtitle %>% stringr::str_wrap(width = 35),
                           caption = caption %>% stringr::str_wrap(width = 55)
  )

  # Full slide
  plot_full <- plot + labs(title = NULL,
                           subtitle = subtitle %>% stringr::str_wrap(width = 90),
                           caption = caption %>% stringr::str_wrap(width = 150)
  )

  # Setting up halfslide, fullslide, and all versions
  ret <- if(type == "halfslide") {

    ggplot2::ggsave(...,
                    file=half_slide_name,
                    plot = plot_half,
                    width=12, height=14,
                    units = "cm",
                    dpi = "retina",
                    type = "cairo")

  }

  else if (type == "fullslide") {

  ggplot2::ggsave(...,
                    file=full_slide_name,
                    plot = plot_full,
                    width=28, height=14,
                    units = "cm",
                    dpi = "retina",
                  type = "cairo")

  }

  else if (type == "all") {


# Half slides
    ggplot2::ggsave(...,
                    file=half_slide_name,
                    plot = plot_half,
                    width=12, height=14,
                    units = "cm",
                    dpi = "retina",
                    type = "cairo")

# Full slides
  ggplot2::ggsave(...,
                  file=full_slide_name,
                  plot = plot_full,
                  width=28, height=14,
                  units = "cm",
                  dpi = "retina",
                  type = "cairo")


  }


  else {"type only takes values of halfslide, fullslide, or all"}


  return(ret)

}

