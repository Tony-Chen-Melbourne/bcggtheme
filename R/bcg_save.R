#' Output ggplot charts to pdf or png files
#' @name bcg_save
#' @param filename Name for output file
#' @param type Type of chart to output, either "third", "half", "two third", "large", "full", or "all"
#' @import ggplot2
#' @import stringr
#' @export
bcg_save <- function(...,
                     filename,
                     type) {

  # Last plot
  base_plot <- ggplot2::last_plot()

  # Getting the text labels
  subtitle <- base_plot$labels$subtitle
  caption <- base_plot$labels$caption

  # Creating the right names for outputting first
  dir <- tools::file_path_sans_ext(filename)
  filetype <- tools::file_ext(filename)
  base_name <- tools::file_path_sans_ext(basename(filename))

  # Creating all the file names
  third_slide_name <- stringr::str_c(dir,"/", base_name, "_third.", filetype)
  half_slide_name <- stringr::str_c(dir,"/", base_name, "_half.", filetype)
  two_third_slide_name <- stringr::str_c(dir,"/", base_name, "_two_third.", filetype)
  large_slide_name <- stringr::str_c(dir,"/", base_name, "_large.", filetype)
  full_slide_name <- stringr::str_c(dir,"/", base_name, "_full.", filetype)

  # Creating the file directory if it doesn't already exist
  if(!dir.exists(dir)) {

    dir.create(dir, recursive = TRUE)

  }

  # There are five types of output size:
  # One Third size - 11 cm wide
  # Half size - 14cm wide
  # Two third size - 18cm wide
  # Large size - 24cm wide
  # Full size - 30cm wide

    # Setting up halfslide, fullslide, and all versions
  ret <- if(type == "third") {

    plot_third <- bcggtheme::wrap_titles(base_plot = base_plot,
                                         subtitle = subtitle,
                                         caption = caption,
                                         width = 11)

    ggplot2::ggsave(...,
                    file=third_slide_name,
                    plot = plot_third,
                    width=11, height=14,
                    units = "cm",
                    dpi = "retina",
                    type = "cairo")

  }

  else if(type == "half") {

    plot_half <- bcggtheme::wrap_titles(base_plot = base_plot,
                                        subtitle = subtitle,
                                        caption = caption,
                                        width = 14)

    ggplot2::ggsave(...,
                    file=half_slide_name,
                    plot = plot_half,
                    width=12, height=14,
                    units = "cm",
                    dpi = "retina",
                    type = "cairo")

  }


  else if(type == "two third") {

    plot_two_third <- bcggtheme::wrap_titles(base_plot = base_plot,
                                             subtitle = subtitle,
                                             caption = caption,
                                             width = 18)

    ggplot2::ggsave(...,
                    file=two_third_slide_name,
                    plot = plot_two_third,
                    width=18, height=14,
                    units = "cm",
                    dpi = "retina",
                    type = "cairo")

  }


  else if(type == "large") {

    plot_large <- bcggtheme::wrap_titles(base_plot = base_plot,
                                         subtitle = subtitle,
                                         caption = caption,
                                         width = 24)

    ggplot2::ggsave(...,
                    file=large_slide_name,
                    plot = plot_large,
                    width=24, height=14,
                    units = "cm",
                    dpi = "retina",
                    type = "cairo")

  }

  else if (type == "full") {

  plot_full <- bcggtheme::wrap_titles(base_plot = base_plot,
                                      subtitle = subtitle,
                                      caption = caption,
                                      width = 30)

  ggplot2::ggsave(...,
                    file=full_slide_name,
                    plot = plot_full,
                    width=30, height=14,
                    units = "cm",
                    dpi = "retina",
                  type = "cairo")

  }

  else if (type == "all") {

    plot_third <- bcggtheme::wrap_titles(base_plot = base_plot,
                                         subtitle = subtitle,
                                         caption = caption,
                                         width = 11)

    plot_half <- bcggtheme::wrap_titles(base_plot = base_plot,
                                        subtitle = subtitle,
                                        caption = caption,
                                        width = 14)

    plot_two_third <- bcggtheme::wrap_titles(base_plot = base_plot,
                                             subtitle = subtitle,
                                             caption = caption,
                                             width = 18)

    plot_large <- bcggtheme::wrap_titles(base_plot = base_plot,
                                         subtitle = subtitle,
                                         caption = caption,
                                         width = 24)

    plot_full <- bcggtheme::wrap_titles(base_plot = base_plot,
                                        subtitle = subtitle,
                                        caption = caption,
                                        width = 30)

    ggplot2::ggsave(...,
                    file=third_slide_name,
                    plot = plot_third,
                    width=11, height=14,
                    units = "cm",
                    dpi = "retina",
                    type = "cairo")

    ggplot2::ggsave(...,
                    file=half_slide_name,
                    plot = plot_half,
                    width=14, height=14,
                    units = "cm",
                    dpi = "retina",
                    type = "cairo")

    ggplot2::ggsave(...,
                    file=two_third_slide_name,
                    plot = plot_two_third,
                    width=18, height=14,
                    units = "cm",
                    dpi = "retina",
                    type = "cairo")

    ggplot2::ggsave(...,
                    file=large_slide_name,
                    plot = plot_large,
                    width=24, height=14,
                    units = "cm",
                    dpi = "retina",
                    type = "cairo")

    ggplot2::ggsave(...,
                    file=full_slide_name,
                    plot = plot_full,
                    width=30, height=14,
                    units = "cm",
                    dpi = "retina",
                    type = "cairo")

  }


  else {"type only takes values of one third, half, two third, large, full, or all"}


  return(ret)

}

