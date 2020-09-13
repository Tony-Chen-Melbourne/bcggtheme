#' Output ggplot charts to powerpoint files
#' @name bcg_save_pptx
#' @param filename Name for output file
#' @param type Type of chart to output, either "third", "half", "two third", "large", "full", or "all"
#' @import ggplot2
#' @import stringr
#' @import officer
#' @import rvg
#' @export
bcg_save_pptx <- function(...,
                     filename,
                     type) {

  # Last plot
  base_plot <- ggplot2::last_plot()

  # Getting the text labels
  subtitle <- base_plot$labels$subtitle
  caption <- base_plot$labels$caption
  x <- base_plot$labels$x

  # Creating the right names for outputting first
  dir <- tools::file_path_sans_ext(filename)
  filetype <- tools::file_ext(filename)
  base_name <- tools::file_path_sans_ext(basename(filename))

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

  # Getting the ppt base, and calling the information for officer
  ppt_base <- system.file("extdata", "base_ppt.pptx", package = "bcggtheme")

  #ppt_base <- read_pptx(ppt_base)
  #layout_summary(ppt_base)
  #layout_properties (ppt_base, layout = "full slide" )

  # Setting up all versions

  ret <- if(type == "one third") {

    plot_third <- bcggtheme::wrap_titles(base_plot = base_plot,
                                         subtitle = subtitle,
                                         caption = caption,
                                         x=x,
                                         width = 11)

    officer::read_pptx(ppt_base) %>%
      officer::remove_slide() %>%
      officer::add_slide(layout = "one third slide", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot_third),
                       ph_location_label(ph_label = "Content Placeholder 2")) %>%
      print(target = third_slide_name)

  }

  else if(type == "half") {

    plot_half <- bcggtheme::wrap_titles(base_plot = base_plot,
                                        subtitle = subtitle,
                                        caption = caption,
                                        x=x,
                                        width = 14)

    officer::read_pptx(ppt_base) %>%
      officer::remove_slide() %>%
      officer::add_slide(layout = "half slide", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot_half),
                       ph_location_label(ph_label = "Content Placeholder 2")) %>%
      print(target = half_slide_name)

  }

  else if(type == "two third") {

    plot_two_third <- bcggtheme::wrap_titles(base_plot = base_plot,
                                             subtitle = subtitle,
                                             caption = caption,
                                             x=x,
                                             width = 18)

    officer::read_pptx(ppt_base) %>%
      officer::remove_slide() %>%
      officer::add_slide(layout = "two third slide", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot_two_third),
                       ph_location_label(ph_label = "Content Placeholder 2")) %>%
      print(target = two_third_slide_name)

  }


  else if(type == "large") {

    plot_large <- bcggtheme::wrap_titles(base_plot = base_plot,
                                         subtitle = subtitle,
                                         caption = caption,
                                         x=x,
                                         width = 24)

    officer::read_pptx(ppt_base) %>%
      officer::remove_slide() %>%
      officer::add_slide(layout = "large slide", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot_large),
                       ph_location_label(ph_label = "Content Placeholder 2")) %>%
      print(target = large_slide_name)

  }

  else if (type == "full") {

    plot_full <- bcggtheme::wrap_titles(base_plot = base_plot,
                                        subtitle = subtitle,
                                        caption = caption,
                                        x=x,
                                        width = 30)

    officer::read_pptx(ppt_base) %>%
      officer::remove_slide() %>%
      officer::add_slide(layout = "full slide", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot_full),
                       ph_location_label(ph_label = "Content Placeholder 2")) %>%
      print(target = full_slide_name)

  }

  else if (type == "all") {

    plot_third <- bcggtheme::wrap_titles(base_plot = base_plot,
                                         subtitle = subtitle,
                                         caption = caption,
                                         x=x,
                                         width = 11)
    plot_half <- bcggtheme::wrap_titles(base_plot = base_plot,
                                        subtitle = subtitle,
                                        caption = caption,
                                        x=x,
                                        width = 14)
    plot_two_third <- bcggtheme::wrap_titles(base_plot = base_plot,
                                             subtitle = subtitle,
                                             caption = caption,
                                             x=x,
                                             width = 18)
    plot_large <- bcggtheme::wrap_titles(base_plot = base_plot,
                                         subtitle = subtitle,
                                         caption = caption,
                                         x=x,
                                         width = 24)
    plot_full <- bcggtheme::wrap_titles(base_plot = base_plot,
                                        subtitle = subtitle,
                                        caption = caption,
                                        x=x,
                                        width = 30)

    # Outputting all types
    officer::read_pptx(ppt_base) %>%
      officer::remove_slide() %>%
      officer::add_slide(layout = "one third slide", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot_third),
                       ph_location_label(ph_label = "Content Placeholder 2")) %>%
      print(target = third_slide_name)

    officer::read_pptx(ppt_base) %>%
      officer::remove_slide() %>%
      officer::add_slide(layout = "half slide", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot_half),
                       ph_location_label(ph_label = "Content Placeholder 2")) %>%
      print(target = half_slide_name)

    officer::read_pptx(ppt_base) %>%
      officer::remove_slide() %>%
      officer::add_slide(layout = "two third slide", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot_two_third),
                       ph_location_label(ph_label = "Content Placeholder 2")) %>%
      print(target = two_third_slide_name)

    officer::read_pptx(ppt_base) %>%
      officer::remove_slide() %>%
      officer::add_slide(layout = "large slide", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot_large),
                       ph_location_label(ph_label = "Content Placeholder 2")) %>%
      print(target = large_slide_name)

    officer::read_pptx(ppt_base) %>%
      officer::remove_slide() %>%
      officer::add_slide(layout = "full slide", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot_full),
                       ph_location_label(ph_label = "Content Placeholder 2")) %>%
      print(target = full_slide_name)


  }


  else {"type only takes values of one third, half, two third, large, full, or all"}


  return(ret)

}

