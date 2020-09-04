#' Output ggplot charts to powerpoint files
#' @name bcg_save_pptx
#' @param filename Name for output file
#' @param type Type of chart to output, either "halfslide" or "fullslide" or "all"
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
  title <- base_plot$labels$title
  subtitle <- base_plot$labels$subtitle
  caption <- base_plot$labels$caption

  # Creating the right names for outputting first
  dir <- tools::file_path_sans_ext(filename)
  filetype <- tools::file_ext(filename)
  base_name <- tools::file_path_sans_ext(basename(filename))

  half_slide_name <- stringr::str_c(dir,"/", base_name, "_halfslide.", filetype)
  half_slide_name_sans_title <- stringr::str_c(dir,"/", base_name, "_halfslide_sans_title.", filetype)

  full_slide_name <- stringr::str_c(dir,"/", base_name, "_fullslide.", filetype)
  full_slide_name_sans_title <- stringr::str_c(dir,"/", base_name, "_fullslide_sans_title.", filetype)

  # Creating the file directory if it doesn't already exist
  if(!dir.exists(dir)) {

    dir.create(dir, recursive = TRUE)

  }

  # Creating different versions for outputting

  #No title
  plot_sans_title <- base_plot + labs(title = NULL,
                                      subtitle = NULL,
                                      caption = NULL
  )

  # Half size
  plot_half <- base_plot + ggplot2::labs(title = title %>% stringr::str_wrap(width = 35),
                                    subtitle = subtitle %>% stringr::str_wrap(width = 35),
                                    caption = caption %>% stringr::str_wrap(width = 55)
  )

  # Full size
  plot_full <- base_plot + labs(title = title %>% stringr::str_wrap(width = 90),
                           subtitle = subtitle %>% stringr::str_wrap(width = 90),
                           caption = caption %>% stringr::str_wrap(width = 150)
  )

  # Getting the ppt base, and calling the information for officer
  ppt_base <- system.file("extdata", "base_ppt.pptx", package = "bcggtheme")

  #layout_summary(ppt_base)
  #layout_properties (ppt_base, layout = "Title and Content" )

  # Setting up halfslide, fullslide, and all versions
  ret <- if(type == "halfslide") {

    officer::read_pptx(ppt_base) %>%
      officer::remove_slide() %>%
      officer::add_slide(layout = "half slide", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot_sans_title),
                       location = ph_location_label(ph_label = "Content Placeholder 2")) %>%
      print(target = half_slide_name_sans_title)

    officer::read_pptx(ppt_base) %>%
      officer::remove_slide() %>%
      officer::add_slide(layout = "half slide", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot_half),
                       ph_location_label(ph_label = "Content Placeholder 2")) %>%
      print(target = half_slide_name)

  }

  else if (type == "fullslide") {

    officer::read_pptx(ppt_base) %>%
      officer::remove_slide() %>%
      officer::add_slide(layout = "full slide", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot_sans_title),
                       ph_location_label(ph_label = "Content Placeholder 2")) %>%
      print(target = full_slide_name_sans_title)

    officer::read_pptx(ppt_base) %>%
      officer::remove_slide() %>%
      officer::add_slide(layout = "full slide", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot_full),
                       ph_location_label(ph_label = "Content Placeholder 2")) %>%
      print(target = full_slide_name)

  }

  else if (type == "all") {

    # Both half slide versions
    officer::read_pptx(ppt_base) %>%
      officer::remove_slide() %>%
      officer::add_slide(layout = "half slide", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot_sans_title),
                       ph_location_label(ph_label = "Content Placeholder 2")) %>%
      print(target = half_slide_name_sans_title)

    officer::read_pptx(ppt_base) %>%
      officer::remove_slide() %>%
      officer::add_slide(layout = "half slide", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot_half),
                       ph_location_label(ph_label = "Content Placeholder 2")) %>%
      print(target = half_slide_name)

    # Both full slide versions
    officer::read_pptx(ppt_base) %>%
      officer::remove_slide() %>%
      officer::add_slide(layout = "full slide", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot_sans_title),
                       ph_location_label(ph_label = "Content Placeholder 2")) %>%
      print(target = full_slide_name_sans_title)

    officer::read_pptx(ppt_base) %>%
      officer::remove_slide() %>%
      officer::add_slide(layout = "full slide", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot_full),
                       ph_location_label(ph_label = "Content Placeholder 2")) %>%
      print(target = full_slide_name)


  }


  else {"type only takes values of halfslide, fullslide, or all"}


  return(ret)

}

