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
  plot <- ggplot2::last_plot()

  # Getting the text labels
  title <- plot$labels$title
  subtitle <- plot$labels$subtitle
  caption <- plot$labels$caption

  # Creating the right names for outputting first
  dir <- tools::file_path_sans_ext(filename)
  filetype <- tools::file_ext(filename)
  base_name <- tools::file_path_sans_ext(basename(filename))

  half_slide_name <- stringr::str_c(dir,"/", base_name, "_halfslide.", filetype)
  full_slide_name <- stringr::str_c(dir,"/", base_name, "_fullslide.", filetype)


  # Creating the file directory if it doesn't already exist
  if(!dir.exists(dir)) {

    dir.create(dir, recursive = TRUE)

  }

  # Setting up halfslide, fullslide, and all versions
  ret <- if(type == "halfslide") {

    plot_half <- plot + labs(title = title %>% stringr::str_wrap(width = 35),
                             subtitle = subtitle %>% stringr::str_wrap(width = 35),
                             caption = caption %>% stringr::str_wrap(width = 55)
    )

    officer::read_pptx() %>%
      officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot_half),
                       location = ph_location(width = 12/2.54, height = 14/2.54, type = "body")) %>%
      print(target = half_slide_name)


  }

  else if (type == "fullslide") {

    plot_full <- plot + labs(title = title %>% stringr::str_wrap(width = 90),
                             subtitle = subtitle %>% stringr::str_wrap(width = 90),
                             caption = caption %>% stringr::str_wrap(width = 150)
    )

    officer::read_pptx() %>%
      officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot_full),
                       location = ph_location(width = 28/2.54, height = 14/2.54, type = "body")) %>%
      print(target = full_slide_name)

  }

  else if (type == "all") {

    plot_half <- plot + labs(title = title %>% stringr::str_wrap(width = 35),
                             subtitle = subtitle %>% stringr::str_wrap(width = 35),
                             caption = caption %>% stringr::str_wrap(width = 55)
    )

    plot_full <- plot + labs(title = title %>% stringr::str_wrap(width = 90),
                             subtitle = subtitle %>% stringr::str_wrap(width = 90),
                             caption = caption %>% stringr::str_wrap(width = 150)
    )

    officer::read_pptx() %>%
      officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot_half),
                       location = ph_location(width = 12/2.54, height = 14/2.54, type = "body")) %>%
      print(target = half_slide_name)


    officer::read_pptx() %>%
      officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot_full),
                       location = ph_location(width = 28/2.54, height = 14/2.54, type = "body")) %>%
      print(target = full_slide_name)

  }


  else {"type only takes values of halfslide, fullslide, or all"}


  return(ret)

}

