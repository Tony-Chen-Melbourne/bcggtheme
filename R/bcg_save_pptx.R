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

  # Creating the right names for outputting first
  dir <- tools::file_path_sans_ext(filename)
  filetype <- tools::file_ext(filename)
  base_name <- tools::file_path_sans_ext(basename(filename))

  half_slide_name <- stringr::str_c(dir,"/", base_name, "_halfslide.", filetype)
  full_slide_name <- stringr::str_c(dir,"/", base_name, "_fullslide.", filetype)

  # Last plot
  plot <- ggplot2::last_plot()

  # Creating the file directory if it doesn't already exist
  if(!dir.exists(dir)) {

    dir.create(dir, recursive = TRUE)

  }

  # Setting up halfslide, fullslide, and all versions
  ret <- if(type == "halfslide") {

    officer::read_pptx() %>%
      officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot),
                       location = ph_location(width = 12/2.54, height = 14/2.54, type = "body")) %>%
      print(target = half_slide_name)


  }

  else if (type == "fullslide") {

    officer::read_pptx() %>%
      officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot),
                       location = ph_location(width = 28/2.54, height = 14/2.54, type = "body")) %>%
      print(target = full_slide_name)

  }

  else if (type == "all") {

    officer::read_pptx() %>%
      officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot),
                       location = ph_location(width = 12/2.54, height = 14/2.54, type = "body")) %>%
      print(target = half_slide_name)


    officer::read_pptx() %>%
      officer::add_slide(layout = "Title and Content", master = "Office Theme") %>%
      officer::ph_with(rvg::dml(ggobj = plot),
                       location = ph_location(width = 28/2.54, height = 14/2.54, type = "body")) %>%
      print(target = full_slide_name)

  }


  else {"type only takes values of halfslide, fullslide, or all"}


  return(ret)

}

