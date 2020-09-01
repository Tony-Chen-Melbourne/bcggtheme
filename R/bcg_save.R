#' Output ggplot charts in the desired size
#' @name bcg_save
#' @param filename Name for output file
#' @param type Type of chart to output, either "halfslide" or "fullslide" or "all"
#' @import ggplot2
#' @import stringr
#' @export
bcg_save <- function(...,
                     filename,
                     type) {

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

    ggplot2::ggsave(...,
                    file=half_slide_name, width=12, height=14,
                    units = "cm",
                    dpi = 400)

  }

  else if (type == "fullslide") {

    ggplot2::ggsave(...,
                    file=full_slide_name, width=28, height=14,
                    units = "cm",
                    dpi = 400)

  }

  else if (type == "all") {

    ggplot2::ggsave(...,
                    file=half_slide_name, width=12, height=14,
                    units = "cm",
                    dpi = 400)

    ggplot2::ggsave(...,
                    file=full_slide_name, width=28, height=14,
                    units = "cm",
                    dpi = 400)

  }


  else {"type only takes values of halfslide, fullslide, or all"}


  return(ret)

}

