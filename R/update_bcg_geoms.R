#' Update the main ggplot geoms
#' @name update_bcg_geoms
#' @import ggplot2

update_bcg_geoms <- function() {

  base_size <- 16

  update_geom_defaults("point",
                       list(size = 6 / .pt))

  update_geom_defaults("bar",
                       list(size = 0.75 / .pt))

  update_geom_defaults("col",
                       list(size = 0.75 / .pt))

  update_geom_defaults("line",
                       list(size = 3 / .pt))

  update_geom_defaults("text",
                       list(size = 18 / .pt))

  update_geom_defaults("path",
                       list(size = 3 / .pt))

  update_geom_defaults(ggrepel::GeomTextRepel,
                       list(size = base_size/ .pt))

  update_geom_defaults(ggrepel::GeomLabelRepel,
                       list(size = base_size/ .pt))

  update_geom_defaults("label",
                       list(size = base_size/ .pt))



}
