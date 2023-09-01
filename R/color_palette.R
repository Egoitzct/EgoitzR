#' @title color_palette
#' @author Egoitz Carral
#' @description
#' Some preconfigured color palettes, useful for plots
#' @rdname color_palette
#'
#' @export

egoitz_colors <- c(`light_blue`           = "#C3DFE0",
                   `pistachio`            = "#BCD979",
                   `olivine`              = "#9DAD6F",
                   `dim_gray`             = "#7D6D61",
                   `walnut_brown`         = "#5E574D",
                   `rose_taupe`           = "#8B575C",
                   `madder`               = "#9E0031",
                   `murrey`               = "#8E0045",
                   `byzantium`            = "#770058",
                   `tyrian_purple`        = "#600047",
                   `chocolate_cosmos`     = "#44001A",
                   `tyrian_chocolate`     = "#55172F",
                   `eggplant`             = "#642C42",
                   `pink_lavender`        = "#E4B7E5",
                   `african_violet`       = "#B288C0",
                   `royal_purple`         = "#7E5A9B",
                   `ultra_violet`         = "#63458A",
                   `veronica`             = "#9A48D0",
                   `amethyst`             = "#A359D4",
                   `lavander_floral`      = "#B376DC",
                   `wisteria`             = "#C697E5",
                   `flax`                 = "#F3E37C",
                   `naples_yellow`        = "#F3D34A",
                   `butterscotch`         = "#EEA243",
                   `gamboge`              = "#F3A738",
                   `peach_yellow`         = "#F1D9A7",
                   `tea_green`            = "#CCF5AC",
                   `celadon`              = "#BAD29F",
                   `cool_gray`            = "#808A9F",
                   `yinmn_blue`           = "#2C497F",
                   `russian_violet`       = "#3D2B56",
                   `blue`                 = "#0064E4",
                   `lima`                 = "#BBFB00",
                   `magenta`              = "#EB00AA",
                   `naranja`              = "#FF9E00",
                   `gray_1`               = "#D6D6D6",
                   `gray_2`               = "#616161")

#' Function to extract egoitz_colors colors as hexadecimal codes
#'
#' @param ... Character names of egoitz_colors
#'
egoitz_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (egoitz_colors)

  egoitz_colors[cols]
}

egoitz_color_palettes <- list(
  `main` = egoitz_cols("blue", "lima", "magenta", "naranja"),
  `gray` = egoitz_cols("gray_1", "gray_2"),
  `blue_green_brown` = egoitz_cols("light_blue", "pistachio", "olivine",
                                   "dim_gray", "walnut_brown", "rose_taupe"),
  `madder_chocolate` = egoitz_cols("madder", "murrey", "byzantium",
                                   "tyrian_purple", "chocolate_cosmos",
                                   "tyrian_chocolate", "eggplant"),
  `lavander_violet` = egoitz_cols("pink_lavander", "african_violet",
                                  "royal_purple", "ultra_violet", "veronica",
                                  "amethyst", "lavander_floral", "wisteria"),
  `flax_yellow` = egoitz_cols("flax", "naples_yellow", "butterscotch",
                              "gamboge", "peach_yellow"),
  `green_violet` = egoitz_cols("tea_green", "celadon", "cool_gray",
                               "yinmn_blue", "russian_violet")
)

#' Return function to interpolate a palette
#'
#' @param palette Character name of palette in egoitz_color_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
egoitz_color_pal <- function(palette = "blue_green_brown", reverse = FALSE, ...) {
  pal <- egoitz_color_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

#' Color scale constructor
#'
#' @param palette Character name of palette in egoitz_color_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_egoitz <- function(palette = "blue_green_brown", discrete = TRUE,
                                         reverse = FALSE, ...) {
  pal <- egoitz_color_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("egoitz_color_", palette),
                            palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor
#'
#' @param palette Character name of palette in egoitz_color_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or
#'            FALSE
#'
scale_fill_egoitz <- function(palette = "blue_green_brown", discrete = TRUE,
                                        reverse = FALSE, ...) {
  pal <- egoitz_color_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("egoitz_color_", palette),
                            palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}
