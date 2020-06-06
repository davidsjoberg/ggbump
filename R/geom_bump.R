# ** StatBump ------------------------------------------------------------------
StatBump <- ggplot2::ggproto("StatBump", ggplot2::Stat,
                             compute_group = function(data, scales, smooth, direction = direction) {
                               if(nrow(data) == 1) {
                                 warning("'StatBump' needs at least two observations per group")
                                 return(data %>% dplyr::slice(0))
                               }
                               data <- data %>%
                                 dplyr::arrange(x)

                               out <-rank_sigmoid(data$x, data$y, smooth = smooth, direction = direction) %>%
                                 dplyr::mutate(key = 1) %>%
                                 dplyr::left_join(data %>%
                                                    dplyr::select(-x, -y) %>%
                                                    dplyr::mutate(key = 1) %>%
                                                    dplyr::distinct(),
                                                  by = "key") %>%
                                 dplyr::select(-key) %>%
                                 as.data.frame()
                               out
                             },

                             required_aes = c("x", "y")
)

# ** geom_bump -----------------------------------------------------------------
#' @title geom_bump
#'
#' Creates a ggplot that makes a smooth rank over time. To change the `smooth`
#' argument you need to put it outside of the `aes` of the geom. Uses the x and y aestethics.
#'
#' @param mapping provide you own mapping. both x and y need to be numeric.
#' @param data provide you own data
#' @param geom change geom
#' @param position change position
#' @param na.rm remove missing values
#' @param show.legend show legend in plot
#' @param smooth how much smooth should the curve have? More means steeper curve.
#' @param direction the character x or y depending of smoothing direction
#' @param inherit.aes should the geom inherits aestethics
#' @param ... other arguments to be passed to the geom
#'
#' @return ggplot layer
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(country = c(
#'   "India", "India", "India",
#'   "Sweden", "Sweden", "Sweden",
#'   "Germany", "Germany", "Germany",
#'   "Finland", "Finland", "Finland"),
#' year = c(2011, 2012, 2013,
#' 2011, 2012, 2013,
#' 2011, 2012, 2013,
#' 2011, 2012, 2013),
#' rank = c(4, 2, 2, 3, 1, 4, 2, 3, 1, 1, 4, 3))
#'
#' ggplot(df, aes(year, rank, color = country)) +
#'   geom_point(size = 10) +
#'   geom_bump(size = 2)
#'
#' @export
geom_bump <- function(mapping = NULL, data = NULL, geom = "line",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      smooth = 8, direction = "x", inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatBump, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, smooth = smooth, direction = direction, ...)
  )
}
