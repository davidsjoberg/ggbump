#' ggbump extensions to ggplot2
#'
#' These ggproto objects are participate in the ggproto class extension
#' mechanism provided by ggplot2. They are typically of no concern, as they can
#' be used through the `geom_*()` functions.
#'
#' @name ggbump-ggproto
#' @rdname ggbump-ggproto
NULL

# ** StatBump ------------------------------------------------------------------

#' @export
#' @format NULL
#' @usage NULL
#' @rdname ggbump-ggproto
StatBump <- ggplot2::ggproto("StatBump", ggplot2::Stat,
                             setup_data = function(data, params) {
                               # Create x_lag, and y_lag to be passed to `compute_group`
                               # Factors need this to be able to compute a sigmoid function
                               data <- data %>%
                                 dplyr::mutate(r = dplyr::row_number()) %>%
                                 dplyr::arrange(x) %>%
                                 dplyr::group_by_at(vars(-PANEL, -group, -x, -y, -r)) %>%
                                 dplyr::mutate(x_lag = dplyr::lag(x),
                                               y_lag = dplyr::lag(y)) %>%
                                 dplyr::ungroup() %>%
                                 dplyr::arrange(r) %>%
                                 dplyr::select(-.data$r) %>%
                                 as.data.frame()
                               data
                             },
                             compute_group = function(data, scales, smooth = 8, direction = "x") {
                               data <- data %>%
                                 dplyr::arrange(x)

                               # Handling of the special case of factors
                               # Factors come as a df with one row
                               if(nrow(data) == 1) {
                                 if(is.na(data$x_lag) | is.na(data$y_lag)) {
                                   return(data %>% dplyr::slice(0))
                                 } else {
                                   out <- sigmoid(data$x_lag, data$x, data$y_lag, data$y,
                                                  smooth = smooth, direction = direction)
                                   return(as.data.frame(out))
                                 }
                               }

                               # Normal case
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
#' geom_bump
#'
#' Creates a ggplot that makes a smooth rank over time. To change the `smooth`
#' argument you need to put it outside of the `aes` of the geom. Uses the x and y aestethics.
#' Usually you want to compare multiple lines and if so, use the `color` aestethic.
#' To change the direction of the curve to 'vertical' set `direction = "y`
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
#' library(ggbump)
#' df <- data.frame(country = c(
#'   "India", "India", "India",
#'   "Sweden", "Sweden", "Sweden",
#'   "Germany", "Germany", "Germany",
#'   "Finland", "Finland", "Finland"),
#' year = c(2011, 2012, 2013,
#' 2011, 2012, 2013,
#' 2011, 2012, 2013,
#' 2011, 2012, 2013),
#' month = c("January", "July", "November",
#'           "January", "July", "November",
#'           "January", "July", "November",
#'           "January", "July", "November"),
#' rank = c(4, 2, 2, 3, 1, 4, 2, 3, 1, 1, 4, 3))
#'
#' # Contingous x axis
#' ggplot(df, aes(year, rank, color = country)) +
#'   geom_point(size = 10) +
#'   geom_bump(size = 2)
#'
#' # Discrete x axis
#' ggplot(df, aes(month, rank, color = country)) +
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
