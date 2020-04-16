#' @importFrom dplyr %>%
#'
#' @title sigmoid
#'
#' Creates a longer dataframe with coordinates for a smoothed line.
#'
#' @param x_from start x value
#' @param x_to end x value
#' @param y_from start y value
#' @param y_to end y values
#' @param n number of point that should be smoothed
#' @param smooth smooth parameter. Higher means less smoothing
#' @param direction the character x or y depending on direction of smoothing
#'
#' @return a data frame
#'
#' @export
sigmoid <- function(x_from, x_to, y_from, y_to, smooth = 5, n = 100, direction = "x") {
  if(!direction %in% c("x", "y")) {stop("Only the directions x or y is allowed.")}

  if(direction == "x") {
    x <- seq(-smooth, smooth, length = n)
    y <- exp(x) / (exp(x) + 1)
    out <- data.frame(x = (x + smooth) / (smooth * 2) * (x_to - x_from) + x_from,
               y = y * (y_to - y_from) + y_from)
  }

  if(direction == "y") {
    y <- seq(-smooth, smooth, length = n)
    x <- exp(y) / (exp(y) + 1)
    out <- data.frame(y = (y + smooth) / (smooth * 2) * (y_to - y_from) + y_from,
                      x = x * (x_to - x_from) + x_from)
  }
  out
}

# rank_sigmoid -------------------------------------------------------------
#' @title rank_sigmoid
#'
#' Creates a longer dataframe with coordinates for a smoothed line.
#'
#' @param x vector
#' @param y vector
#' @param smooth smooth parameter. Higher means less smoothing
#' @param direction the character x or y depending of smoothing direction
#'
#' @return a data frame
#'
#' @export
rank_sigmoid <- function(x, y, smooth = 8, direction = "x") {
  .df <- dplyr::tibble(x = x,
                y = y) %>%
    dplyr::mutate(x_lag = dplyr::lag(x),
                 y_lag = dplyr::lag(y)) %>%
    tidyr::drop_na("x_lag")
    purrr::pmap_dfr(.df, ~sigmoid(x_from = ..3, x_to = ..1, y_from = ..4, y_to = ..2, smooth  = smooth, direction = direction))
}

# geom_bump --------------------------------------------------------------------

# ** StatBump ------------------------------------------------------------------
StatBump <- ggplot2::ggproto("StatBump", ggplot2::Stat,
                           compute_group = function(data, scales, smooth, direction = direction) {
                             if(nrow(data) == 1) {
                               warning("'StatBump' needs at least two observations per group")
                               return(data %>% dplyr::slice(0))
                             }
                             data <- data %>%
                               arrange(x)

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


# geom_sigmoid -----------------------------------------------------------------

# ** StatSigmoid ------------------------------------------------------------------
StatSigmoid <- ggplot2::ggproto("StatSigmoid", ggplot2::Stat,
                             compute_group = function(data, scales, smooth, direction) {
                               out <- sigmoid(data$x, data$xend, data$y, data$yend,
                                              smooth = smooth, direction = direction)
                               out
                             },

                             required_aes = c("x", "y", "xend", "yend")
)

# ** geom_sigmoid -----------------------------------------------------------------
#' @title geom_sigmoid
#'
#' Creates a ggplot that makes a smooth rank over time. To change the `smooth`
#' argument you need to put it outside of the `aes` of the geom.
#' Uses the x, xend, y and yend aestethics. Make sure each sigmoid curve is its own group.
#'
#' @param mapping provide you own mapping. both x, xend, y and yend need to be numeric.
#' @param data provide you own data
#' @param geom xhange geom
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
#' df <- data.frame(x = 1:6,
#'        y = 5:10,
#'        xend = 7,
#'        yend = -3:2)
#'
#' ggplot(df, aes(x = x, xend = xend, y = y, yend = yend, color = factor(x))) +
#'   geom_sigmoid()
#'
#' @export
geom_sigmoid <- function(mapping = NULL, data = NULL, geom = "line",
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      smooth = 8, direction = "x", inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatSigmoid, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, smooth = smooth, direction = direction, ...)
  )
}
