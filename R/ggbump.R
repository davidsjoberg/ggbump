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
#'
#' @return a data frame
#'
#' @export
sigmoid <- function(x_from, x_to, y_from, y_to, smooth = 5, n = 100) {
  x <- seq(-smooth, smooth, length = n)
  y <- exp(x) / (exp(x) + 1)
  data.frame(x = (x + smooth) / (smooth * 2) * (x_to - x_from) + x_from,
             y = y * (y_to - y_from) + y_from)
}

# rank_sigmoid -------------------------------------------------------------
#' @title rank_sigmoid
#'
#' Creates a longer dataframe with coordinates for a smoothed line.
#'
#' @param x vector
#' @param y vector
#' @param smooth smooth parameter. Higher means less smoothing
#'
#' @return a data frame
#'
#' @export
rank_sigmoid <- function(x, y, smooth = 8) {
  .df <- dplyr::tibble(x = x,
                y = y) %>%
    dplyr::mutate(x_lag = dplyr::lag(x),
                 y_lag = dplyr::lag(y)) %>%
    tidyr::drop_na("x_lag")
    purrr::pmap_dfr(.df, ~sigmoid(x_from = ..3, x_to = ..1, y_from = ..4, y_to = ..2, smooth  = smooth))
}

# stat ---------
StatBump <- ggplot2::ggproto("StatBump", ggplot2::Stat,
                           compute_group = function(data, scales) {
                             if(nrow(data) == 1) {
                               warning("'StatBump' needs at least two observations per group")
                               return(data %>% dplyr::slice(0))
                             }
                             if("smooth" %in% names(data)) {
                               smoother <- unique(data[, "smooth"])
                               data <- data %>% dplyr::select(-smooth)
                             } else {
                               smoother <- 5
                             }

                             out <-rank_sigmoid(data$x, data$y, smooth = smoother) %>%
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

                           required_aes = c("x", "y"),
                           default_aes = ggplot2::aes(smooth = 5)
)
#' @title geom_bump
#'
#' Creates a ggplot that makes a smooth rank over time. To change the `smooth`
#' argument you need to put it in the `aes` of the geom.
#'
#' @param mapping provide you own mapping. both x and y need to be numeric.
#' @param data provide you own data
#' @param geom xhange geom
#' @param position change position
#' @param na.rm remove missing values
#' @param show.legend show legend in plot
#' @param inherit.aes should the geom inherits aestethics
#' @param ... other arguments to be passed to the geom
#'
#' @return ggplot layer
#'
#' @examples
#' \dontrun{
#'   df <- tibble(country = c(
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
#' }
#'
#' @export
geom_bump <- function(mapping = NULL, data = NULL, geom = "line",
                              position = "identity", na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatBump, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

