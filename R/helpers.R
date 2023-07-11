#' sigmoid
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
#' @import dplyr
#' @importFrom tidyr drop_na
#' @importFrom purrr pmap_dfr
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
#' rank_sigmoid
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
