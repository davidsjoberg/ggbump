library(testthat)
library(ggplot2)

df <- dplyr::tibble(country = c(
  "India", "India", "India",
  "Sweden", "Sweden", "Sweden",
  "Germany", "Germany", "Germany",
  "Finland", "Finland", "Finland"),
  year = c(2011, 2012, 2013,
           2011, 2012, 2013,
           2011, 2012, 2013,
           2011, 2012, 2013),
  rank = c(4, 2, 2, 3, 1, 4, 2, 3, 1, 1, 4, 3))

df_sig <- data.frame(xend = 1:10,
                 y = 1,
                 x = 1,
                 yend = 10:1)

test_that("Basic usage", {
  expect_equal(class(ggplot(df, aes(year, rank, color = country)) +
                  geom_point(size = 10) +
                  geom_bump(size = 2)),
               c("gg", "ggplot"))
  expect_equal(class(ggplot(dplyr::filter(df, country == "Sweden"), aes(year, rank)) +
                  geom_point(size = 10) +
                  geom_bump(size = 2)),
               c("gg", "ggplot"))
  expect_equal(class(ggplot(dplyr::filter(df, country == "Sweden"), aes(year, rank)) +
                  geom_point(size = 10) +
                  geom_bump(size = 2, smooth = 10)
  ),
               c("gg", "ggplot"))
  expect_warning(ggplot(dplyr::filter(df, country == "Sweden"), aes(year, rank)) +
                  geom_point(size = 10) +
                  geom_bump(size = 2, aes(smooth = 10))
  )

})

test_that("geom_sigmoid", {
  expect_equal(class(ggplot(df_sig, aes(x = x, xend = xend, y = y, yend = yend, color = factor(yend))) +
                       geom_sigmoid(size = 2)),
               c("gg", "ggplot"))
})
