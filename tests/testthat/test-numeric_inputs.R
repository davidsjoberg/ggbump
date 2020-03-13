library(dplyr)
library(testthat)
library(ggplot2)

df <- tibble(country = c(
  "India", "India", "India",
  "Sweden", "Sweden", "Sweden",
  "Germany", "Germany", "Germany",
  "Finland", "Finland", "Finland"),
  year = c(2011, 2012, 2013,
           2011, 2012, 2013,
           2011, 2012, 2013,
           2011, 2012, 2013),
  rank = c(4, 2, 2, 3, 1, 4, 2, 3, 1, 1, 4, 3))

test_that("multiplication works", {
  expect_equal((ggplot(df, aes(year, rank, color = country)) +
                  geom_point(size = 10) +
                  geom_bump(size = 2)) %>% class(),
               c("gg", "ggplot"))
  expect_equal((ggplot(df %>% filter(country == "Sweden"), aes(year, rank)) +
                  geom_point(size = 10) +
                  geom_bump(size = 2)
  ) %>% class(),
               c("gg", "ggplot"))
  expect_equal((ggplot(df %>% filter(country == "Sweden"), aes(year, rank)) +
                  geom_point(size = 10) +
                  geom_bump(size = 2, smooth = 10)
  ) %>% class(),
               c("gg", "ggplot"))
  expect_warning(ggplot(df %>% filter(country == "Sweden"), aes(year, rank)) +
                  geom_point(size = 10) +
                  geom_bump(size = 2, aes(smooth = 10))
  )

})
