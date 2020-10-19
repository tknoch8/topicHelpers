library(testthat)
library(tidyverse)

test_that("get_td_gamma returns a tibble", {
  require(topicHelpers)
  td_beta <- get_td_beta(rio::import(here::here("k_50_stm.rds")))
  expect_true(is_tibble(td_beta))
  expect_false(nrow(td_beta) == 0)
})
