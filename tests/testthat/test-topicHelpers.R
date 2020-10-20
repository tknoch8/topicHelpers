library(testthat)
library(topicHelpers)
library(tidyverse)

test_mod <- rio::import(here::here("k_50_stm.rds"))

test_that("get_td_beta returns a tibble with > 0 rows", {
  td_beta <- get_td_beta(test_mod)
  expect_true(is_tibble(td_beta))
  expect_false(nrow(td_beta) == 0)
})

test_that("get_td_gamma returns a tibble with > 0 rows", {
  td_gamma <- get_td_gamma(test_mod)
  expect_true(is_tibble(td_gamma))
  expect_false(nrow(td_gamma) == 0)
})

test_that("column names in td_beta are correct", {
  td_beta <- get_td_beta(test_mod)
  expect_true(all.equal(names(td_beta), c("topic", "term", "beta")))
})

test_that("column names in td_gamma are correct", {
  td_gamma <- get_td_beta(test_mod)
  expect_true(all.equal(names(td_gamma), c("topic", "term", "beta")))
})
