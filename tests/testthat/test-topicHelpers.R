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

test_that("class of make_topic_prevalence_plot is gg and ggplot", {
  td_gamma <- get_td_gamma(test_mod)
  test_plot <- make_topic_prevalence_plot(gamma_terms_obj = td_gamma)
  expect_true(all.equal(class(test_plot), c("gg", "ggplot")))
})

test_that("get_top_n_beta_terms two correct-type columns", {
  td_beta <- get_td_beta(test_mod)
  beta_terms <- get_top_n_beta_terms(td_beta_obj = td_beta, n = 8)
  expect_true(length(beta_terms) == 2)
  # make list containing column classes
  my_types <- map(beta_terms, class)
  expect_true(my_types$topic == "integer" & my_types$terms == "character")
})






