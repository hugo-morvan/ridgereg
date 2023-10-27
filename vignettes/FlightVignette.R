## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----library------------------------------------------------------------------
library(ridgereg)

## ----example------------------------------------------------------------------
library(ridgereg)
ridgereg_model <- ridgereg:::ridgereg$new(Petal.Length~Species, iris, 0.001)
ridgereg_model$beta_ridge

