#******************************************************************************
# 0. Identification -----------------------------------------------------------
# Title: Cluster Sampling - Quiz 3
# Institution: Sampling Course
# Responsable: MVM
# Executive Summary: This script contains answers for the third quiz
# Date: June 26, 2026
#******************************************************************************

rm(list = ls())

# 1. Load packages ------------------------------------------------------------

c("tidyverse", "janitor", "glue") |>
  purrr::walk(library, character.only = TRUE)

###############################################################################
############## Sampling People, Networks and Records ##########################
#################### Week 4 Quiz: Problem Set #################################
###############################################################################

# A stratified random sample size n = 30 is to be selected from the following 
# population:

pop <- tibble::tibble(
  h = c(1, 2, 3),
  N_h = c(100, 200, 300),
  W_h = N_h/sum(N_h),
  P_h = c(0.8, 0.5, 0.2)
)

# 1) Calculate the overall proportion (across strata) of the population that 
# has the characteristic of interest.

p_w <- with(pop, sum(W_h * P_h))

# 2) Compute the sampling variance of the mean from the proportionately
# allocated sample of n = 30

n <- 30
N <- sum(pop$N_h)

f <- n/N

pop <- pop |> 
  mutate(
    n_h = n * W_h,
    p_h = P_h, # typo?
    var_p_h = (1 - n_h / N_h) * ((p_h * (1 - p_h))/(n_h - 1))
    )

# 3) If S2 = 0.24, what is the simple random sampling variance of the estimated
# proportion,

S2 <- 0.24
var_srs <- (1 - f) * (S2/n) 
                
# 4) What is the gain in precision from using proportionately allocated stratified
# sampling?

var_p_w <- with(pop, sum((W_h^2) * var_p_h))
deff <- var_p_w / var_srs
precision <- 1 - deff

# 5) Compute the sampling variance of the mean from the entire “equal
# allocation” sample of n = 30. That is, compute v(pw)= Σh=1Wh2v(ph ) using the
# hint in problem 2 when sample sizes are equal to 10 in each stratum.

# 6) What is the design effect from using “equal allocation” stratified sampling? v(p_w)
# That is, what is deff = v(p_w) / v(p_0)?
