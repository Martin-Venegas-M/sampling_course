#******************************************************************************
# 0. Identification -----------------------------------------------------------
# Title: Faculty Salaries - Assignment 1
# Institution: Sampling Course
# Responsable: MVM
# Executive Summary: This script loads and explores the Faculty Salaries
#   dataset (PNR_PeerAssess_Mod2_FacultySalaries) for assignment 1
# Date: June 13, 2026
#******************************************************************************

rm(list = ls())

# 1. Load packages ------------------------------------------------------------

c("tidyverse", "here", "janitor", "glue") |>
  purrr::walk(library, character.only = TRUE)

###############################################################################
############## Sampling People, Networks and Records ##########################
#################### Week 2 Quiz: Problem Set #################################
###############################################################################

# For the following sample values of salaries from a simple random sample of n = 20
# faculty selected from N = 150, compute the specified statistics.

salaries <- c(
  103,
  55,
  61,
  77,
  58,
  60,
  57,
  64,
  57,
  79,
  52,
  39,
  46,
  55,
  29,
  40,
  47,
  103,
  56,
  118
)

# 1) The sum ------------------------------------------------------------------
sum(salaries) # 1256

# 2) The mean -----------------------------------------------------------------
mean_salary <- mean(salaries) 
mean_salary # 62.8

# 3) The sampling fraction n/N ------------------------------------------------
f <- 20/150 
f # 0.1333333

# 4) The finite population correction -----------------------------------------

1 - f # 0.8666667

# 5) The sum of squared deviations --------------------------------------------

squared_deviations <- purrr::map(
  salaries,
  \(yi) {
    (yi - mean_salary)^2
  }
) |> unlist()

sum_squared_deviations <- sum(squared_deviations)
sum_squared_deviations # 9891.2

# 6. The sample variance ------------------------------------------------------

(1/(20 - 1)) * sum_squared_deviations # 520.5895
s2 <- var(salaries) 
s2 # 520.5895

# 7. The sampling variance of the mean ----------------------------------------

sampling_variance <- (1 - f) * s2 / 20
sampling_variance # 22.55888

# 8. The standard error og the mean -------------------------------------------

se <- sqrt(sampling_variance) 
se # 4.749619

# 9. The margin of error (95% confidence rounded) -----------------------------

2 * se # 9.499237

# 10. We know for the population of faculty that S2 is 735, and we wanted a
# standard error of 4 (to give a margin of error of 2 x $4 = $8), what simple
# random sample size would be needed?

S2 <- 735
e <- 8

n_prim <- S2 / (e / 2)^2
n_prim # 45.9375

n <- n_prim / (1 + (n_prim / 150))
n # 35.16746
