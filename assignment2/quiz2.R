#******************************************************************************
# 0. Identification -----------------------------------------------------------
# Title: Cluster Sampling - Quiz 2
# Institution: Sampling Course
# Responsable: MVM
# Executive Summary: This script contains answers for the second quiz
# Date: June 22, 2026
#******************************************************************************

rm(list = ls())

# 1. Load packages ------------------------------------------------------------

c("tidyverse", "here", "janitor", "glue") |>
  purrr::walk(library, character.only = TRUE)

###############################################################################
############## Sampling People, Networks and Records ##########################
#################### Week 3 Quiz: Problem Set #################################
###############################################################################

# From a population of A = 2,500 high schools and N = 500,000 students an epsem
#  sample of students is chosen in a = 50 schools with b = 41 students selected 
# from each school (n = 2,050 students). The fpc may be ignored in all
# calculations.

A <- 2500
N <- 500000
a <- 50
b <- 41
n <- a * b

# 1. If for a characteristic of interest, s2 = 10, what is the simple random 
# sampling variance s2of the mean of the characteristic?

s2 <- 10
var_srs <- s2/n
var_srs # 0.004878049

# 2. If the sampling variance of the same mean is, for the cluster sample, 
# v(y) = 0.01, then what is the design effect of the mean?
var <- 0.01
deff <- var/var_srs

deff # 2.05

# 3. What is the rate of homogeneity roh for this characteristic?

roh <- (deff - 1)/(b - 1)
roh # 0.02625

# 4. Suppose a new sample is to be selected, and the mean of the same 
# characteristic estimated again from the new sample. If the new sample size
# is n = 1,050, selected as a = 50 schools with b = 21 students each, 
# then compute a projected simple random sampling variance, using s2 = 10.

a_new <- 50
b_new <- 21
n_new <- a_new * b_new

var_srs_new <- s2/n_new
var_srs_new # 0.00952381

# 5. Compute a projected design effect using roh from problem 3 above.

deff_new = 1 + (b_new - 1) * roh
deff_new # 1.525

# 6. Compute a projected sampling variance of the mean for the characteristic
# from the new sample

var_new <- var_srs_new * deff_new
var_new # 0.01452381
