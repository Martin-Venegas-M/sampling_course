#******************************************************************************
# 0. Identification -----------------------------------------------------------
# Title: Weights - Final Quiz
# Institution: Sampling Course
# Responsable: MVM
# Executive Summary: This script contains answers for the third quiz
# Date: June 29, 2026
#******************************************************************************

rm(list = ls())

# 1. Load packages ------------------------------------------------------------

c("tidyverse", "janitor", "glue") |>
  purrr::walk(library, character.only = TRUE)

###############################################################################
############## Sampling People, Networks and Records ##########################
#################### Week 6 Final Quiz: Problem Set ###########################
###############################################################################

# Part 1: Background

# A list of residents in a senior independent living complex is divided into 
# two strata, A and B, consisting of NA = 400 and NB = 150 households, respectively.
# A SRS of nA = 4 is selected from stratum A, and nA = 3 from stratum B. The 
# selected senior residents are asked to report (1) the number of visits to a doctor’s 
# office made in the past month and (2) the number of chronic conditions (CC) 
# they have from a list of seven common chronic conditions among seniors. From this, 
# we calculated a weight for each sample person that is the inverse of their 
# probability of selection by computing for stratums A and B, and then assigned 
# their weights (Wgt) accordingly.

N_A <- 400
N_B <- 150
n_a <- 4
n_b <- 3

f_a <- n_a / N_A
f_b <- n_b / N_B

w_a <- 1 / f_a
w_b <- 1 / f_b

sample <- tibble::tibble(
  stratum = c(rep("A", 4), rep("B", 3)),
  id = 1:7,
  n_visits = c(1, 0, 6, 3, 3, 2, 0),
  n_cc = c(3, 2, 3, 2, 1, 2, 1),
  wgt = c(rep(100, 4), rep(50, 3))

)

# Part 1: Problems
# 1. Given the weights provided, compute the weighted mean number of visits to a
# doctor’s office

# Version microdato
with(sample, sum(wgt * n_visits) / sum(wgt)) # 2.272727

# Version analitica
n_visits_a <- 10
n_visits_b <- 5

((n_visits_a * w_a) + (n_visits_b * w_b)) / (n_a * w_a + n_b * w_b) # 2.272727

# 2. Given the weights provided, compute the mean number of chronic conditions
# (CC) across the seven sample cases.

with(sample, sum(wgt * n_cc) / sum(wgt)) # 2.181818

# 3. Calculate the unweighted mean for visits to a doctor’s office. 

mean(sample$n_visits) # 2.142857
with(sample, sum(n_visits) / nrow(sample)) # 2.142857

# 4. Calculate the unweighted mean for chronic conditions (CC). 

mean(sample$n_cc) # 2

# Part 2: Background

# A college registrar office has a list of N = 10,000 student registrations during a
# semester, where a single registration consists of a student enrolling in a course. The
# registrar selects a sample of n = 10 registrations using simple random sampling from
# the list. The student associated with each selected registration is visited by a college
# health worker and interviewed. Data below are reports on the number of courses the
# student is currently registered in and the student’s report of the number, in the last 30
# days, they consumed more than six alcoholic beverages in less than two hours (binge
# drinking episodes). Each student has multiple probabilities of being selected, one for
# each course enrolled.

N <- 10000
n <- 10
f <- n / 10000

sample2 <- tibble::tibble(
  student = 1:10,
  n_course     = c(4, 6, 5, 7, 3, 6, 5, 4, 6, 7),
  n_drinks_eps = c(0, 4, 1, 1, 0, 1, 2, 1, 7, 3),
  wgt = 1 / (n_course * f)
)

sample2

# Part 2: Problems

# Problems 5-14: Calculate a weight for each sample person (each of ten students) that is
# the inverse of their probability of selection. In particular, compute , the number of
# courses or multiplicity for the student times the base probability of selection of a course
# registration, to get a probability of selection for the student, and take the inverse to
# obtain the weight.

sample2$wgt # 250 166.6667 200 142.8571 333.3333 166.6667 200 250 166.6667 142.8571

# 15. Given the weights computed in 5-14, compute the weighted mean number of binge
# drinking episodes,
# The first summation here shouldn’t be from A to B, it should be summed over all the
# weights

with(sample2, sum(wgt * n_drinks_eps) / sum(wgt)) # 1.694575
