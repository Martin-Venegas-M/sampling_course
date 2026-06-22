#******************************************************************************
# 0. Identification -----------------------------------------------------------
# Title: Selecting Schools - Assignment 2 Individual
# Institution: Sampling Course
# Responsable: MVM
# Executive Summary: This script...
# Date: June wq, 2026
#******************************************************************************

rm(list = ls())

# 1. Load packages ------------------------------------------------------------

c("tidyverse", "here", "janitor", "glue") |>
  purrr::walk(library, character.only = TRUE)

###############################################################################
############## Sampling People, Networks and Records ##########################
#################### Week 3 Indiviual Assignment ##############################
###############################################################################

# Consider the list of middle and high schools from a rural area in the 
# United States. Using the table of random digits, select a = 4 schools with 
# probability proportionate to size, using a systematic sampling technique. 

# Please enter the following.

# 1. the sampling interval

a <- 4
N <- 27466
k <- N/a

# 27.466/4 = 6866.5

# 2. the random start

rs <- 1943
# 1943

# 3. how the random start was determined

# From left to right and from up to down, first column of 5 digits random
# number that is below N.

a1 <- rs # 1943
a2 <- k + rs # 8810
a3 <- a2 + k # 15676
a4 <- a3 + k # 22543

# 4. the selected schools

# a1: CHASSELL K-12 SCHOOL
# a2: WEST IRON COUNTY HIGH	SCH
# a3: NATIONAL MINE SCHOOL
# a4: STEPHENSON HIGH	SCHOOL

# 5. the cumulative size for each selected school

# a1: 2123
# a2: 8913
# a3: 15769
# a4: 22661