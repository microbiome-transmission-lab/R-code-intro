##########################################################
##########################################################
# MOTION Lab Summer 2024 Code Camp
# Session I - 6/18/2024
#
# Intro to Programming in R
# Presenter: Leigh Cressman (crel@pennmedicine.upenn.edu)

# Acknowledgment: Some sections of this script were 
# inspired by Dr. Blanca Himes' course, Data Science for 
# Biomedical Informatics (BMIN 503).
##########################################################
##########################################################

########################################################
# Learning objectives:
#
# 1) Understand how to navigate RStudio
# 2) Understand difference between base R and R packages
# 3) Be able to import and export .csv and .xlsx files
# 4) Understand data types in R
# 5) Understand data structures in R
# 6) Understand operators 
# 7) Become familiar with best practices for
#    scientific computing
########################################################


###################
# What is R?
###################

# Language and environment for statistical programming and graphics

# Open source; used in academia and other fields

# Publication-quality graphics can be produced relatively easily

# Designed by Robert Gentleman and Ross Ihaka at
# University of Auckland in 1993

# Implementation of S language (developed at Bell Labs 1975-1976)

# Read more here: https://www.r-project.org/about.html


##################################
# Why can't I just use Excel?
##################################

# we need to be able to reproduce results 
#   - store code in R script file -> gives us record of what we did
#   - share code with other researchers
#   - add comments to explain what code is doing
#   - add notes (to document changes to methods, 
#                describe data provenance, etc.)
# write code once and re-run it many times
# R has many tools for cleaning, wrangling, and merging datasets 
#   so that you do not need to perform these operations by hand


#############################
# 1) Introduction to RStudio
#############################

# integrated development environment (IDE)

# Script: plain text file with .R file extension
#         code is sourced into R console line by line

# Console: can execute commands here and see results of executed commands

# Terminal: provides access to system shell

# Environment: shows variables generated during session

# History: shows all commands used from start of session to present

# Files: shows files and directories available within default workspace

# Plots: shows plots generated during session

# Packages: shows what is currently installed and has interface to
#           install new packages

# Help: retrieve documentation on functions


###################
# 2) Packages in R
###################

# package: collection of R functions, data, and documentation to add 
#          specific functionality

# base packages: installed by default
#                contain basic functions which allow R to work

# see list of attached packages
search()

# the R directories in which packages are stored are called libraries

# main repository for packages is Comprehensive R Archive Network (CRAN)
#   https://cran.r-project.org/
#   Official updates every six months
#   contains over 20,000 packages

# we use specialized packages for data analysis (e.g., tidyverse),
# for generating graphics (e.g., ggplot2), for cleaning files (e.g., janitor),
# as well as others

# only need to install a package once:
install.packages("tidyverse")

# once package is installed, load library into R session
library(tidyverse)


####################################
# 3) importing  and exporting files
####################################

# identify working directory
getwd()

# print list of files in working directory
dir()

# change to a different working directory
setwd("path")

# import .csv file (tidyverse)
patient_list_csv <- read_csv("patient_list_session1.csv")

# import .xlsx file 
install.packages("readxl")
library(readxl)
patient_list_excel <- read_xlsx("patient_list_session1.xlsx")

# export .csv file (tidyverse)
write_csv(patient_list_csv, "patient_list_session1_export.csv")

# export .xlsx file 
install.packages("writexl")
library(writexl)
write_xlsx(patient_list_excel, "patient_list_session1_export.xlsx")


########################################################################
# 4) R data types (classes)
# Read more here: https://www.r-bloggers.com/2022/11/data-types-in-r-3/
########################################################################

# Character: text 

# Numeric: real numbers
  # Double: decimals (default)
  # Integer: whole number (denoted by "L")
   
# Complex: stores numbers with an imaginary component (denoted by "i")

# Logical: TRUE or FALSE

# Raw: stores data as raw bytes (used for binary data)

# patient name
class(patient_list_csv$patient_name)
typeof(patient_list_csv$patient_name)

# patient age
class(patient_list_csv$age)
is.numeric(patient_list_csv$age)
is.integer(patient_list_csv$age)

# number of members in household who are enrolled in the study
class(patient_list_csv$household_member_count)
is.double(patient_list_csv$household_member_count)

# organism identified (NA means no organism)
class(patient_list_csv$maldi_id)
is.numeric(patient_list_csv$maldi_id)

# did the patient have an escre?
class(patient_list_csv$escre)

# change escre to logical because
# this is not a true numeric value; R
# just interprets it that way
patient_list_csv <- patient_list_csv |> 
  mutate(escre_logical = as.logical(escre))


#############################################################
# 5) R data structures 
# Read more here: http://adv-r.had.co.nz/Data-structures.html
#############################################################

# Vectors: basic data structure in R
    # most common types: integer, double (numeric), character, logical
  # Atomic vectors
    #   one dimensional
    #   elements of one class
  # Lists
    #   one dimensional
    #   elements can be of different classes

    # double vector
dbl_vector <- c(3, 6, 7.7)
class(dbl_vector)
is.double(dbl_vector)
is.numeric(dbl_vector)
is.atomic(dbl_vector)

    # list vector
l_vector <- c(1, 7.7, "cat")
class(l_vector)
is.character(l_vector)
is.numeric(l_vector)
is.atomic(l_vector)
is.list(l_vector)


# Lists
#   one dimensional
#   elements can be from different classes
ex_list <- list(2:5, "katze", c("cinnamon", "bun"))
class(ex_list)
is.list(ex_list)
is.atomic(ex_list)


# Factors
#   one dimensional
#   elements represent categorical data with underlying numerical format
#   built on top of integer vectors using
    # class "factor"
    # levels - defines set of allowed values

# sex as character value
sex_char <- c("m", "m", "m")
class(sex_char)
table(sex_char)

# sex as factor value
sex_factor <- factor(sex_char, levels = c("m", "f"))
class(sex_factor)
table(sex_factor)


# Matrices: adding dim attribute to an atomic vector allows it to
#           behave like a multi-dimensional array
#           A special case of the array is the matrix
#   two dimensional
#   elements of one class

# create matrix
num_matrix <- matrix(c(1:6), ncol = 3, nrow = 2)
class(num_matrix)

char_matrix <- matrix(c("dill", "cinnamon bun", "pickled herring", 
                        "lingonberries", "roe", "tiny shrimp"), 
                      ncol = 3, nrow = 2)
class(char_matrix)


# Data frames 
#   two dimensional
#   elements can be of different classes

class(patient_list_csv)
head(patient_list_csv)


##############################################################
# 6) R operators
#    Symbols which represent computations or 
#    actions performed on operands
# note: not exhaustive list
# Read more here: https://www.w3schools.com/r/r_operators.asp
##############################################################

# variable assignment
# R has variables (or objects) which values, functions, and 
# other variables can be assigned to
# different approaches, but we will use <- for this lesson
# keyboard shortcut is "Alt" + "-" 
# R uses = to associate function arguments with values

# 1) standard assignment operator
greeting <- "Hello, World!"
print(greeting)

# 2) also works for assignment but is not favored
# by R programmers
greeting_2 = "Hello, World!"
print(greeting_2)

# 3) can also use this for assignment so that you can
# read the statement from left to right; however,
# it is less common than option 1) above
"Hello, World!" -> greeting_3
print(greeting_3)


# assign values to x and y
x <- 2
y <- 3

# arithmetic operators
x + y # addition
x - y # subtraction
x * y # multiplication
x / y # division
x %% y # modulus division
x %/% y # integer division
x ^ y # exponentiation


# relational operators
x < y # less than
x > y # greater than
x <= y # less than or equal to
x >= y # greater than or equal to
x == y # equal to
x != y # not equal to


# logical operators
# &  AND
# |  OR
# !  NOT

# pipe operator - will cover more in future lessons
# allows you to chain operations together
# takes output from expression on left and passes it 
#   to the first argument of the function on the right

# native R  |> 
# magrittr  %>%
# usage: LHS |> RHS

patient_list_csv |> head()
# equivalent to:
head(patient_list_csv)


# optional: set keyboard shortcut for native pipe operator
# go to "Tools" -> "Global Options" -> "Code"
# check box next to "Use native pipe operator..."
# test by pressing Ctrl + Shift + M (Windows) or
# Cmd + Shift + M (Mac)


############################################
# 7) Best practices in scientific computing
############################################

# include details about data provenance
# add comments that describe code in meaningful way
# organize code
# describe code in plain English
# store code in repository with version control 
# work in small steps - make sure all steps work before combining them
# define global variables once 
# do not copy/paste similar portions of code 
# use meaningful names for variables and functions 
