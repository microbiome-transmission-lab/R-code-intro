#####################################################
#####################################################
# MOTION Lab R Code Club Session 2; 7/30/24
# Presented by: Laura Cowden
# Data Wrangling and Cleaning
#####################################################
#####################################################

################################################################
# Objectives:
#     - introduce some basic data cleaning/wrangling functions
#     - show some examples of where these are helpful in our lab
################################################################

################################################################
# Helpful tools and resources:
#     - R for Data Science, Chapter 3: https://r4ds.hadley.nz/data-transform
#     - Stack Overflow: https://stackoverflow.com/
################################################################


# Some considerations when producing and recording data:
#     - be consistent
#     - in excel, consider using drop-downs for binary variables and beyond
#     - capitalization matters
#     - consider routinely checking excel data for typos
#         - I format my data as a table and look at unique entries 

# Example dataset

# Tidyverse
# An R programming package that contains functions designed to help transform and 
# better present data for downstream manipulation, analysis, and the creation of 
# visualizations.

# Core Packages in the tidyverse:
#     - ggplot2: creates graphics/visualizations
#     - dplyr: contains functions for data manipulation
#     - lubridate: allows for easy manipulation of dates/times
#     - readr: provides a way to read rectangular data into R (csv, tsv, fwf)
#     - purrr: provides several functions to map, filter and reduce data with complex structure
#
# Janitor Package
# https://cran.r-project.org/web/packages/janitor/vignettes/janitor.html
# An R package that contains helpful functions for data cleaning and exploration
#
# 
# There are also a lot of great functions in base R


# I needed to update R on my laptop; not necessary for this session, but I thought I'd share
# You can check the version you're working with in the console

# install.packages("installr")
# library(installr)
# updateR()

# Install necessary packages
install.packages("tidyverse") 
install.packages("readxl") # provides functions to read excel sheets
install.packages("janitor") # provides functions to examine and clean data

# Load packages to make them available in your current R session
library(tidyverse)
library(readxl)
library(janitor)

# Set working directory
setwd("H:/R files/codeclub_session2")

##################################################
# Scenario 1 
# Ebb wants to know how many surveillance isolates 
# were collected in 2023
##################################################

# INTEGRATE Dataset
# This dataset contains several tabs that include
# a list of all isolates collected in Kindred hospitals (CA), patient and hospital 
# information and antibiotic sensitivity data that was collected in-house

# Load data and commit to environment
?read_excel
integrate_original <- read_excel(path = "INTEGRATE Isolate Master List_LC_codeclub.xlsx", sheet = 1 , col_names = TRUE)

# Take a look at the resulting dataframe
# Lets get rid of capitalization and spaces in our column names
integrate <- integrate_original %>%
  clean_names()

# There's some nonsense in this spreadsheet that was used for something or other, but I don't
# need it for my analysis. 
# You can delete specific columns that you don't care about (or keep only ones you do) using select (dplyr)
integrate <- subset(integrate, select = -c(12, 13, 14, 15, 16))

# Notice the collect date column in our dataframe; this is called an "origin date" which is the number
# of days pased the origin date of Dec 30, 1899 in excel. We can correct this using as.Date (base R)

# To convert the origin date to YYYY/MM/DD format, first we need to convert collect_date from a
# character to numeric value using as.numeric (base R)

integrate$collect_date <- as.numeric(integrate$collect_date)

# then use as.Date function, specifying the origin date

integrate$collect_date <- as.Date(integrate$collect_date, origin = "1899-12-30")

# I am wanting to sort based on year, so I'm parsing YYYY/MM/DD into separate columns using lubridate

integrate$year <- year(ymd(integrate$collect_date))
integrate$month <- month(ymd(integrate$collect_date))
integrate$day <- day(ymd(integrate$collect_date))

# Now I can answer Ebb's question using functions from dplyr

integrate %>%
  group_by(year, culture_type) %>% 
  summarise(n=n())

# or 

table(integrate$year, integrate$culture_type)
# notice that this code won't give you data for NAs unless you tell it to
table(integrate$year, integrate$culture_type, useNA = "ifany")

# I don't know why my collect_date column isn't being read uniformly, and I looked
# into other potential code, but the quickest way to rectify this was to just read it 
# as a .csv file.

integrate_original_csv <- read.csv("INTEGRATE Isolate Master List_LC_codeclub.csv")

# Ebb then asks Leigh to do an analysis on just the surveillance culture isolates
# from 2023. We can use the filter function to create a new dataframe that just
# includes relevant isolates

integrate_2023_SC <- filter(integrate, year == 2023, culture_type == "SC")

# How many isolates came from La Mirada hospital?

filter(integrate, hospital_location == "La Mirada")

# This creates a tibble, and doesn't assign anything to a dataframe. You can check
# the dimensions of the tibble to get this answer

##################################################
# Scenario 2
# Ebb wants to know how many INTEGRATE isolates
# are resistant to colistin
##################################################

integrate.mics.original <- read_excel("INTEGRATE Isolate Master List_LC_codeclub.xlsx", sheet = "Isolate MICs")

# this sheet needs tidied. There are columns with data that aren't actually part 
# of the dataset. Drop columns you don't care about using subset() function

integrate.mics <- subset(integrate.mics.original, select = -c(16,19,20,21,22)) %>%
  clean_names()

# now we are left with only relevant information

# for the sake of consistency, let's change the data such that it only has MIC
# values under each drug. eg. instead of "R" under C/T, let's put "8"

# I like to use base R for this
integrate.mics$c_t[integrate.mics$c_t == "R"] <- 8
integrate.mics$c_t[integrate.mics$c_t == "S"] <- 0.06
integrate.mics$mev[integrate.mics$mev == "R"] <- 16
integrate.mics$mev[integrate.mics$mev == "S"] <- 0.008
integrate.mics$omc[integrate.mics$omc == "R"] <- 8
integrate.mics$omc[integrate.mics$omc == "S"] <- 0.12
integrate.mics$col[integrate.mics$col == "R"] <- 4
integrate.mics$col[integrate.mics$col == "S"] <- 0.25
integrate.mics$plz[integrate.mics$plz == "R"] <- 4
integrate.mics$plz[integrate.mics$plz == "S"] <- 0.12
integrate.mics$imi[integrate.mics$imi == "R"] <- 2
integrate.mics$imi[integrate.mics$imi == "S"] <- 16
# integrate.mics$'fos+'[integrate.mics$'fos+' == "R"] <- 128 # E.coli only
# integrate.mics$'fos+'[integrate.mics$'fos+' == "S"] <- 64 # E. coli only
integrate.mics$dlx[integrate.mics$dlx == "R"] <- 0.12
integrate.mics$dlx[integrate.mics$dlx == "S"] <- 1
integrate.mics$cza[integrate.mics$cza == "R"] <- 32
integrate.mics$cza[integrate.mics$cza == "S"] <- 0.12
integrate.mics$mero[integrate.mics$mero == "R"] <- 8
integrate.mics$mero[integrate.mics$mero == "S"] <- 0.25
integrate.mics$erv[integrate.mics$erv == "R"] <- 8
integrate.mics$erv[integrate.mics$erv == "S"] <- 0.03
integrate.mics$imr[integrate.mics$imr == "R"] <- 16
integrate.mics$imr[integrate.mics$imr == "S"] <- 0.03

# how many isolates are resistant to colistin?
# dplyr package count() function would work, but it requires a numeric variable

is.numeric(integrate.mics$col) #FALSE

# try base R without having to change variable type
length(which(integrate.mics$col >= 4))

# 282 isolates are resistant to colistin
# what percentage of our isolates is that?
length(na.omit(integrate.mics$col))
282/1515 # 18.61%

# Ebb now wants to know which isolates are resistant all beta-lactamase inhibitor containing drugs:
# cefazidime-avibactam, ceftazolane-tazobactam and imipenem-relebactam

unique(integrate.mics$c_t)
unique(integrate.mics$cza)
unique(integrate.mics$imr)
      
ext_spec <- integrate.mics %>%
  select(isolate, c_t, cza, imr) %>%
  mutate(extended_spectrum = case_when(
    c_t %in% c("2/4", "4/4", "8/4", "8") &
      cza %in% c("16/4", "32/4") &          
      imr %in% c("2", "2/4", "4/4", "8/4", "16/4", "4") ~ "pan-resistant", 
    TRUE ~ "susceptible"))

table(ext_spec$extended_spectrum)


##################################################
# Scenario 3
# A collaborator sent me a list of IMPALA isolates that 
# they would like me to send for long read sequencing 
##################################################

# load the list the collaborator provided
longread <- read_excel("impala_longreadiso_selection_codeclub.xlsx")

# load the data that contains freezer location information
locations <- read_excel("IMPALA_masterlist_codeclub.xlsx")

## Combining Datasets ##
# to combine datasets, I will first pick my "key." I will be using the IMPALA
# isolate identifier as my key, but this variable is called different things 
# in longread and locations, so I will change one to match the other.
colnames(locations) #ISOLATE NUMBERS
colnames(longread) #Sample ID

locations <- locations %>%
  rename(
    "Sample ID" = "ISOLATE NUMBERS")

# both datasets now have the isolate identifiers in the column "Sample ID."
# I will now use a join function to combine these datasets. Left_join(X,y) 
# keeps all values in X, where X will contain our Sample ID

impala_longread_locations <- left_join(longread,locations, by = "Sample ID")

# write this dataframe to an .csv file so that others can use it who may 
# not use R, or for me to print when I go looking for these isolates

write.csv(impala_longread_locations,"Y:/R files/impala_longread_locations.csv")

# the .csv file was written into the folder specified

## This also can be done with LabVantage reports! ##


##################################################
# Scenario 4
# Brendan wants me to prepare a report on the CRE 
# isolates I've been collecting for the past 6 years,
# but I also have a feeling he's going to want some
# revisited, so I'm going to collect location info 
##################################################

# load CRE labvantage report
cre.lv.original <- read_excel("IDSampleByStudy_lcowden_Jul-24-2024-09-36-55.xlsx")
cre.lv <- clean_names(cre.lv.original)

# fix the origin date by again, first converting the variable to numeric
#cre.lv$collection_date <- as.numeric(cre.lv$collection_date)

# then use as.Date function, specifying the origin date
#cre.lv$collection_date <- as.Date(cre.lv$collection_date, origin = "1899-12-30")

# Since I'm going to also load a spreadsheet that also contains "collection dates" 
# (those obtained from cerner), I'm going to change the name of the collect date column
# to be more specific 

cre.lv <- cre.lv %>%
  rename("lv_collection_date" = "collection_date")

# load my CRE spreadsheet that contains PHI and LabVantage info
cre.data.original <-read_excel("CRE_isolates_codeclub.xlsx")
cre.data <- clean_names(cre.data.original)

# I want to combine these datasets to include everything that is in cre.data.
# first, I'll select a key, and make sure the column name matches in both datasets

cre.lv <- cre.lv %>%
 rename(
   "isolate_id" = "external_participant_id")

# I'm going to combine cre.lv and cre.data using left join 
cre.all <- left_join(cre.lv,cre.data, by = "isolate_id")

# How many of each species collected do I have banked?
species <- cre.all %>%
  group_by(species)%>%
  summarise(n=n())

# How many of those collected have enzyme data?
cre.data %>%
  group_by(enzyme_detected) %>%
  summarise(n=n())




