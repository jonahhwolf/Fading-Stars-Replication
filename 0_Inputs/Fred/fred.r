# clear all 
# set more off 
# set logtype text
# tempfile tfile

library(fredr)
library(tidyverse)
library(data.table)
library(this.path)

setwd(dirname(this.path()))

# *************** DESCRIPTION ******************************************
# * Download required macro-data from FRED
# *
# * Version 1.0
# * Last edit: 1/12/2018 
# **********************************************************************
# 
# **** Monthly data
# * CE16OV: Civilian Employment Level, Thousands, Monthly
# freduse CE16OV , clear
# g year = year(daten)
# g month = month(daten)
# keep if month == 12 
# sort year month
# rename CE16OV emp_civil
# replace emp_civil = emp_civil/1000
# lab var emp_civil  "Civilian Employment (Millions)"
# drop date daten month 
# saveold `tfile', replace

emp_civil <- fredr("CE16OV", frequency = "a", aggregation_method = "eop") |>
  select(date, value) |>
  rename(emp_civil = value)

# *
# 
# **** Annual data
# * GDPA:	GDP, Annual, Billions of Dollars, Not Seasonally Adjusted
# freduse  GDPA , clear 
# g year = year(daten)
# drop date daten
# rename GDPA y
# lab var y 		"GDP (Billions)"
 
GDPA <- fredr("GDPA") |>
  select(date, value) |>
  rename(GDPA = value)

# *
# 
# **** Merge
# merge 1:1 year using `tfile', nogen
# order year 
# sort year
# compress
# keep if year >= 1960
# save Fred/loaded/fred_data, replace

fred_data <- left_join(emp_civil, GDPA, by = "date") |>
  filter(year(date) >= 1960)

fwrite(fred_data, "loaded/fred_data.csv")