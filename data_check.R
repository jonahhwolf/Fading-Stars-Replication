library(haven)
library(this.path)
library(tidyverse)

data_check <- function(filepath){
  r_data <- read_csv(paste0(dirname(this.path()), "/", filepath, ".csv"))
  stata_data <- read_dta(paste0(dirname(this.path()), "/", filepath, ".dta")) |>
    relocate(names(r_data))
  all.equal(r_data, stata_data, check.attributes = FALSE)
}

data_check("Temp/naics2bea")

data_check("Temp/bea2industry")
# error just a function of how R reads empty values

data_check("0_Inputs/US_BEA_Main/loaded/BEA_industry_raw")
# miniscule difference

data_check("Temp/BEA_mapped")
# seems mostly right

data_check("0_Inputs/NA_Compustat_Annual/raw/crsp_msf")
# different lengths but otherwise seems OK

data_check("0_Inputs/NA_Compustat_Annual/raw/ccmxpf_linktable")
# some issues but don't know if it matters

data_check("3_Final_Data/main_dataset_firm")
# seems alright?

data_check("0_Inputs/NA_Compustat_Annual/loaded/NA_compustat")
# seems OK?

data_check("0_Inputs/NA_Compustat_Annual/raw/funda")

data_check("0_Inputs/NA_Compustat_Annual/raw/company")