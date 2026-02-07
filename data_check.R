library(haven)
library(this.path)
library(tidyverse)

data_check <- function(filepath){
  r_data <- read_csv(paste0(dirname(this.path()), "/", filepath, ".csv"))
  stata_data <- read_dta(paste0(dirname(this.path()), "/", filepath, ".dta")) |>
    relocate(names(r_data))
    # mutate(across(everything(), ~ ifelse(.x == "", NA, .x)))
  all.equal(r_data, stata_data, check.attributes = FALSE)
}

data_check("0_Inputs/NA_Compustat_Annual/raw/funda")

data_check("0_Inputs/NA_Compustat_Annual/raw/company")

data_check("Temp/naics2bea")
# TRUE

data_check("Temp/bea2industry")
# TRUE

data_check("0_Inputs/Fred/loaded/fred_data")
# lengths differ

data_check("0_Inputs/US_BEA_Main/loaded/BEA_industry_raw")
# miniscule difference

data_check("Temp/BEA_mapped")
# miniscule difference

data_check("0_Inputs/NA_Compustat_Annual/raw/crsp_msf")
# different lengths but otherwise seems OK

data_check("0_Inputs/NA_Compustat_Annual/raw/ccmxpf_linktable")
# some issues but don't know if it matters

data_check("0_Inputs/NA_Compustat_Annual/loaded/NA_compustat")

data_check("3_Final_Data/main_dataset_firm")
# seems alright?

data_check("Temp/tempanalysis_stars")

