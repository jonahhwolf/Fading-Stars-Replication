library(RPostgres)
library(data.table)
library(tidyverse)

# create first csv
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='jonahwolf')

res <- dbSendQuery(wrds, "SELECT gvkey, loc, dlrsn, ipodate, sic, naics FROM comp_na_daily_all.company")
company <- dbFetch(res, n = 2000)
fwrite(company, "./raw/company.csv")
dbClearResult(res)

# create second csv
res <- dbSendQuery(wrds, "SELECT gvkey, consol, indfmt, datafmt, popsrc, curcd, fyear, datadate, conm, at, cogs, csho, dlc, dltt, emp, ib, pstk, sale, xlr, xsga, fic, prcc_f, prcc_c, oiadp, fyr FROM comp_na_daily_all.funda")
funda <- dbFetch(res, n = 2000)
dbClearResult(res)

filtered_funda <- funda |>
  filter(consol == "C" & 
           indfmt == "INDL" & 
           datafmt == "STD" & 
           popsrc == "D" & 
           curcd == "USD")

fwrite(filtered_funda, "./raw/funda.csv")

# Load CRSP-Compustat Linking Table
res <- dbSendQuery(wrds, "SELECT * FROM crsp_a_ccm.ccmxpf_linktable")
linktable_data <- dbFetch(res, n = 2000)
dbClearResult(res)
fwrite(linktable_data, "./raw/ccmxpf_linktable.csv")

# Load CRSP Price & Shares
res <- dbSendQuery(wrds, "SELECT permno, permco, date, prc, shrout FROM crsp.msf")
crsp_msf <- dbFetch(res, n = 2000)
dbClearResult(res)
fwrite(crsp_msf, "./raw/crsp_msf.csv")

# Close database connection
dbDisconnect(wrds)