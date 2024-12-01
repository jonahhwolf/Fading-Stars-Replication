library(RPostgres)
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='jonahwolf')

res <- dbSendQuery(wrds, "SELECT gvkey, loc, dlrsn, ipodate, sic, naics FROM comp_na_daily_all.company")
company <- dbFetch(res, n = 10)
dbClearResult(res)

res <- dbSendQuery(wrds, "SELECT gvkey, consol, indfmt, datafmt, popsrc, curcd, fyear, datadate, conm, at, cogs, csho, dlc, dltt, emp, ib, pstk, sale, xlr, xsga, fic, prcc_f, prcc_c, oiadp, fyr FROM comp_na_daily_all.funda")
funda <- dbFetch(res, n = 10)
dbClearResult(res)
funda