library(rsconnect)

tmp.enc <- options()$encoding
options(encoding = "UTF-8")
deployApp(appFiles = c("app.R", "kommuner_2020_simplest_2.rds", "fylker_2020_simplest_2.rds"), forceUpdate = TRUE)
options(encoding = tmp.enc)

