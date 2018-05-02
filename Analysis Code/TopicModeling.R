library(needs)
needs(data.table,topicmodels,quanteda,ggplot2,ROracle)
library(ROracle)
drv <- dbDriver("Oracle")


con <- dbConnect(drv, username = "TAB_HR_SVCS", password = "Prd7abhr",
                 dbname = "(DESCRIPTION(ADDRESS=(PROTOCOL=TCP)(HOST=exa01-scan.hgst.com)(PORT=1521))(CONNECT_DATA=(SERVICE_NAME=EPMDWPRD)))")


ROracle::dbGetRowCount(con,schema='HR',"HR_EMPLOYEE_VW")

dbListTables(con,schema='HR')
RODBC::odbcConnect("(DESCRIPTION(ADDRESS=(PROTOCOL=TCP)(HOST=exa01-scan.hgst.com)(PORT=1521))(CONNECT_DATA=(SERVICE_NAME=EPMDWPRD)))",uid ="TAB_HR_SVCS",pwd ="Prd7abhr")
