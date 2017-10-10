# Creating a dataset in PostgreSQL from R

library(RPostgreSQL)
p_word <- {
  "postgres123"
}

databaseDriver <- dbDriver("PostgreSQL")
con2DB <- dbConnect(databaseDriver, dbname = "postgres",
                    host = "localhost", port = 5432,
                    user = "postgres", password = p_word)
dbExistsTable(con2DB, "airlineDB")

dat1 <- read.csv('USAairlineData2016.csv', header = T, as.is = T)
dat1 <- dat1[c("YEAR", "QUARTER", "MONTH", "ORIGIN", "DEST", "DEP_DELAY", "ARR_DELAY")]
dat1$ROW_ID <- 1:nrow(dat1)
head(dat1)

dbWriteTable(con2DB, "airlineTBL", 
             value = dat1, append = TRUE, row.names = FALSE)
dat2 <- dbGetQuery(con2DB, 'SELECT * FROM "airlineTBL"')

head(dat2)
head(dat1)

dbDisconnect(con2DB)
dbUnloadDriver(databaseDriver)

# Interacting PostgreSQL database from R
library(RPostgreSQL)
databaseDriver <- dbDriver("PostgreSQL")
con2DB <- dbConnect(databaseDriver, dbname = "postgres",
                    host = "localhost", port = 5432,
                    user = "postgres", password = "joy@pidvs")
uniqueOrigin <- dbGetQuery(con2DB, 'SELECT "ORIGIN", COUNT(*) AS freq FROM "airlineDB" GROUP BY "ORIGIN"')
negDelay <- dbGetQuery(con2DB, 'SELECT * FROM "airlineDB" WHERE "DEP_DELAY"<0')
avgDelay <- dbGetQuery(con2DB, 'SELECT "ORIGIN", avg("DEP_DELAY") AS avgDelay FROM "airlineDB" GROUP BY "ORIGIN"')

# Creating and interacting with SQLite database from R

library(RSQLite)
dbDriverSQLite <- dbDriver("SQLite")
dbNew <- dbConnect(drv = dbDriverSQLite, dbname="testdbSQL")
carTable <- dbWriteTable(con =  dbNew, name = "cardata", value = mtcars)
dbListTables(dbNew)   
autocars <- dbGetQuery(conn = dbNew, 'SELECT * FROM "cardata" WHERE "am"==0')
head(autocars)
dbDisconnect(dbNew)
dbUnloadDriver(dbDriverSQLite)

library(sqldf)
usaAirlines100 <- read.csv.sql("2016_01.csv", sql = "select * from file order by random(*) limit 100")


