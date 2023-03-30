read.dt <- function(astring, type =c('table_name', 'sqlquery')) {
  sqllite_connection <- dbConnect(RSQLite::SQLite(), 'hh_survey.db')
  if (type == 'table_name') {
    dtelm <- dbReadTable(sqllite_connection, SQL(astring))
  } else {
    dtelm <- dbGetQuery(sqllite_connection, SQL(astring))
  }
  dbDisconnect(sqllite_connection)
  setDT(dtelm)
}