
#' List indexes of a database
#'
#' List all indexes attached to tables of a database.
#'
#' @param conn a PostgreSQLConnection object.
#' @param tables character vector; tables names from wich to list indexes.
#' If NULL (default), all indexes all returned.
#'
#' @return a dataframe.
#' @export
#'
#' @importFrom methods is
#' @importFrom RPostgreSQL  dbGetQuery
#'
#' @examples
#' library(RPostgreSQL)
#' data(mtcars)
#' conn = dbConnect(drv=dbDriver("PostgreSQL"), host="localhost", port=5432,
#'                 dbname="postgres", user="milo", password="postgres")
#' dbWriteTable(conn, "mtcars", mtcars, overwrite=TRUE)
#' dbSendQuery(conn, "CREATE INDEX IF NOT EXISTS mpg_idx ON mtcars USING btree(mpg);")
#' head(pgListIndexes(conn, "mtcars"))
#' dbDisconnect(conn)
#'
pgListIndexes = function(conn,
                         tables)
{
  if (!is(conn, "PostgreSQLConnection"))
    stop("'conn' must be connection object: <PostgreSQLConnection>")

  if (missing(tables)) {
    q = paste0("
      SELECT tablename, indexname, indexdef FROM pg_indexes
      WHERE schemaname = 'public'
      ORDER BY tablename, indexname;
    ")
  } else {
    tables = paste0("'", paste(tables, collapse="', '"), "'")
    q = paste0("
      SELECT indexname, indexdef FROM pg_indexes
      WHERE tablename IN (", tables, ");
    ")
  }
  dbGetQuery(conn, q)
}


#' Drop tables from a PostgreSQL database
#'
#' Drop tables from a PostgreSQL database matching a pattern with CASCADE.
#'
#' @param conn a PostgreSQLConnection object.
#' @param pattern character; string pattern matching tables names .
#' @param verbose boolean; whether or not drop message must be printed.
#'
#' @references
#' https://stackoverflow.com/questions/4202135/how-to-drop-multiple-tables-in-postgresql-using-a-wildcard
#'
#' @export
#'
#' @importFrom methods is
#' @importFrom stringr str_detect
#' @importFrom RPostgreSQL dbListTables
#' @importFrom RPostgreSQL dbSendQuery
#'
#' @examples
#' library(RPostgreSQL)
#' data(mtcars)
#' conn = dbConnect(drv=dbDriver("PostgreSQL"), host="localhost", port=5432,
#'                 dbname="foodflows", user="milo", password="postgres")
#' dbWriteTable(conn, "mtcars", mtcars, overwrite=TRUE)
#' dbWriteTable(conn, "mtcars2", mtcars, overwrite=TRUE)
#' pgDropTables(conn, "cars") # Will drop the 2
#' dbDisconnect(conn)
#'
pgDropTables = function(conn,
                        pattern,
                        verbose = TRUE)
{
  if (missing(pattern))
    stop("pattern required")
  if (!is(conn, "PostgreSQLConnection"))
    stop("'conn' must be connection object: <PostgreSQLConnection>")

  # Get tables names matching pattern
  tables = dbListTables(conn)
  tables = tables[str_detect(tables, as.character(pattern))]
  if (!length(tables))
    stop(paste0("No table matching pattern '", pattern,"'"))

  q = paste0("DROP TABLE IF EXISTS ", paste(tables, collapse=", ")," CASCADE;")
  dbSendQuery(conn, q)

  if (verbose)
    message(paste("Dropped tables:", paste(tables, collapse=" ")))
}
