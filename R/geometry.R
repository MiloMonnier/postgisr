
#' Get EPSG of a table
#'
#' Get EPSG of a table
#'
#' @param conn a PostgreSQLConnection object.
#' @param tbl character; table name.
#' @param geom.col character; name of the geometry column (default: 'geometry).
#'
#' @return a numeric EPSG code of 4 numbers.
#' @export
#'
#' @importFrom RPostgreSQL dbGetQuery
#'
#' @examples
#' library(spData)
#' library(sf)
#' library(RPostgreSQL)
#' data(seine)
#' conn = dbConnect(drv=dbDriver("PostgreSQL"), host="localhost", port=5432,
#'                 dbname="foodflows", user="postgres", password="postgres")
#' dbWriteTable(conn, name="seine", seine, overwrite=TRUE)
#' (pgisGetEPSG(conn, "seine")==2154)
#' dbDisconnect(conn)
#'
pgisGetEPSG = function(conn,
                       tbl,
                       geom.col = "geometry")
{
  if (!is(conn, "PostgreSQLConnection"))
    stop("'conn' must be connection object: <PostgreSQLConnection>")

  epsg = dbGetQuery(conn, paste0("SELECT ST_SRID(", geom.col, ") FROM ", tbl, " LIMIT 1;"))
  as.numeric(epsg)
}


#' Get bbox of a table
#'
#' Get the extent of the geometry of a PostGIS table, in the same EPSG
#' than table.
#'
#' @param conn a PostgreSQLConnection object.
#' @param tbl character; table name.
#' @param geom.col character; name of the geometry column (default: 'geometry).
#'
#' @return a bbox class object.
#' @export
#'
#' @importFrom RPostgreSQL dbGetQuery
#' @importFrom sf st_bbox
#'
#' @examples
#' library(spData)
#' library(sf)
#' library(RPostgreSQL)
#' data(seine)
#' conn = dbConnect(drv=dbDriver("PostgreSQL"), host="localhost", port=5432,
#'                 dbname="foodflows", user="postgres", password="postgres")
#' dbWriteTable(conn, name="seine", seine, overwrite=TRUE)
#' bb = pgisGetBbox(conn, "seine")
#' (class(bb)=="bbox")
#' dbDisconnect(conn)
#'
pgisGetBbox = function(conn,
                       tbl,
                       geom.col = "geometry")
{
  # Get table geometry extent coordinates
  q = paste0("
        SELECT
          min(ST_XMin(", geom.col, ")) as xmin,
          max(ST_XMax(", geom.col, ")) as xmax,
          min(ST_YMin(", geom.col, ")) as ymin,
          max(ST_YMax(", geom.col, ")) as ymax
        FROM ", tbl, ";
    ")
  ext = dbGetQuery(conn, q)

  # Get the EPSG code of the Coordinate Reference System (CRS)
  q = paste0("SELECT ST_SRID(", geom.col, ") FROM ", tbl, " LIMIT 1;")
  epsg = dbGetQuery(conn, q)

  # Convert the extent coordinates into a sf bbox object
  bbox = c(xmin=ext$xmin,
          xmax=ext$xmax,
          ymin=ext$ymin,
          ymax=ext$ymax)
  st_bbox(bbox, epsg)
}


#' Correct PostGIS table geometry errors
#'
#'  The geometry of a table can be invalid (most of the time because of
#'  intersecting segments), ans causes bugs in geoprocessing. This function
#'  correct these errors if necessary.
#'
#' @param conn a PostgreSQLConnection object.
#' @param tbl character; table name.
#' @param geom.col character; name of the geometry column (default: 'geometry).
#' @param verbose boolean; wheter or not warning when correction are applied (default: TRUE)
#'
#' @export
#'
#' @importFrom RPostgreSQL dbGetQuery
#' @importFrom RPostgreSQL dbSendQuery
#'
pgisMakeValid = function(conn,
                         tbl,
                         geom.col   = "geometry",
                         verbose    = TRUE)
{
  # Correct geometry errors if any
  q =  paste0("SELECT count(*) FROM ", tbl, " WHERE ST_IsValid(", geom.col, ") = FALSE;")
  nb_errors = dbGetQuery(conn, q)
  if (nb_errors > 0) {
    q = paste0("UPDATE ", tbl, " SET ", geom.col, " = ST_MakeValid(", geom.col, ") WHERE ST_IsValid(", geom.col, ") = FALSE;")
    dbSendQuery(conn, q)
    if (verbose)
      message(paste0("Corrected ", nb_errors, " geometry errors"))
  }
}


#' Simplify geometry of a PostGIS table
#'
#' Highly detailed geometries use more memory and make geoprocesses longer.
#' If we don't need high precision, we can simplify (=generalize) it.
#'
#' @param conn a PostgreSQLConnection object.
#' @param tbl character; the table to simplify.
#' @param tolerance numeric; the tolerance in meters CHECK THIS
#' @param geom.col character; the geometry column (default; 'geometry')
#' @param new.geom.col character; the name of the geometry column in which new
#' simplified geometry will be written. If identical to \code{geom.col}, this
#' one is overwritten (default: geometry_simpl).
#' @param verbose boolean; shall the gain of memory be printed ? (default: TRUE).
#'
#' @export
#'
#' @importFrom RPostgreSQL dbGetQuery
#' @importFrom RPostgreSQL dbSendQuery
#'
#' @examples
#' library(spData)
#' library(sf)
#' library(RPostgreSQL)
#' data(seine)
#' conn = dbConnect(drv=dbDriver("PostgreSQL"), host="localhost", port=5432,
#'                dbname="foodflows", user="postgres", password="postgres")
#' dbWriteTable(conn, name="seine", seine, overwrite=TRUE)
#' pgisSimplifyGeom(conn, "seine", tolerance=10)
#' pgisSimplifyGeom(conn, "seine", tolerance=100)
#' pgisSimplifyGeom(conn, "seine", tolerance=1000)
#' dbDisconnect(conn)
#'
pgisSimplifyGeom = function(conn,
                            tbl,
                            tolerance,
                            geom.col     = "geometry",
                            new.geom.col = "geometry_simpl",
                            verbose      = TRUE)
{
  # Get memory size u
  size0 = dbGetQuery(conn, paste0("SELECT SUM(ST_MemSize(", geom.col, ")) FROM ", tbl,";"))
  # Overwrite current geometry column, or store simplified geoemtry in a new column
  if (geom.col==new.geom.col) {
    warning("Overwrite geometry column")

  } else {
    # GGet table EPSG
    epsg = pgisGetEPSG(conn, tbl)
    # Get table geometry type
    q = paste0("SELECT ST_GeometryType(", geom.col, ") FROM ", tbl, " LIMIT 1;")
    geom_type = dbGetQuery(conn, q)
    geom_type = stringr::str_remove(geom_type, "ST_")
    # Create new column
    q = paste0("ALTER TABLE ", tbl, " ADD COLUMN IF NOT EXISTS ", new.geom.col, " geometry('", geom_type, "', ", epsg, ");")
    dbSendQuery(conn, q)
  }
  # Simplify geometry
  q = paste0("UPDATE ", tbl, " SET ", new.geom.col, " = ST_Simplify(", geom.col , ",", tolerance, ");")
  dbSendQuery(conn, q)
  # Get geometry memory size after and print if wanted
  size1 = dbGetQuery(conn, paste0("SELECT SUM(ST_MemSize(", new.geom.col, ")) FROM ", tbl,";"))
  if (verbose) {
    size0 = size0 / 10^6 # b to Mb
    size1 = size1 / 10^6
    rate = (size1-size0)/size0
    rate = abs(round(rate*100, 2))
    print(paste("Geometry size reduced by", rate, "%"))
  }
}
