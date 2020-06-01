
#' Write a raster into PostGIS
#'
#' Import a RasterLayer object into a PostGIS database.
#'
#' @param rast RasterLayer object.
#' @param tbl character; name of the table created.
#' @param dbname character; name of the database.
#' @param crs numeric(4); EPSG code of the Coordinate Reference System.
#'
#' @references
#' https://postgis.net/docs/using_raster_dataman.html
#'
#' @export
#'
#' @importFrom raster writeRaster
#' @examples
#' library(spData)
#' data(elev)
#' pgisWriteRaster(elev, dbname="foodflows", crs=2154)
#'
pgisWriteRaster = function(rast,
                           tbl,
                           dbname,
                           crs)
{
  # Disable warnings temporarily
  oldw = getOption("warn")
  options(warn=-1)
  # Check raster class
  if (!is(rast, "RasterLayer"))
    stop("rast require a RasterLayer object")
  # Write raster into a temporary file
  file = "/tmp/raster.tif"
  writeRaster(rast, file, overwrite=TRUE)
  # Use the variable name of rast if not passed
  if (missing(tbl))
    tbl = deparse(substitute(rast))
  # Import it into postgis
  # I: index; C: apply raster constraints (srid, pixelsize etc)
  # M: vaccum; d: drop table and create new
  system2(
    "raster2pgsql",
    args = c("-I -C -M",
             "-s", crs,
             "-d ", file,
             tbl, "| psql -d", dbname),
    stdout = FALSE,
    stderr = FALSE
  )
  options(warn=oldw) # Restore warnings
}
# TODO: Manage nodata ?


#' Get raster from PostGIS
#'
#' Get raster table from a PostGIS database. The raster can be verticaly flipped
#' if necessary, if a Y axis bug in rdgal occurs.
#'
#' @param conn a PostgreSQLConnection object.
#' @param tbl character; name of the raster table.
#' @param flip boolean; flip raster Y axis if necessary (default: FALSE).
#' @param password character; password for database (default: 'postgres').
#'
#' @return a RasterLayer object.
#' @export
#'
#' @importFrom rgdal readGDAL
#' @importFrom raster raster
#' @importFrom sp flipVertical
#' @importFrom RPostgreSQL dbGetInfo
#' @examples
#' library(RPostgreSQL)
#' library(spData)
#' library(raster)
#' data(elev)
#' pgisWriteRaster(elev, dbname="foodflows", crs=2154)
#' conn = dbConnect(drv=dbDriver("PostgreSQL"), host="localhost", port=5432,
#'                 dbname="foodflows", user="milo", password="postgres")
#' rast = pgisGetRaster(conn, tbl="elev")
#' raster::plot(rast)
#'
pgisGetRaster = function(conn,
                         tbl,
                         flip = FALSE,
                         password = "postgres")
{
  # Disable warnings temporarily
  oldw = getOption("warn")
  options(warn=-1)

  if (!is(conn, "PostgreSQLConnection"))
    stop("'conn' must be connection object: <PostgreSQLConnection>")
  # Retrieve DB connection identifiers
  dbinfos = dbGetInfo(conn)
  dbname = dbinfos$dbname
  host   = dbinfos$host
  user   = dbinfos$user
  port   = dbinfos$port
  # Import raster from PostGIS
  password = "postgres"
  dsn = paste0(
    "PG:",
    "dbname='",dbname,"' ",
    "host='",host,"' ",
    "port=",port," ",
    "user='",user,"' ",
    "password='",password,"' ",
    "table='", tbl,"'"
  )
  # dsn = paste0("PG:dbname='",dbname,"' host=localhost user='",user,"' password='",password,"' port=5432 schema='public' table='",tbl,"'") # mode=2
  rast = rgdal::readGDAL(dsn, silent=TRUE)
  if (flip)   # Y axis can be flipped
    rast = flipVertical(rast)
  rast = raster(rast, 1)    # Convert 1st band to raster
  options(warn=oldw) # Restore warnings
  return(rast)
}



#' Create a raster into PostGIS
#'
#' Creates into PostGIS database a raster covering a given extent, with a given
#' pixel size (scale).
#'
#' @param conn a PostgreSQLConnection object.
#' @param tbl character; name of the created table (default: rastertest).
#' @param extent spatial extent covered by the raster. Can be either a bbox class
#' objet, a sf object, or the name of a PostGIS table (default: NULL).
#' @param scale numeric; size of the raster pixels, in meters (default: 1000).
#' @param disconnect boolean; shall the connection be closed at the end ? (default: TRUE)
#'
#' @export
#'
#' @importFrom methods is
#' @importFrom sf st_bbox
#' @importFrom RPostgreSQL dbSendQuery
#' @importFrom RPostgreSQL dbDisconnect
#' @examples
#' library(spData)
#' library(sf)
#' library(RPostgreSQL)
#' library(raster)
#' data(seine) # CRS is in meters
#' # Create a raster of 5km resolution pixel covering french seine river
#' conn = dbConnect(drv=dbDriver("PostgreSQL"), host="localhost", port=5432,
#'                 dbname="foodflows", user="milo", password="postgres")
#' pgisCreateRaster(conn, "rastertest", extent=seine, scale=5000, disconnect=FALSE)
#' # Check
#' rast = pgisGetRaster(conn, "rastertest")
#' dbDisconnect(conn)
#' values(rast) = rnorm(ncell(rast))
#' plot(rast)
#' plot(st_geometry(seine), add=TRUE)
#'
pgisCreateRaster = function(conn,
                            tbl,
                            extent,
                            scale      = 1000,
                            disconnect = TRUE)
{
  if (!is(conn, "PostgreSQLConnection"))
    stop("'conn' must be connection object: <PostgreSQLConnection>")

  if (missing(extent)) {
    stop("extent argument require a bbox, sf, or PostGIS table name")
  } else if (is(extent, "bbox")) {
    bbox = extent
  } else if (is(extent, "sf")) {
    bbox = st_bbox(extent)
  } else if (is(extent, "character")) {
    bbox =  pgisGetBbox(extent)
  }
  # Fit new extent to the given object
  xmin = bbox["xmin"]
  ymin = bbox["ymin"]
  xmax = bbox["xmax"]
  ymax = bbox["ymax"]
  xext = xmax - xmin                  # Get x and y extent
  yext = ymax - ymin
  width = ceiling(xext/scale)         # Nb of necessary cells
  height = ceiling(yext/scale)
  newxext = width * scale             # Enlarge extent with superior nb of cells needed
  newyext = height * scale
  xmin = xmin - (newxext - xext) / 2  # Compute new min and max
  ymin = ymin - (newyext - yext) / 2
  # Create the raster in PostGIS database
  q = paste0("
        DROP TABLE IF EXISTS ", tbl," CASCADE;
        CREATE TABLE ", tbl,"(rid SERIAL primary key, rast raster);
        INSERT INTO ", tbl," (rast)
        SELECT ST_AddBand(
                  ST_MakeEmptyRaster(", width,",", height,",", xmin,",", ymin,", ", scale,", ", scale,", 0, 0, 2154),
                  '8BSI'::text, 1, 0
                ) AS rast;
        ")
  dbSendQuery(conn, q)
  if (disconnect)
    dbDisconnect(conn)
}


#' Rasterize a PostGIS vector table
#'
#' Rasterize a PostGIS vector table.
#'
#' @param conn a PostgreSQLConnection object.
#' @param geom.tbl character; name of the input vector table
#' @param rast.tbl character; name of the output raster table
#' @param xref numeric; x coordinate of reference pixel bottomleft corner.
#' If NULL (default), the center of the extent is taken.
#' @param yref numeric; y coordinate of reference pixel bottomleft corner.
#' If NULL (default), the center of the extent is taken.
#' @param scale numeric; size of the raster pixels, in meters (default: 1000).
#' @param disconnect boolean; shall the connection be closed at the end ? (default: TRUE)
#'
#' @references
#' https://nronnei.github.io/blog/2017/03/creating-rasters-from-scratch-in-postgis/
#' http://geospatialelucubrations.blogspot.com/2014/05/a-guide-to-rasterization-of-vector.html
#'
#' @export
#'
#' @examples
#' library(spData)
#' library(RPostgreSQL)
#' library(sf)
#' data(seine)
#' conn = dbConnect(drv=dbDriver("PostgreSQL"), host="localhost", port=5432,
#'                 dbname="foodflows", user="milo", password="postgres")
#' sf::dbWriteTable(conn, name="seine", seine, overwrite=TRUE)
#' pgisRasterizeTable(conn, "seine", "seine_rast", disconnect=FALSE)
#' rast = pgisGetRaster(conn, "seine_rast", flip=TRUE)
#' raster::plot(rast)
#'
pgisRasterizeTable = function(conn,
                              geom.tbl,
                              rast.tbl,
                              xref       = NULL,
                              yref       = NULL,
                              scale      = 1000,
                              disconnect = TRUE)
{

  if (!geom.tbl %in% dbListTables(conn))
    stop(paste("Table", geom.tbl, "does not exist"))
  # Set x an y reference if not passed as argument
  if (is.null(xref)) {
    bbox = pgisGetBbox(conn, geom.tbl, disconnect=FALSE)
    xref = mean(bbox["xmin"], bbox["xmax"])
  }
  if (is.null(yref)) {
    bbox = pgisGetBbox(conn, geom.tbl, disconnect=FALSE)
    yref = mean(bbox["ymin"], bbox["ymax"])
  }

  # Create an empty reference raster of 1 pixel
  q = paste0("
          DROP TABLE IF EXISTS tmp_pixelref CASCADE;
          CREATE TABLE tmp_pixelref (rid SERIAL primary key, rast raster);
          INSERT INTO tmp_pixelref (rast)
          SELECT ST_AddBand(
                    ST_MakeEmptyRaster(1, 1,", xref,",", yref,", ", scale,", ", scale,", 0, 0, 2154),
                    '8BSI'::text, 1, 0
                  ) AS rast;
          ")
  dbSendQuery(conn, q)
  # Rasterize geometry using previous raster as reference reference
  q = paste0("
          DROP TABLE IF EXISTS ", rast.tbl, " CASCADE;
          CREATE TABLE ", rast.tbl, " AS
          SELECT ST_Union(ST_AsRaster(geometry, rast, '32BF', '1', -9999)) rast
          FROM ", geom.tbl, ", (SELECT rast FROM tmp_pixelref LIMIT 1) rast;
        ")
  dbSendQuery(conn, q)
  if (disconnect)
    dbDisconnect(conn)
}


#' Convert a PostGIS raster into a polygon grid
#'
#' @param conn a PostgreSQLConnection object.
#' @param rast.tbl character; input raster table name.
#' @param grid.tbl character; output polygon table name.
#' @param exclude.nodata boolean; if TRUE, only those pixels whose values are
#' not NODATA are returned as points (default: TRUE).
#' @param disconnect boolean; shall the connection be closed at the end ? (default: TRUE)
#'
#' @export
#'
#' @importFrom RPostgreSQL dbListTables
#' @importFrom RPostgreSQL dbSendQuery
#' @importFrom RPostgreSQL dbDisconnect
#'
#' @examples
#' library(RPostgreSQL)
#' library(raster)
#' library(spData)
#' library(sf)
#' conn = dbConnect(drv=dbDriver("PostgreSQL"), host="localhost", port=5432,
#'                  dbname="foodflows", user="milo", password="postgres")
#' # Example 1: build a grid based on a simple raster
#' data(elev)
#' pgisWriteRaster(elev, dbname="foodflows", crs=2154)
#' pgisPolygonizeRaster(conn, "elev", "elev_grid", exclude.nodata=TRUE, disconnect=FALSE)
#' rast = pgisGetRaster(conn, tbl="elev")
#' grid = st_read(conn, "elev_grid")
#' plot(rast)
#' plot(st_geometry(grid), border="red", add=TRUE)
#' # Example 2: build a grid convering only a vector shape
#' data(seine)
#' sf::dbWriteTable(conn, "seine", seine, overwrite=TRUE)
#' pgisRasterizeTable(conn, "seine", "seine_rast", scale=10000, disconnect=FALSE)
#' pgisPolygonizeRaster(conn, "seine_rast", "seine_grid", exclude.nodata=TRUE, disconnect=FALSE)
#' rast = pgisGetRaster(conn, tbl="seine_rast", flip=TRUE)
#' grid = st_read(conn, "seine_grid")
#' plot(st_geometry(seine))
#' plot(rast, add=TRUE)
#' plot(st_geometry(grid), border="red", add=TRUE)
#'
pgisPolygonizeRaster = function(conn,
                                rast.tbl,
                                grid.tbl,
                                exclude.nodata = TRUE,
                                disconnect = TRUE)
{
  if (!rast.tbl %in% dbListTables(conn))
    stop(paste("Raster table", rast.tbl, "does not exist"))

  q = paste0("
            DROP TABLE IF EXISTS ",grid.tbl," CASCADE;
            CREATE TABLE ",grid.tbl," (id serial primary key, geometry geometry(POLYGON, 2154));
            CREATE INDEX ON ",grid.tbl," USING gist (geometry);
            INSERT INTO ",grid.tbl," (geometry)
              SELECT geom AS geometry
              FROM (SELECT (ST_PixelAsPolygons(rast, 1, ", exclude.nodata, ")).* FROM ", rast.tbl, ") AS geom;
          ")
  dbSendQuery(conn, q)
  # dbSendQuery(con, paste0("VACUUM ANALYZE ",tbl.out,";"))
  if (disconnect)
    dbDisconnect(conn)
}

