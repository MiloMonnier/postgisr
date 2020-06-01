# WRITE GRAPH IN POSTGIS -------------------------------------------------------$
# source("R/igraph2sf.R")

# Write V in a 'vertices' table, and E in 'edges' table

# Write graph vertices in a table "vertices", and edges in a table "edges".
# Edges table take roads as geometry, extracted from roads table
#' Title
#'
#' @param conn a PostgreSQLConnection object.
#' @param what character vector; what must be written into DB: vertices and/or
#' edges ? Default: the 2.
#' @param coords names of the numeric columns holding coordinates in vertices
#' dataframe (default: c('x','y')).
#' @param crs numeric(4); EPSG code of the Coordinate Reference System.
#' @param geom.col character; name of the geometry column in output PostGIS table
#' (default: 'geometry').
#' @param disconnect boolean; shall the connection be closed at the end ? (default: TRUE)
#'
#' @importFrom igraph get.data.frame
#' @importFrom RPostgreSQL dbSendQuery
#' @importFrom RPostgreSQL dbDisconnect
#' @importFrom sf st_as_sf
#' @importFrom sf dbWriteTable
#' @export
#'
pgisWriteGraph = function(conn,
                          what       = c("vertices","edges"),
                          coords     = c("x","y"),
                          crs        = 2154,
                          geom.col   = "geometry",
                          disconnect = TRUE)
{
  if ("vertices" %in% what) {
    v     = st_as_sf(v, coords=coords, crs=crs)
    v$x   = v$x
    v$y   = v$y
    v$lng = v$lng
    v$lat = v$lat
    v     = v[, c(colnames(v), "geometry")]
    sf::dbWriteTable(conn, name="vertices", v)
  }

  if ("edges" %in% what) {
    edges = get.data.frame(g, "edges")
    dbSendQuery(conn, "DROP TABLE IF EXISTS edges CASCADE;")
    sf::dbWriteTable(conn, "edges", edges, overwrite=TRUE)  # Import raw table without geometry
    q = paste0("
        ALTER TABLE edges ADD COLUMN IF NOT EXISTS geometry geometry(LINESTRING, ", crs, ");
        UPDATE edges SET geometry = roads.", geom.col," FROM roads WHERE roads.name = edges.name;
      ")
    dbSendQuery(conn, q)    # Join geometry of already stored big roads table
    dbSendQuery(conn, "CREATE INDEX ON edges USING gist(geometry);")
    if (disconect)
      dbDisconnect(conn)
  }
}
# WARNING: If makevalid=TRUE, reveals geometry incompatibility (Roads = point, Edges=linestring)

