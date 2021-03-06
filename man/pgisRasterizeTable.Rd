% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rasters.R
\name{pgisRasterizeTable}
\alias{pgisRasterizeTable}
\title{Rasterize a PostGIS vector table}
\usage{
pgisRasterizeTable(conn, tbl, name, xref = NULL, yref = NULL, scale = 1000)
}
\arguments{
\item{conn}{a PostgreSQLConnection object.}

\item{tbl}{character; name of the input vector table to rasterize.}

\item{name}{character; name of the output raster table.}

\item{xref}{numeric; x coordinate of reference pixel bottomleft corner.
If NULL (default), the center of the extent is taken.}

\item{yref}{numeric; y coordinate of reference pixel bottomleft corner.
If NULL (default), the center of the extent is taken.}

\item{scale}{numeric; size of the raster pixels, in meters (default: 1000).}
}
\description{
Rasterize a PostGIS vector table.
}
\examples{
library(spData)
library(RPostgreSQL)
library(sf)
data(seine)
Sys.sleep(10)
conn = dbConnect(drv=dbDriver("PostgreSQL"), host="localhost", port=5432,
                dbname="foodflows", user="milo", password="postgres")
sf::dbWriteTable(conn, name="seine", seine, overwrite=TRUE)
Sys.sleep(10)
pgisRasterizeTable(conn, "seine", "seine_rast")
Sys.sleep(10)
rast = pgisGetRaster(conn, "seine_rast", flip=TRUE)
raster::plot(rast)

}
\references{
https://nronnei.github.io/blog/2017/03/creating-rasters-from-scratch-in-postgis/
http://geospatialelucubrations.blogspot.com/2014/05/a-guide-to-rasterization-of-vector.html
}
