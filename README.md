# Functions

## Tables managment functions

These functions are not specific to PostGIS, but more generic to PostgreSQL. Thus the prefix "pg" is used, instead of "pgis"

* **pgListIndexes**: List all indexes attached to tables of a database.
* **pgDropTables**: Apply a DROP CASCADE to PostgreSQL tables matching a pattern.


## Geometry functions

* **pgisGetEPSG**: get the EPSG code of a PostGIS table. The ESPG defines the Coordinate Reference System (CRS) of the geometry.
* **pgisGetBbox**: get the bounding box of the 
* **pgisMakeValid**: 
* **pgisSimplifyGeom**: 


## Raster functions

* **pgisWriteRaster**: import a RasterLayer object into PostGIS.
* **pgisGetRaster**: get a raster table from PostGIS.
* **pgisCreateRaster**: create into PostGIS a raster covering a given extent, with a given resolution.
* **pgisRasterizeTable**: rasterize a PostGIS vector table into a new raster table.
* **pgisPolygonizeRaster**: convert a PostGIS raster table into a new polygon grid table.

# Installing
