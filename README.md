## Install

To install the package, use [`remotes`](https://CRAN.R-project.org/package=remotes):

```r
remotes::install_github("MiloMonnier/postgisr")
```

You can find the full documentation of the package [here](https://138.197.187.155/index.php/s/o3ZDyF9KMYzRjoi).



## Tables managment functions

These functions are not specific to PostGIS, but more generic to PostgreSQL. Thus the prefix "pg" is used, instead of "pgis"

* **pgListIndexes**: List all indexes attached to tables of a database.
* **pgDropTables**: Apply a DROP CASCADE to PostgreSQL tables matching a pattern.


## Geometry functions

* **pgisGetEPSG**: get the EPSG code of a PostGIS table. The ESPG defines the Coordinate Reference System (CRS) of the geometry.
* **pgisGetBbox**: get the bounding box of the 
* **pgisMakeValid**: correct PostGIS poloygon tables geometry errors if any (e.g. polygons boundary lines cross-cuts).
* **pgisSimplifyGeom**: wrapper to simplify the geometric complexity of PostGIS vector tables. Indeed, high precision geometries use more memory and lengthen geoprocessing. If we don't need high precision, we can reduce it.


## Raster functions

* **pgisWriteRaster**: import a RasterLayer object into PostGIS.
* **pgisGetRaster**: get a raster table from PostGIS.
* **pgisCreateRaster**: create into PostGIS a raster covering a given extent, with a given resolution.
* **pgisRasterizeTable**: rasterize a PostGIS vector table into a new raster table.
* **pgisPolygonizeRaster**: convert a PostGIS raster table into a new polygon grid table.
