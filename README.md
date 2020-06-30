# Copernicus Global Land Service Resampling Tool Using R: A Resample Exercise

The Copernicus Global Land Service ([CGLS](https://land.copernicus.eu/global/)) vegetation-related products (i.e. NDVI, LAI, FAPAR...), based on PROBA-V observations, have been distributed at 1km and 333m until June, 2020. However, as of July, 2020, all Near Real Time (NRT) production of the vegetation biophysical variables, based on Sentinel-3 observations, are no longer provided at 1km resolution. Nonetheless, the users who might be interested on continuing their 1km time series can use a resample of the new 333m products.

The science and production teams of the CGLS, in support to the 1km users, provide different tools to make their own resampling exercises from the new 333m products to a 1km resolution, corresponding to the usual 1km grid. A Notebook with R code and some explanations on how to make a resample can be found [here](https://nbviewer.jupyter.org/github/VITObelgium/notebook-samples/blob/master/datasets/probav/ResampleTool_R_notebook.ipynb).

In this repository some R scripts and a report with results can be found on a comparison made of different resampled products using this R-based tool with the original CGLS products at 1km resolution for different areas. In addition, a comparison with the results obtained with another tool (QGIS/Python-based) is also provided.

### Content:

* NDVI - Europe/North Africa: Resmapled with R-tool vs original 1km product
* NDVI-Europe/North Africa: Resmapled with QGIS/Python-tool vs original 1km product
* NDVI-Europe/North Africa: Resmapled with R-tool vs resmapled with QGIS/Python-tool
* LAI - Europe/North Africa: Resmapled with R-tool vs original 1km product
* LAI - Amazonia: Resmapled with R-tool vs original 1km product


