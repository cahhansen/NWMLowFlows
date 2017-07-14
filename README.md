# NWMLowFlows R Package

This package contains functions that are useful in data pre-processing and evaluation tasks for exploring performance of the National Water Model streamflow data, especially under low-flow and drought conditions. An example of an evaluation workflow is described below:

## Workflow:


***

Function | Function Description
-------- | --------------------
hourly2DailyQ | Reads in hourly NWM streamflow data from a NetCDF format and aggregates to a daily timestep (computes the average). This function may be run on a directory that contains individual hourly files for a single year, and the folder must be named in the format "yyyy" (e.g. 1993). 