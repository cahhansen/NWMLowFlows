# NWMLowFlows R Package

This package contains functions that are useful in data pre-processing and evaluation tasks for exploring performance of the National Water Model streamflow data, especially under low-flow and drought conditions. An example of an evaluation workflow is described below:

## Workflow:
![alt text](https://github.com/cahhansen/NWMLowFlows/blob/master/NWMLowFlows%20R%20Package%20WorkFlow.png "Workflow diagram")

***

Function | Function Description
-------- | --------------------
hourly2DailyQ | Reads in hourly NWM streamflow data from a NetCDF format and aggregates to a daily timestep (computes the average). This function may be run on a directory that contains individual hourly files for a single year, and the folder must be named in the format "yyyy" (e.g. 1993).
USGSFlowStats | Calculates summary streamflow statistics (min, max, avg) and low flow thresholds (5%, 25%, 7Q10, 7Q2) for a given streamgage and time period. Uses the USGS dataRetrieval package and the PearsonDS package.
NWMFlowStats | Calculates summary streamflow statistics (min, max, avg) and low flow thresholds (5%, 25%, 7Q10, 7Q2) for a reach of the NWM. User must the name of the reach (COMID) and specify a .csv file of with a "Date" column and reach columns. Uses the PearsonDS package.