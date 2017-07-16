# NWMLowFlows R Package

This package contains functions that are useful in data pre-processing and evaluation tasks for exploring performance of the National Water Model streamflow data, especially under low-flow and drought conditions. An example of an evaluation workflow is described below:

## Workflow:
![alt text](https://github.com/cahhansen/NWMLowFlows/blob/master/NWMLowFlows%20R%20Package%20WorkFlow.png "Workflow diagram")

***

Function | Function Description
-------- | --------------------
hourly2DailyQ | Reads in hourly NWM streamflow data from a NetCDF format and aggregates to a daily timestep (computes the average). This function runs through a directory that contains individual hourly files for a single year, and the folder must be named in the format "yyyy" (e.g. 1993).
USGSFlowStats | Calculates summary streamflow statistics (min, max, avg) and low flow thresholds (5%, 25%, 7Q10, 7Q2) for a given streamgage and time period. Uses the USGS dataRetrieval package and the PearsonDS package.
NWMFlowStats | Calculates summary streamflow statistics (min, max, avg) and low flow thresholds (5%, 25%, 7Q10, 7Q2) for a reach of the NWM. User must the name of the reach (COMID) and specify a .csv file of with a "Date" column and reach columns. Uses the PearsonDS package.
retroNWMUSGSEval | Calculates goodness-of-fit (GOF) statistics for NWM and USGS time series on an overall, seasonal, and yearly basis. Statistics calculated are provided for both relative streamflow (streamflow as a percentile of historic streamflow values, calculated on a monthly basis) and absolute streamflow (average daily streamflow).
lowFlowEval | Calculates the number of low flow events on a monthly basis over the period of record for both NWM historic data and USGS historic data. Returns a summary of the differences, as well as plots of the number of low flow events and differences in low flow events.