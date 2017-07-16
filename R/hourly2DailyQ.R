#' Aggregate NetCDF Hourly Streamflow to Daily
#'
#' @param directory Directory where hourly NetCDF files are kept. Must be named for the year (yyyy) of the NetCDF files contained inside.
#' @param comID A vector of COMID values (identifiers of NHDPlus stream reaches).
#' @return A time series of daily streamflow for each of the user-specified reaches.
#' @export
#' @import ncdf4
#' @examples
#' dailyQ <- hourly2DailyQ("~/1994",c("18578829", "18578755"))

hourly2DailyQ <- function(directory,comID){
  year <- substr(directory,nchar(directory)-3,nchar(directory))
  #Create list for cycling through hourly files
  hourList <- sprintf("%02d",seq(0,23,by=1))

  #Cycle through the hourly time steps and convert to daily
  ncdfFileList <- list.files(path=directory,pattern = "\\.nc$")
  dateList <- unique(substr(ncdfFileList,5,8))
  #Initialize data frame for daily summaries
  dailyQDF <- data.frame(Date=dateList)
  dailyQDF[as.character(comID)] <- as.numeric(NA)
  for (y in dateList){
    #Initialize data frame for hourly values
    hourlyQDF <- data.frame(Hour=hourList)
    hourlyQDF[as.character(comID)] <- as.numeric(NA)
    for (x in hourList){
      #Get and open netcdf files
      ncdfFileName <- paste0(year,y,x,"00_streamflow.nc")
      ncdfFile <- paste0(directory,"/",ncdfFileName)
      nwmFile <- nc_open(ncdfFile,readunlim=FALSE)
      #Check if the streamflow data is missing (if so, assign NA value)
      if (nwmFile$dim$time$len==0){
        nc_close(nwmFile)
      }else{
        #Initialize dataframe with columns for each reach (COMID)
        if (y=="0101" & x == "00"){
          #Get index of COMID from netcdf (dim=1 is the stream reach)
          featureIDList <- nwmFile$dim$feature_id$vals
          featureIndex <- match(comID,featureIDList)
        }
        for (i in seq(1,length(featureIndex),by=1)){
          #Get streamflow for chosen reaches(in cms)
          nwmData <- ncvar_get(nwmFile,
                            varid = "streamflow",
                            start = c(featureIndex[i],1), #Start value for dimensions (first value is stream reach index)
                            count = c(1,-1)) #Number of values to get in each dimension
          #Add hourly streamflow value to data frame
          hourlyQDF[(hourlyQDF$Hour==x),comID[i]] <- nwmData
        }
        nc_close(nwmFile)
      }
    }

    #Calculate daily mean streamflow
    if(length(comID) >= 2){
      meanDailyQ <- colMeans(hourlyQDF[,-1],na.rm=TRUE)
    }else{
      meanDailyQ <- mean(hourlyQDF[,2],na.rm=TRUE)
    }
    #Add daily mean streamflow to data frame
    dailyQDF[(dailyQDF$Date==y),c(comID)] <- meanDailyQ
    print(paste0("Finished processing: ",y))
  }
  #Format Date
  dailyQDF$Date <- as.Date(paste0(year,"-",substr(dailyQDF$Date,1,2),"-",substr(dailyQDF$Date,3,4)))
  return(dailyQDF)
}
