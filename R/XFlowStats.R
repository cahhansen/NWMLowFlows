#' USGSFlowStats
#'
#' USGSFlowStats calculates summary stats for streamflow at a given stream gage using the USGS dataRetrieval package.
#'
#' @param gageID USGS gage ID, character
#' @param startDate beginning date for data retrieval, "yyyy-mm-dd"
#' @param endDate end date for data retrieval, "yyyy-mm-dd"
#' @return dataframe of monthly flow statistics
#' @export
#'
USGSFlowStats <- function(gageID,startDate,endDate){
  qData <- dataRetrieval::readNWISdata(sites=gageID, service="dv",parameterCd="00060",
                       startDate=startDate,endDate=endDate)
  #Calculate monthly statistics
  qData$Month <- months(qData$dateTime)
  monthlyQStats <- data.frame(Month = unique(qData$Month),Min=NA,Avg=NA,Max=NA,Perc5=NA,Perc25=NA,x7Q10=NA,x7Q2=NA)
  #Read in discharge data
  for (i in unique(qData$Month)){
    q <- qData[(qData$Month==i),"X_00060_00003"]
    #Use Log-Pearson Type III Distribution. See: https://pubs.usgs.gov/sir/2008/5126/section3.html and
    #http://deq1.bse.vt.edu/sifnwiki/index.php/R_iha_7q10#PearsonDS_function_details for the details on this function
    #Remove 0's (will cause error in log function)
    q <- q[q != 0]
    logq<-log(q)
    pars <- PearsonDS:::pearsonIIIfitML(logq)
    x7Q2 <- exp(PearsonDS::qpearsonIII(0.5, params = pars$par))
    x7Q10 <- exp(PearsonDS::qpearsonIII(0.1, params = pars$par))
    min <- min(q)
    avg <- mean(q)
    max <- max(q)
    perc5 <- quantile(q,na.rm = TRUE, probs=0.05)
    perc25 <- quantile(q,na.rm = TRUE, probs=0.25)
    monthlyQStats[(monthlyQStats$Month==i),c("Min","Avg","Max","Perc5","Perc25","x7Q10","x7Q2")]=c(min,avg,max,perc5,perc25,x7Q10,x7Q2)
  }
  return(monthlyQStats)
}


#'NWMFlowStats
#'
#'NWMFlowStats calculates stats for a given reach, using a user-provided .csv file of NWM data.
#'
#' @param comID a reach ID (corresponds to the feature IDs in the NHD Plus dataset)
#' @param flowfile .csv file of time series data, with columns of Date and comIDs
#' @return dataframe of monthly flow statistics
#' @export
#'
NWMFlowStats <- function(comID,flowfile){
  qData <- read.csv(flowfile)
  comID_column <- paste0("X",comID)
  #Calculate monthly statistics
  qData$Month <- months(as.Date(qData$Date,format="%m/%d/%Y"))
  monthlyQStats <- data.frame(Month = unique(qData$Month),Min=NA,Avg=NA,Max=NA,Perc5=NA,Perc25=NA,x7Q10=NA,x7Q2=NA)
  #Read in discharge data
  for (i in unique(qData$Month)){
    q <- qData[(qData$Month==i),comID_column]
    #Use Log-Pearson Type III Distribution. See: https://pubs.usgs.gov/sir/2008/5126/section3.html and
    #http://deq1.bse.vt.edu/sifnwiki/index.php/R_iha_7q10#PearsonDS_function_details for the details on this function
    #Remove 0's (will cause error in log function)
    q <- q[q != 0]
    logq<-log(q)
    pars <- PearsonDS:::pearsonIIIfitML(logq)
    x7Q2 <- exp(PearsonDS::qpearsonIII(0.5, params = pars$par))
    x7Q10 <- exp(PearsonDS::qpearsonIII(0.1, params = pars$par))
    min <- min(q)
    avg <- mean(q)
    max <- max(q)
    perc5 <- quantile(q,na.rm = TRUE,0.05)
    perc25 <- quantile(q,na.rm = TRUE,0.25)
    monthlyQStats[(monthlyQStats$Month==i),c("Min","Avg","Max","Perc5","Perc25","x7Q10","x7Q2")]=c(min,avg,max,perc5,perc25,x7Q10,x7Q2)
  }
  return(monthlyQStats)
}
