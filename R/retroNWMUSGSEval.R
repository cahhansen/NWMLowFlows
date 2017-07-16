#' retroNWMUSGSEval
#'
#' Calculates a suite of goodness of fit/model performance statistics. Calculates overall GOF,
#' seasonal, and by year.
#'
#' @param gageID USGS gage ID, character
#' @param comID beginning date for data retrieval, "yyyy-mm-dd"
#' @param flowfile end date for data retrieval, "yyyy-mm-dd"
#' @param plot.on plots time series of input data. Default is TRUE
#' @return list of goodness of fit statistics
#' @export
#' @import hydroGOF
#'

retroNWMUSGSEval <- function(gageID,comID,flowfile,plot.on=TRUE){
  #Get discharge USGS Data
  qDataUSGS <- dataRetrieval::readNWISdata(sites=gageID, service="dv",parameterCd="00060",
                           startDate="1993-01-01",endDate="2016-10-31")
  qDataUSGS <- data.frame(Date=as.Date(qDataUSGS$dateTime),Discharge=qDataUSGS$X_00060_00003)
  #Get discharge from Historical Modeled (Retrospective) data
  qDataNWM <- read.csv(flowfile)
  qDataNWM$Date <- as.Date(qDataNWM$Date, format="%m/%d/%Y")
  comID_column <- paste0("X",comID)
  qDataNWM <- qDataNWM[,(names(qDataNWM) %in% c("Date",comID_column))]
  colnames(qDataNWM)<-c("Date","Discharge")

  #Calculate percentile of flow from historical data
  calc_percentile <- function(data){
    perc.rank <- (trunc(rank(data[,2]))/length(data[,2]))*100
    return(perc.rank)
  }

  #Overall GOF---------------------------------------------------------------------------------
  qDataNWM$QPerc <- calc_percentile(data=qDataNWM)
  qDataUSGS$QPerc <- calc_percentile(data=qDataUSGS)

  QGOF_relative <- hydroGOF:::gof(sim=qDataNWM$QPerc,obs=qDataUSGS$QPerc)
  QGOF_absolute <- hydroGOF:::gof(sim=qDataNWM$Discharge,obs=qDataUSGS$Discharge)
  #Graphical representation
  if (plot.on==TRUE){
    hydroGOF::ggof(sim=qDataNWM$QPerc,obs=qDataUSGS$QPerc)
  }
  #--------------------------------------------------------------------------------------------

  #GOF by season------------------------------------------------------------------------------
  seasonsubset <- function(dataframe){
    dataframe[(months(dataframe$Date) %in% c("January","February","March")),"Season"] = "Winter"
    dataframe[(months(dataframe$Date) %in% c("April","May","June")),"Season"] = "Spring"
    dataframe[(months(dataframe$Date) %in% c("July","August","September")),"Season"] = "Summer"
    dataframe[(months(dataframe$Date) %in% c("October","November","December")),"Season"] = "Fall"
    dataframe$Season <- as.factor(dataframe$Season)
    return(dataframe)
  }
  qDataNWMSeasonal <- seasonsubset(dataframe=qDataNWM)
  qDataUSGSSeasonal <- seasonsubset(dataframe=qDataUSGS)

  SeasonalGOF_relative <- data.frame(Winter=rep(NA,20),Spring=rep(NA,20),Summer=rep(NA,20),Fall=rep(NA,20))
  SeasonalGOF_absolute <- data.frame(Winter=rep(NA,20),Spring=rep(NA,20),Summer=rep(NA,20),Fall=rep(NA,20))
  for(i in c("Winter","Spring","Summer","Fall")){
    qDataNWMSeasonal[(qDataNWMSeasonal$Season==i),"QPerc"]<-
      calc_percentile(qDataNWMSeasonal[(qDataNWMSeasonal$Season==i),])
    qDataUSGSSeasonal[(qDataUSGSSeasonal$Season==i),"QPerc"]<-
      calc_percentile(qDataUSGSSeasonal[(qDataNWMSeasonal$Season==i),])
    NWMsub <- qDataNWMSeasonal[(qDataNWMSeasonal$Season==i),]
    USGSsub <- qDataUSGSSeasonal[(qDataUSGSSeasonal$Season==i),]
    tempgof_relative <- hydroGOF:::gof(sim=NWMsub$QPerc,obs=USGSsub$QPerc)
    tempgof_absolute <- hydroGOF:::gof(sim=NWMsub$Discharge,obs=USGSsub$Discharge)
    SeasonalGOF_relative[,i] <- tempgof_relative[,1]
    SeasonalGOF_absolute[,i] <- tempgof_absolute[,1]
  }
  row.names(SeasonalGOF_relative) <- row.names(tempgof_relative)
  row.names(SeasonalGOF_absolute) <- row.names(tempgof_absolute)
  #-------------------------------------------------------------------------------------------
  #GOF by year
  qDataUSGS$Year <- lubridate::year(qDataUSGS$Date)
  qDataNWM$Year <- lubridate::year(qDataNWM$Date)
  YearlyGOF_relative <- data.frame(matrix(ncol = 24, nrow = 20))
  colnames(YearlyGOF_relative)<-as.character(seq(1993,2016,by=1))
  YearlyGOF_absolute <- data.frame(matrix(ncol = 24, nrow = 20))
  colnames(YearlyGOF_absolute)<-as.character(seq(1993,2016,by=1))
  for (j in seq(1993,2016,by=1)){
    tempgof_relative <- hydroGOF:::gof(sim=qDataNWM[(qDataNWM$Year==j),"QPerc"],
                         obs=qDataUSGS[(qDataUSGS$Year==j),"QPerc"])
    tempgof_absolute <- hydroGOF:::gof(sim=qDataNWM[(qDataNWM$Year==j),"Discharge"],
                         obs=qDataUSGS[(qDataUSGS$Year==j),"Discharge"])
    YearlyGOF_relative[,as.character(j)] <- tempgof_relative[,1]
    YearlyGOF_absolute[,as.character(j)] <- tempgof_absolute[,1]
  }
  row.names(YearlyGOF_relative) <- row.names(tempgof_relative)
  row.names(YearlyGOF_absolute) <- row.names(tempgof_absolute)
  #--------------------------------------------------------------------------------------------

  return(list(QGOF_relative,QGOF_absolute,SeasonalGOF_relative,SeasonalGOF_absolute,YearlyGOF_relative,YearlyGOF_absolute))

}
