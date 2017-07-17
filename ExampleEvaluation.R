#Example script for evaluating historic data for NWM reaches against USGS gages

#Calculate low flow stats for NWM reach
comID='18578829'
path=getwd()
filename=paste0(path,'/RetrospectiveData/RetroRecord_cfs.csv')
x=NWMFlowStats(comID = comID, flowfile =filename )
write.csv(x,file = paste0('Accessory Files/LowFlowThresholds/',comID,'.csv'))

#Calculate low flow stats for USGS gage
gageID='02450250'
x=USGSFlowStats(gageID = gageID, startDate="1993-01-01", endDate = "2016-10-31" )
write.csv(x,file = paste0('Accessory Files/LowFlowThresholds/',gageID,'.csv'))

#Calculate GOF statistics for streamflow (relative and absolute)
y=retroNWMUSGSEval(gageID,comID,flowfile=filename,plot.on=TRUE)
save(y,file = paste0(path,'/RetrospectiveEvaluation/',comID,'.RData'))

#Calculate numbers of low flow events
startDate="1993-01-01"
endDate = "2016-10-31"
threshold="x7Q10"
summary=lowFlowEval(gageID=gageID,comID=comID,flowfile=filename,threshold="x7Q10",startDate=startDate,endDate=endDate)

#Plot results
par(mar=c(5, 4, 4, 2) + 0.1)
plot(x=summary$Date,y=summary$DiffEvents,type='l',ylab=paste0("Difference in # of Low Flow Events"),xlab="Time",
     main=paste0("Low-Flow Threshold:",threshold," at: ",comID,"\n (USGS - NWM)"))

par(xpd=T, mar=par()$mar+c(0,0,0,6))
plot(x=summary$Date,y=summary$USGS,type='l',col=1,main=paste0("Low-Flow Threshold: ",threshold," at : ",comID),ylab="Number of Low Flow Events",xlab="Time")
lines(x=summary$Date,y=summary$NWM,col=2)
legend("topright", inset=c(-0.4,0), legend=c("USGS","NWM"), lty=c(1,1),col=c(1,2))
