#Example script for evaluating historic data for NWM reaches against USGS gages

comID='17611425'
path=getwd()
filename=paste0(path,'/RetrospectiveData/RetroRecord_cfs.csv')
x=NWMFlowStats(comID = comID, flowfile =filename )
write.csv(x,file = paste0('Accessory Files/LowFlowThresholds/',comID,'.csv'))

gageID='11124500'
x=USGSFlowStats(gageID = gageID, startDate="1993-01-01", endDate = "2016-10-31" )
write.csv(x,file = paste0('Accessory Files/LowFlowThresholds/',gageID,'.csv'))


y=retroNWMUSGSEval(gageID,comID,flowfile=filename,plot.on=TRUE)
save(y,file = paste0(path,'/RetrospectiveEvaluation/',comID,'.RData'))
