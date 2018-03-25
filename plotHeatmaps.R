
### Call Pull Data functions
source('PullDataMethods.R')
pull_data_2017_All(star=TRUE,score=TRUE)
pull_data_2017_Compare(star=TRUE,score=TRUE)
locat_corr = read.csv('Data/LocationLookup.csv')

# 1-AT&T 2-Sprint 3-T-Mobile 4-Verizon
carriers=c('AT&T','Sprint','T-Mobile','Verizon')

for (carrier_id in 1:length(carriers)){
  callHeatMat = dataHeatMat = smsHeatMat = speedHeatMat = matrix(0,ncol=7,nrow=7)
  for (i in 1:nrow(smsScores_df_change)){
    callHeatMat[callScoresRank_df_change[i,],]
    dataHeatMat
    smsHeatMat
    speedHeatMat[,]
  }
}