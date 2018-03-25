market_data<-read.csv("C:/Users/john_allen/Documents/cofc/Operations Research/data for root metrics/rating_data/rating_data/market_report_sets_ratings_2H2017.csv",header=T)
test_data<-read.csv("C:/Users/john_allen/Documents/cofc/Operations Research/data for root metrics/rating_data/rating_data/test_summary_ratings_2h2017.csv",header=T)
rootscore_data<-read.csv('C:/Users/john_allen/Documents/cofc/Operations Research/data for root metrics/rating_data/rating_data/rootscore_ranks_2H2017.csv',header = T)


# 1-AT&T 2-Sprint 3-T-Mobile 4-Verizon
carriers=c('AT&T','Sprint','T-Mobile','Verizon')
unique_locs=unique(market_data$collection_set_id)

callStars_df = setNames(data.frame(matrix(0,ncol = 4, nrow = length(unique_locs))),carriers)
dataStars_df = setNames(data.frame(matrix(0,ncol = 4, nrow = length(unique_locs))),carriers)
speedStars_df = setNames(data.frame(matrix(0,ncol = 4, nrow = length(unique_locs))),carriers)
smsStars_df = setNames(data.frame(matrix(0,ncol = 4, nrow = length(unique_locs))),carriers)
row.names(callStars_df)=unique_locs
row.names(dataStars_df)=unique_locs
row.names(speedStars_df)=unique_locs
row.names(smsStars_df)=unique_locs

for(carrier_id in 1:length(carriers)){
  #extract carrier information and subset necessary data
  carrier=carriers[carrier_id]#carrier name
  data_ind=which(market_data$report_set_name==carrier) #indices that correspond to a current carrier in market_data
  test_subset=test_data[test_data$carrier_id==carrier_id,] #subset of test_data that only contains rows correspondingt to current carrier
  
  #set up temporary 'star' vectors for assignment in the following code (reset for each carrier)
  callStars=rep(0,length(data_ind))
  dataStars=rep(0,length(data_ind))
  speedStars=rep(0,length(data_ind))
  smsStars = rep(0,length(data_ind))
  
  
  #loop through each of the regions and assign stars based on the criteria specified (for the given carrier at a time)
  for (i in 1:length(data_ind)){
    id=market_data$collection_set_id[data_ind[i]] #set id equal to the area id for each iteration
    tmp_testDat=test_subset[which(test_subset$collection_set_id==id),] #subset the test_subset by selecting rows corresponding to the area id

    smsStars[i] = smsStars[i]+if(market_data[data_ind[i],6]>=.99){1}else if(market_data[data_ind[i],6]>=.97){.5}else{0}
    smsStars[i] = smsStars[i]+if(market_data[data_ind[i],7]>=.99){1}else if(market_data[data_ind[i],7]>=.97){.5}else{0}
    smsStars[i] = smsStars[i]+if(market_data[data_ind[i],8]>=.99){1}else if(market_data[data_ind[i],8]>=.97){.5}else{0}
    smsStars[i] = smsStars[i]+if(market_data[data_ind[i],9]>=.99){1}else if(market_data[data_ind[i],9]>=.97){.5}else{0}
    smsStars[i] = smsStars[i]+if(market_data[data_ind[i],10]>=.98){.5}else{0}
    
    indTest<-which(test_data[data_ind,2]==carrier_id & test_data[data_ind,3]==26)
    mean.ind<-mean(na.omit(market_data[indTest,6]))
    
    smsStars[i]<-smsStars[i]+if(mean.ind<=2000){.5}else{0}
    
    #mobile to landline call drop
    callStars[i]=callStars[i]+ifelse(market_data$co_drop[data_ind[i]]<=.01,2.5,ifelse(market_data$co_drop[data_ind[i]]<=.015,2,ifelse(market_data$co_drop[data_ind[i]]<=.02,1.5,ifelse(market_data$co_drop[data_ind[i]]<=.025,1,ifelse(market_data$co_drop[data_ind[i]]<=.03,.5,0)))))
    #mobile to landline call block
    callStars[i]=callStars[i]+ifelse(market_data$co_block[data_ind[i]]<=.002,1.5,ifelse(market_data$co_block[data_ind[i]]<=.005,1,ifelse(market_data$co_block[data_ind[i]]<=.01,.5,0)))
    #mobile to landline call block
    callStars[i]=callStars[i]+ifelse(market_data$m2mo_block[data_ind[i]]<=.015,1,ifelse(market_data$m2mo_block[data_ind[i]]<=.02,.5,0))

    #Lite Data Secure
    dataStars[i]=dataStars[i]+ifelse(market_data$ldrs_task_success[data_ind[i]]>=.99,.5,0)
    # Download Task Success
    dataStars[i]=dataStars[i]+ifelse(market_data$dsd_task_success[data_ind[i]]>=.99,.5,0)

    percentDLThrough=sum(!is.na(tmp_testDat$dsd_effective_download_test_speed)&tmp_testDat$dsd_effective_download_test_speed>=1000)/sum(!is.na(tmp_testDat$dsd_effective_download_test_speed))
    # % DL Throughput
    dataStars[i]=dataStars[i]+ifelse(percentDLThrough>=.97,2,ifelse(percentDLThrough>=.95,1.5,ifelse(percentDLThrough>=.92,1,ifelse(percentDLThrough>=.9,.5,0))))
    # Upload Task
    dataStars[i]=dataStars[i]+ifelse(market_data$dsd_task_success[data_ind[i]]>=.99,.5,0)
    # % UL Through (dsu_effective_upload_test_speed >= 500)/count(dsu_effective_upload_test_speed) -- where dsdu_effective_upload_test_speed not NULL
    percentULThrough=sum(!is.na(tmp_testDat$dsu_effective_upload_test_speed)&tmp_testDat$dsu_effective_upload_test_speed>=500)/sum(!is.na(tmp_testDat$dsd_effective_download_test_speed))
    dataStars[i]=dataStars[i]+ifelse(percentULThrough>=.97,1,ifelse(percentULThrough>=.92,.5,0))

    #UDP Packet Drop Rate mean(udp_packet_drop_rate) -- where test_type_id = 25
    UDP_dat_mean=mean(na.omit(tmp_testDat[which(tmp_testDat$test_type_id==25),]$udp_avg_packet_drop))
    dataStars[i]=dataStars[i]+ifelse(UDP_dat_mean<=.05,.5,0)

    
    #Calculate Speed and Performance stars for each of the regions
    speedStars[i]=speedStars[i]+ifelse(market_data$dsd_effective_throughput_05p[data_ind[i]]>=5000,1.5,ifelse(market_data$dsd_effective_throughput_05p[data_ind[i]]>=3000,1,ifelse(market_data$dsd_effective_throughput_05p[data_ind[i]]>=2000,0.5,0)))
    
    speedStars[i]=speedStars[i]+ifelse(market_data$dsd_time_to_first_byte_50p[data_ind[i]]<=400,1,ifelse(market_data$dsd_time_to_first_byte_50p[data_ind[i]]<=700,0.5,0))
    
    speedStars[i]=speedStars[i]+ifelse(market_data$dsd_effective_throughput_95p[data_ind[i]]>=75000,.5,0)
    
    speedStars[i]=speedStars[i]+ifelse(market_data$dsu_effective_throughput_05p[data_ind[i]]>=1500,1,ifelse(market_data$dsu_effective_throughput_05p[data_ind[i]]>=1000,0.5,0)) 
  
    liteData95Quant=quantile(na.omit(tmp_testDat[which(tmp_testDat$test_type_id==26),]$ldrs_task_speed_max),probs=c(.95))
    speedStars[i]=speedStars[i]+ifelse(liteData95Quant<=1000,.5,0)
    
    MM95Quant=quantile(na.omit(tmp_testDat[which(tmp_testDat$test_type_id==23 & tmp_testDat$flag_access_success=='t'),]$m2mo_total_call_setup_duration),probs=c(.95))
    speedStars[i]=speedStars[i]+ifelse(MM95Quant<=7000,.5,0)
  }
  
  
  callStars_df[,carrier_id]=callStars
  dataStars_df[,carrier_id]=dataStars
  speedStars_df[,carrier_id]=speedStars
  smsStars_df[,carrier_id]=smsStars
  
}






callWins_pure=NULL
callWins_tie=NULL
dataWins_pure=NULL
dataWins_tie=NULL
for(i in 1:nrow(callStars_df)){
  callWinner=which(callStars_df[i,]==max( callStars_df[i,]))
  dataWinner=which(dataStars_df[i,]==max( dataStars_df[i,]))
  if(length(callWinner)==1){
    callWins_pure=c(callWins_pure,callWinner)
  }else
    callWins_tie=c(callWins_tie,callWinner)
  if(length(dataWinner)==1){
    dataWins_pure=c(dataWins_pure,dataWinner)
  }else
    dataWins_tie=c(dataWins_tie,dataWinner)
}

hist(callWins_pure,breaks=seq(.5,4.5,1))
hist(callWins_tie,breaks=seq(.5,4.5,1))
hist(dataWins_pure,breaks=seq(.5,4.5,1))
hist(dataWins_tie,breaks=seq(.5,4.5,1))





#Winners from the rootscores
catStars<-c('Data','Call','Text','Speed')
winnersUnique<-matrix(0,nrow = length(carrier.means[,1]),ncol = length(catStars))
winnersStars<-matrix(0,nrow = length(carrier.means[,1]),ncol = length(catStars))

allWins<-NULL

for(h in 1:length(carriers)){
  carWins<-NULL
  for(j in 1:length(catStars)){
    car.wins<-NULL
    for(i in 1:length(unique_locs)){
      ind<-which(rootscore_data[,2]==unique_locs[i] & rootscore_data[,4]==h & rootscore_data[,3]==catStars[j])
      car.wins<-rbind(car.wins,c(unique_locs[i],if(rootscore_data[ind,5]==1){1}else{0}))
    }
    
    carWins<-cbind(carWins,car.wins[,2])
    
  }
  colnames(carWins)<-catStars
  rownames(carWins)<-unique_locs
  space<-matrix(0,nrow=125,ncol=1)
  colnames(space)<-carriers[h]
  allWins<-cbind(allWins,space,carWins)
}


#winners from the star method
allWinsStars<-NULL

for(i in 1:length(carriers)){
  #Data, Call, Text, Speed
  carwinstar<-NULL
  for(j in 1:length(unique_locs)){
  dataWin<-NULL
  nMax<-max(dataStars_df[j,])
  dataWin<-if(dataStars_df[j,i]==nMax){1}else{0}
  
  callWin<-NULL
  nMax<-max(callStars_df[j,])
  callWin<-if(callStars_df[j,i]==nMax){1}else{0}
  
  smsWin<-NULL
  nMax<-max(smsStars_df[j,])
  smsWin<-if(smsStars_df[j,i]==nMax){1}else{0}
  
  speedWin<-NULL
  nMax<-max(speedStars_df[j,])
  speedWin<-if(speedStars_df[j,i]==nMax){1}else{0}
  
  carwinstar<-rbind(carwinstar,c(dataWin,callWin,smsWin,speedWin))
  }
  
  colnames(carwinstar)<-catStars
  rownames(carwinstar)<-unique_locs
  space<-matrix(0,nrow=125,ncol=1)
  colnames(space)<-carriers[i]
  allWinsStars<-cbind(allWinsStars,space,carwinstar)
  
}





#Figure Plots

#Stars plots
#Speed Stars
par(mfrow=c(2,2))
for( i in 1:4){
  hist(speedStars_df[,i],main=paste(carriers[i],' Speed'),xlab='Stars',ylim = c(0,100),breaks=seq(-.25,5.25,by=.5))
  abline(v=mean(speedStars_df[,i]),col='red')
}


#Calls Stars
par(mfrow=c(2,2))
for( i in 1:4){
  hist(callStars_df[,i],main=paste(carriers[i],' Calls'),xlab='Stars',ylim = c(0,100),breaks=seq(-.25,5.25,by=.5))
  abline(v=mean(callStars_df[,i]),col='red')
}


#Data Stars
par(mfrow=c(2,2))
for( i in 1:4){
  hist(dataStars_df[,i],main=paste(carriers[i],' Data'),xlab='Stars',ylim = c(0,100),breaks=seq(-.25,5.25,by=.5))
  abline(v=mean(dataStars_df[,i]),col='red')
}


#SMS Stars
par(mfrow=c(2,2))
for( i in 1:4){
  hist(smsStars_df[,i],main=paste(carriers[i],' SMS'),xlab='Stars',ylim = c(0,100),breaks=seq(-.25,5.25,by=.5))
  abline(v=mean(smsStars_df[,i]),col='red')
}



#Wins comparison plots

#Data Comaprison
for(j in 1:4){
par(mfrow=c(2,2))
for(i in 1:4){
  mod.wins<-allWins+.5
  hist(c(2*allWinsStars[,(j+1+5*(i-1))],2*allWins[,(j+1+5*(i-1))]+.5),breaks = seq(-.25,2.75,.5),main = paste(carriers[i],catStars[j]),col=c('red','blue'),xlab='Wins and Not wins',ylim=c(0,125))
  if(i==1){legend(4,50,legend = c('Stars','Rootscore'),col=c('red','blue'),lty=1)}
  }
}




#all rankings
#datastars
dataStars.rank<-matrix(NA,nrow=125,ncol=4)
dataStars_df<-as.matrix(dataStars_df)
for(i in 1:length(dataStars_df[,1])){
  vals<-unique(dataStars_df[i,],fromLast = TRUE)
  tot<-0
  for(j in rev(order(vals))){
    ind<-which(dataStars_df[i,]==vals[j])
    dataStars.rank[i,ind]<-1+tot
    tot<-tot+length(ind)
  }
}
colnames(dataStars.rank)<-carriers
rownames(dataStars.rank)<-unique_locs


#rootrankings for data
dataRoot.rank<-matrix(NA,nrow=125,ncol=4)
ind.data<-which(rootscore_data[,3]=='Data')
for(i in 1:length(ind.data)){
  dataRoot.rank[ceiling(i/4),rootscore_data[ind.data[i],4]]<-rootscore_data[ind.data[i],5]
}
colnames(dataRoot.rank)<-carriers
rownames(dataRoot.rank)<-unique_locs
data.tab<-table(dataRoot.rank,dataStars.rank)



#SMSstars
smsStars.rank<-matrix(NA,nrow=125,ncol=4)
smsStars_df<-as.matrix(smsStars_df)
for(i in 1:length(smsStars_df[,1])){
  vals<-unique(smsStars_df[i,],fromLast = TRUE)
  tot<-0
  for(j in rev(order(vals))){
    ind<-which(smsStars_df[i,]==vals[j])
    smsStars.rank[i,ind]<-1+tot
    tot<-tot+length(ind)
  }
}
colnames(smsStars.rank)<-carriers
rownames(smsStars.rank)<-unique_locs


#rootrankings for SMS
smsRoot.rank<-matrix(NA,nrow=125,ncol=4)
ind.data<-which(rootscore_data[,3]=='Data')
for(i in 1:length(ind.data)){
  smsRoot.rank[ceiling(i/4),rootscore_data[ind.data[i],4]]<-rootscore_data[ind.data[i],5]
}
colnames(smsRoot.rank)<-carriers
rownames(smsRoot.rank)<-unique_locs
sms.tab<-table(smsRoot.rank,smsStars.rank)




#speedstars
speedStars.rank<-matrix(NA,nrow=125,ncol=4)
speedStars_df<-as.matrix(speedStars_df)
for(i in 1:length(speedStars_df[,1])){
  vals<-unique(speedStars_df[i,],fromLast = TRUE)
  tot<-0
  for(j in rev(order(vals))){
    ind<-which(speedStars_df[i,]==vals[j])
    speedStars.rank[i,ind]<-1+tot
    tot<-tot+length(ind)
  }
}
colnames(speedStars.rank)<-carriers
rownames(speedStars.rank)<-unique_locs


#rootrankings for data
speedRoot.rank<-matrix(NA,nrow=125,ncol=4)
ind.data<-which(rootscore_data[,3]=='Data')
for(i in 1:length(ind.data)){
  speedRoot.rank[ceiling(i/4),rootscore_data[ind.data[i],4]]<-rootscore_data[ind.data[i],5]
}
colnames(speedRoot.rank)<-carriers
rownames(speedRoot.rank)<-unique_locs
speed.tab<-table(speedRoot.rank,speedStars.rank)







#callstars
callStars.rank<-matrix(NA,nrow=125,ncol=4)
callStars_df<-as.matrix(callStars_df)
for(i in 1:length(callStars_df[,1])){
  vals<-unique(callStars_df[i,],fromLast = TRUE)
  tot<-0
  for(j in rev(order(vals))){
    ind<-which(callStars_df[i,]==vals[j])
    callStars.rank[i,ind]<-1+tot
    tot<-tot+length(ind)
  }
}
colnames(callStars.rank)<-carriers
rownames(callStars.rank)<-unique_locs


#rootrankings for data
callRoot.rank<-matrix(NA,nrow=125,ncol=4)
ind.data<-which(rootscore_data[,3]=='Data')
for(i in 1:length(ind.data)){
  callRoot.rank[ceiling(i/4),rootscore_data[ind.data[i],4]]<-rootscore_data[ind.data[i],5]
}
colnames(callRoot.rank)<-carriers
rownames(callRoot.rank)<-unique_locs
call.tab<-table(callRoot.rank,callStars.rank)

