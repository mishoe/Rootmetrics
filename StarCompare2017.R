market_data_1<-read.csv("~/documents/Consulting/Rootmetrics/rating_data/rating_data/market_report_sets_ratings_1H2017.csv",header=T)
test_data_1<-read.csv("~/documents/Consulting/Rootmetrics/test_summary_data/test_summary_ratings_1h2017.csv",header=T)
rootscore_data_1<-read.csv('~/documents/Consulting/Rootmetrics/rating_data/rating_data/rootscore_ranks_1H2017.csv',header = T)
collection_sets_1<-read.csv('~/documents/Consulting/Rootmetrics/rating_data/rating_data/collection_sets_1H2017.csv',header = T)

market_data_2<-read.csv("~/documents/Consulting/Rootmetrics/rating_data/rating_data/market_report_sets_ratings_2H2017.csv",header=T)
test_data_2<-read.csv("~/documents/Consulting/Rootmetrics/test_summary_data/test_summary_ratings_2h2017.csv",header=T)
rootscore_data_2<-read.csv('~/documents/Consulting/Rootmetrics/rating_data/rating_data/rootscore_ranks_2H2017.csv',header = T)
collection_sets_2<-read.csv('~/documents/Consulting/Rootmetrics/rating_data/rating_data/collection_sets_2H2017.csv',header = T)

# 1-AT&T 2-Sprint 3-T-Mobile 4-Verizon
carriers=c('AT&T','Sprint','T-Mobile','Verizon')

locat_corr=data.frame()
sort_ids=sort(collection_sets_2$collection_set_id)
names=c()
for (id in 1:length(collection_sets_2$market_name)){
  tmp_id = collection_sets_1$collection_set_id[which(collection_sets_1$market_name==collection_sets_2$market_name[which(collection_sets_2$collection_set_id==sort_ids[id])])]
  locat_corr=rbind(locat_corr,c(tmp_id,sort_ids[id]))
  names=c(names,as.character(collection_sets_2$market_name[which(collection_sets_2$collection_set_id==sort_ids[id])]))
}
colnames(locat_corr)=c('id_2017_1','id_2017_2')
locat_corr$City=names


#initialize each of the dataframes for the four categories in both halves of 2017
callStars_df_1 =dataStars_df_1=speedStars_df_1=smsStars_df_1= setNames(data.frame(matrix(0,ncol = 4, nrow = nrow(locat_corr))),carriers)
row.names(callStars_df_1)=row.names(dataStars_df_1)=row.names(speedStars_df_1)=row.names(smsStars_df_1)=locat_corr$id_2017_1
callStars_df_2 =dataStars_df_2=speedStars_df_2=smsStars_df_2= setNames(data.frame(matrix(0,ncol = 4, nrow = nrow(locat_corr))),carriers)
row.names(callStars_df_2)=row.names(dataStars_df_2)=row.names(speedStars_df_2)=row.names(smsStars_df_2)=locat_corr$id_2017_2

smsVector= rep(0,12)
for(carrier_id in 1:length(carriers)){
  #extract carrier information and subset necessary data
  carrier=carriers[carrier_id]#carrier name
  data_ind_1=which(market_data_1$report_set_name==carrier) #indices that correspond to a current carrier in market_data_2
  test_subset_1=test_data_1[test_data_1$carrier_id==carrier_id,] #subset of test_data_1 that only contains rows correspondingt to current carrier
  data_ind_2=which(market_data_2$report_set_name==carrier) #indices that correspond to a current carrier in market_data_2
  test_subset_2=test_data_2[test_data_2$carrier_id==carrier_id,] #subset of test_data_2 that only contains rows correspondingt to current carrier
  
  #set up temporary 'star' vectors for assignment in the following code (reset for each carrier)
  callStars_1=dataStars_1=speedStars_1=smsStars_1=rep(0,length(data_ind_1))
  callStars_2=dataStars_2=speedStars_2=smsStars_2=rep(0,length(data_ind_2))
  
  #loop through each of the regions and assign stars based on the criteria specified (for the given carrier at a time)
  for (i in 1:nrow(locat_corr)){
    
    id_1=market_data_1$collection_set_id[data_ind_1[i]] #set id equal to the area id for each iteration
    tmp_testDat_1=test_subset_1[which(test_subset_1$collection_set_id==id_1),] #subset the test_subset_2 by selecting rows corresponding to the area id
    id_2=locat_corr$id_2017_2[which(locat_corr$id_2017_1==id_1)]
    tmp_testDat_2=test_subset_2[which(test_subset_2$collection_set_id==id_2),] #subset the test_subset_2 by selecting rows corresponding to the area id

    ### Assign stars for sms
    smsStars_1[i] = smsStars_1[i]+if(market_data_1$`sms_access_success_inter`[data_ind_1[i]]>=.99){1}else if(market_data_1$`sms_access_success_inter`[data_ind_1[i]]>=.97){.5}else{0}
    smsStars_1[i] = smsStars_1[i]+if(market_data_1$`sms_access_success_intra`[data_ind_1[i]]>=.99){1}else if(market_data_1$`sms_access_success_intra`[data_ind_1[i]]>=.97){.5}else{0}
    smsStars_1[i] = smsStars_1[i]+if(market_data_1$`sms_task_success_inter`[data_ind_1[i]]>=.99){1}else if(market_data_1$`sms_task_success_inter`[data_ind_1[i]]>=.97){.5}else{0}
    smsStars_1[i] = smsStars_1[i]+if(market_data_1$`sms_task_success_intra`[data_ind_1[i]]>=.99){1}else if(market_data_1$`sms_task_success_intra`[data_ind_1[i]]>=.97){.5}else{0}
    smsStars_1[i] = smsStars_1[i]+if(market_data_1$`ldrs_task_success`[data_ind_1[i]]>=.97){.5}else{0}
    
    smsStars_2[i] = smsStars_2[i]+if(market_data_2$`sms_access_success_inter`[data_ind_2[i]]>=.99){1}else if(market_data_2$`sms_access_success_inter`[data_ind_2[i]]>=.97){.5}else{0}
    smsStars_2[i] = smsStars_2[i]+if(market_data_2$`sms_access_success_intra`[data_ind_2[i]]>=.99){1}else if(market_data_2$`sms_access_success_intra`[data_ind_2[i]]>=.97){.5}else{0}
    smsStars_2[i] = smsStars_2[i]+if(market_data_2$`sms_task_success_inter`[data_ind_2[i]]>=.99){1}else if(market_data_2$`sms_task_success_inter`[data_ind_2[i]]>=.97){.5}else{0}
    smsStars_2[i] = smsStars_2[i]+if(market_data_2$`sms_task_success_intra`[data_ind_2[i]]>=.99){1}else if(market_data_2$`sms_task_success_intra`[data_ind_2[i]]>=.97){.5}else{0}
    smsStars_2[i] = smsStars_2[i]+if(market_data_2$`ldrs_task_success`[data_ind_2[i]]>=.97){.5}else{0}
    
    smsVector[1]=smsVector[1]+if(market_data_1$`sms_access_success_inter`[data_ind_1[i]]>=.99){1}else if(market_data_1$`sms_access_success_inter`[data_ind_1[i]]>=.97){.5}else{0}
    smsVector[2]=smsVector[2]+if(market_data_1$`sms_access_success_intra`[data_ind_1[i]]>=.99){1}else if(market_data_1$`sms_access_success_intra`[data_ind_1[i]]>=.97){.5}else{0}
    smsVector[3]=smsVector[3]+if(market_data_1$`sms_task_success_inter`[data_ind_1[i]]>=.99){1}else if(market_data_1$`sms_task_success_inter`[data_ind_1[i]]>=.97){.5}else{0}
    smsVector[4]=smsVector[4]+if(market_data_1$`sms_task_success_intra`[data_ind_1[i]]>=.99){1}else if(market_data_1$`sms_task_success_intra`[data_ind_1[i]]>=.97){.5}else{0}
    smsVector[5]=smsVector[5]+if(market_data_1$`ldrs_task_success`[data_ind_1[i]]>=.97){.5}else{0}
    smsVector[6]=smsVector[6]+if(market_data_2$`sms_access_success_inter`[data_ind_2[i]]>=.99){1}else if(market_data_2$`sms_access_success_inter`[data_ind_2[i]]>=.97){.5}else{0}
    smsVector[7]=smsVector[7]+if(market_data_2$`sms_access_success_intra`[data_ind_2[i]]>=.99){1}else if(market_data_2$`sms_access_success_intra`[data_ind_2[i]]>=.97){.5}else{0}
    smsVector[8]=smsVector[8]+if(market_data_2$`sms_task_success_inter`[data_ind_2[i]]>=.99){1}else if(market_data_2$`sms_task_success_inter`[data_ind_2[i]]>=.97){.5}else{0}
    smsVector[9]=smsVector[9]+if(market_data_2$`sms_task_success_intra`[data_ind_2[i]]>=.99){1}else if(market_data_2$`sms_task_success_intra`[data_ind_2[i]]>=.97){.5}else{0}
    smsVector[10]=smsVector[10]+if(market_data_2$`ldrs_task_success`[data_ind_2[i]]>=.97){.5}else{0}

    #### come back to this......
    indTest<-length(which(na.omit(tmp_testDat_1[which(tmp_testDat_1$test_type_id==26),]$ldrs_task_speed_max)<2000))/length(na.omit(tmp_testDat_1[which(tmp_testDat_1$test_type_id==26),]$ldrs_task_speed_max))
    smsStars_1[i]<-smsStars_1[i]+if(indTest<=.98){.5}else{0}
    smsVector[11]=smsVector[11]+if(indTest<=.98){.5}else{0}
    
    
    indTest<-length(which(na.omit(tmp_testDat_2[which(tmp_testDat_2$test_type_id==26),]$ldrs_task_speed_max)<2000))/length(na.omit(tmp_testDat_2[which(tmp_testDat_2$test_type_id==26),]$ldrs_task_speed_max))
    smsStars_2[i]<-smsStars_2[i]+if(indTest<=.98){.5}else{0}
    smsVector[12]=smsVector[12]+if(indTest<=.98){.5}else{0}
    
    #mobile to landline call drop
    callStars_1[i]=callStars_1[i]+ifelse(market_data_1$co_drop[data_ind_1[i]]<=.01,2.5,ifelse(market_data_1$co_drop[data_ind_1[i]]<=.015,2,ifelse(market_data_1$co_drop[data_ind_1[i]]<=.02,1.5,ifelse(market_data_1$co_drop[data_ind_1[i]]<=.025,1,ifelse(market_data_1$co_drop[data_ind_1[i]]<=.03,.5,0)))))
    #mobile to landline call block
    callStars_1[i]=callStars_1[i]+ifelse(market_data_1$co_block[data_ind_1[i]]<=.002,1.5,ifelse(market_data_1$co_block[data_ind_1[i]]<=.005,1,ifelse(market_data_1$co_block[data_ind_1[i]]<=.01,.5,0)))
    #mobile to landline call block
    callStars_1[i]=callStars_1[i]+ifelse(market_data_1$m2mo_block[data_ind_1[i]]<=.015,1,ifelse(market_data_1$m2mo_block[data_ind_1[i]]<=.02,.5,0))

    #mobile to landline call drop
    callStars_2[i]=callStars_2[i]+ifelse(market_data_2$co_drop[data_ind_2[i]]<=.01,2.5,ifelse(market_data_2$co_drop[data_ind_2[i]]<=.015,2,ifelse(market_data_2$co_drop[data_ind_2[i]]<=.02,1.5,ifelse(market_data_2$co_drop[data_ind_2[i]]<=.025,1,ifelse(market_data_2$co_drop[data_ind_2[i]]<=.03,.5,0)))))
    #mobile to landline call block
    callStars_2[i]=callStars_2[i]+ifelse(market_data_2$co_block[data_ind_2[i]]<=.002,1.5,ifelse(market_data_2$co_block[data_ind_2[i]]<=.005,1,ifelse(market_data_2$co_block[data_ind_2[i]]<=.01,.5,0)))
    #mobile to landline call block
    callStars_2[i]=callStars_2[i]+ifelse(market_data_2$m2mo_block[data_ind_2[i]]<=.015,1,ifelse(market_data_2$m2mo_block[data_ind_2[i]]<=.02,.5,0))
 

    #Lite Data Secure
    dataStars_1[i]=dataStars_1[i]+ifelse(market_data_1$ldrs_task_success[data_ind_1[i]]>=.99,.5,0)
    # Download Task Success
    dataStars_1[i]=dataStars_1[i]+ifelse(market_data_1$dsd_task_success[data_ind_1[i]]>=.99,.5,0)
    
    percentDLThrough=sum(!is.na(tmp_testDat_1$dsd_effective_download_test_speed)&tmp_testDat_1$dsd_effective_download_test_speed>=1000)/sum(!is.na(tmp_testDat_1$dsd_effective_download_test_speed))
    # % DL Throughput
    dataStars_1[i]=dataStars_1[i]+ifelse(percentDLThrough>=.97,2,ifelse(percentDLThrough>=.95,1.5,ifelse(percentDLThrough>=.92,1,ifelse(percentDLThrough>=.9,.5,0))))
    # Upload Task
    dataStars_1[i]=dataStars_1[i]+ifelse(market_data_1$dsd_task_success[data_ind_1[i]]>=.99,.5,0)
    # % UL Through (dsu_effective_upload_test_speed >= 500)/count(dsu_effective_upload_test_speed) -- where dsdu_effective_upload_test_speed not NULL
    percentULThrough=sum(!is.na(tmp_testDat_1$dsu_effective_upload_test_speed)&tmp_testDat_1$dsu_effective_upload_test_speed>=500)/sum(!is.na(tmp_testDat_1$dsd_effective_download_test_speed))
    dataStars_1[i]=dataStars_1[i]+ifelse(percentULThrough>=.97,1,ifelse(percentULThrough>=.92,.5,0))
    
    #UDP Packet Drop Rate mean(udp_packet_drop_rate) -- where test_type_id = 25
    UDP_dat_mean=mean(na.omit(tmp_testDat_1[which(tmp_testDat_1$test_type_id==25),]$udp_avg_packet_drop))
    dataStars_1[i]=dataStars_1[i]+ifelse(UDP_dat_mean<=.05,.5,0)
    
    
    #Lite Data Secure
    dataStars_2[i]=dataStars_2[i]+ifelse(market_data_2$ldrs_task_success[data_ind_2[i]]>=.99,.5,0)
    # Download Task Success
    dataStars_2[i]=dataStars_2[i]+ifelse(market_data_2$dsd_task_success[data_ind_2[i]]>=.99,.5,0)
    
    percentDLThrough=sum(!is.na(tmp_testDat_2$dsd_effective_download_test_speed)&tmp_testDat_2$dsd_effective_download_test_speed>=1000)/sum(!is.na(tmp_testDat_2$dsd_effective_download_test_speed))
    # % DL Throughput
    dataStars_2[i]=dataStars_2[i]+ifelse(percentDLThrough>=.97,2,ifelse(percentDLThrough>=.95,1.5,ifelse(percentDLThrough>=.92,1,ifelse(percentDLThrough>=.9,.5,0))))
    # Upload Task
    dataStars_2[i]=dataStars_2[i]+ifelse(market_data_2$dsd_task_success[data_ind_2[i]]>=.99,.5,0)
    # % UL Through (dsu_effective_upload_test_speed >= 500)/count(dsu_effective_upload_test_speed) -- where dsdu_effective_upload_test_speed not NULL
    percentULThrough=sum(!is.na(tmp_testDat_2$dsu_effective_upload_test_speed)&tmp_testDat_2$dsu_effective_upload_test_speed>=500)/sum(!is.na(tmp_testDat_2$dsd_effective_download_test_speed))
    dataStars_2[i]=dataStars_2[i]+ifelse(percentULThrough>=.97,1,ifelse(percentULThrough>=.92,.5,0))
    
    #UDP Packet Drop Rate mean(udp_packet_drop_rate) -- where test_type_id = 25
    UDP_dat_mean=mean(na.omit(tmp_testDat_2[which(tmp_testDat_2$test_type_id==25),]$udp_avg_packet_drop))
    dataStars_2[i]=dataStars_2[i]+ifelse(UDP_dat_mean<=.05,.5,0)
    
    
    #Calculate Speed and Performance stars for each of the regions
    speedStars_1[i]=speedStars_1[i]+ifelse(market_data_1$dsd_effective_throughput_05p[data_ind_1[i]]>=5000,1.5,ifelse(market_data_1$dsd_effective_throughput_05p[data_ind_1[i]]>=3000,1,ifelse(market_data_1$dsd_effective_throughput_05p[data_ind_1[i]]>=2000,0.5,0)))
    
    speedStars_1[i]=speedStars_1[i]+ifelse(market_data_1$dsd_time_to_first_byte_50p[data_ind_1[i]]<=400,1,ifelse(market_data_1$dsd_time_to_first_byte_50p[data_ind_1[i]]<=700,0.5,0))
    
    speedStars_1[i]=speedStars_1[i]+ifelse(market_data_1$dsd_effective_throughput_95p[data_ind_1[i]]>=75000,.5,0)
    
    speedStars_1[i]=speedStars_1[i]+ifelse(market_data_1$dsu_effective_throughput_05p[data_ind_1[i]]>=1500,1,ifelse(market_data_1$dsu_effective_throughput_05p[data_ind_1[i]]>=1000,0.5,0)) 
    
    liteData95Quant=quantile(na.omit(tmp_testDat_1[which(tmp_testDat_1$test_type_id==26),]$ldrs_task_speed_max),probs=c(.95))
    speedStars_1[i]=speedStars_1[i]+ifelse(liteData95Quant<=1000,.5,0)
    
    MM95Quant=quantile(na.omit(tmp_testDat_1[which(tmp_testDat_1$test_type_id==23 & tmp_testDat_1$flag_access_success=='t'),]$m2mo_total_call_setup_duration),probs=c(.95))
    speedStars_1[i]=speedStars_1[i]+ifelse(MM95Quant<=7000,.5,0)
    
    #Calculate Speed and Performance stars for each of the regions
    speedStars_2[i]=speedStars_2[i]+ifelse(market_data_2$dsd_effective_throughput_05p[data_ind_2[i]]>=5000,1.5,ifelse(market_data_2$dsd_effective_throughput_05p[data_ind_2[i]]>=3000,1,ifelse(market_data_2$dsd_effective_throughput_05p[data_ind_2[i]]>=2000,0.5,0)))
    
    speedStars_2[i]=speedStars_2[i]+ifelse(market_data_2$dsd_time_to_first_byte_50p[data_ind_2[i]]<=400,1,ifelse(market_data_2$dsd_time_to_first_byte_50p[data_ind_2[i]]<=700,0.5,0))
    
    speedStars_2[i]=speedStars_2[i]+ifelse(market_data_2$dsd_effective_throughput_95p[data_ind_2[i]]>=75000,.5,0)
    
    speedStars_2[i]=speedStars_2[i]+ifelse(market_data_2$dsu_effective_throughput_05p[data_ind_2[i]]>=1500,1,ifelse(market_data_2$dsu_effective_throughput_05p[data_ind_2[i]]>=1000,0.5,0)) 
    
    liteData95Quant=quantile(na.omit(tmp_testDat_2[which(tmp_testDat_2$test_type_id==26),]$ldrs_task_speed_max),probs=c(.95))
    speedStars_2[i]=speedStars_2[i]+ifelse(liteData95Quant<=1000,.5,0)
    
    MM95Quant=quantile(na.omit(tmp_testDat_2[which(tmp_testDat_2$test_type_id==23 & tmp_testDat_2$flag_access_success=='t'),]$m2mo_total_call_setup_duration),probs=c(.95))
    speedStars_2[i]=speedStars_2[i]+ifelse(MM95Quant<=7000,.5,0)
  }
  
  callStars_df_1[,carrier_id]=callStars_1
  dataStars_df_1[,carrier_id]=dataStars_1
  speedStars_df_1[,carrier_id]=speedStars_1
  smsStars_df_1[,carrier_id]=smsStars_1
  
  callStars_df_2[,carrier_id]=callStars_2
  dataStars_df_2[,carrier_id]=dataStars_2
  speedStars_df_2[,carrier_id]=speedStars_2
  smsStars_df_2[,carrier_id]=smsStars_2
  
}
stars_overall_1<-round(.8*callStars_df_1+.55*dataStars_df_1+.55*speedStars_df_1+.1*smsStars_df_1,digits=0)/2
stars_overall_2<-round(.8*callStars_df_2+.55*dataStars_df_2+.55*speedStars_df_2+.1*smsStars_df_2,digits=0)/2

#initialize each of the dataframes for the four categories in both halves of 2017
callStarsRank_df_1 =dataStarsRank_df_1=speedStarsRank_df_1=smsStarsRank_df_1=starsRank_overall_1= setNames(data.frame(matrix(0,ncol = 4, nrow = nrow(locat_corr))),carriers)
row.names(callStarsRank_df_1)=row.names(dataStarsRank_df_1)=row.names(speedStarsRank_df_1)=row.names(smsStarsRank_df_1)=row.names(starsRank_overall_1)=locat_corr$id_2017_1
callStarsRank_df_2 =dataStarsRank_df_2=speedStarsRank_df_2=smsStarsRank_df_2=starsRank_overall_2= setNames(data.frame(matrix(0,ncol = 4, nrow = nrow(locat_corr))),carriers)
row.names(callStarsRank_df_2)=row.names(dataStarsRank_df_2)=row.names(speedStarsRank_df_2)=row.names(smsStarsRank_df_2)=row.names(starsRank_overall_2)=locat_corr$id_2017_2
#all rankings
#datastars


### Iterate over all locations and generate the ranking for each of the carriers
tmpMat<-as.matrix(stars_overall_1)
for(i in 1:nrow(tmpMat)){
  vals<-unique(tmpMat[i,],fromLast = TRUE)
  tot<-0
  for(j in rev(order(vals))){
    ind<-which(tmpMat[i,]==vals[j])
    starsRank_overall_1[i,ind]<-1+tot
    tot<-tot+length(ind)
  }
}
tmpMat<-as.matrix(stars_overall_2)
for(i in 1:nrow(tmpMat)){
  vals<-unique(tmpMat[i,],fromLast = TRUE)
  tot<-0
  for(j in rev(order(vals))){
    ind<-which(tmpMat[i,]==vals[j])
    starsRank_overall_2[i,ind]<-1+tot
    tot<-tot+length(ind)
  }
}
tmpMat<-as.matrix(dataStars_df_1)
for(i in 1:nrow(tmpMat)){
  vals<-unique(tmpMat[i,],fromLast = TRUE)
  tot<-0
  for(j in rev(order(vals))){
    ind<-which(tmpMat[i,]==vals[j])
    dataStarsRank_df_1[i,ind]<-1+tot
    tot<-tot+length(ind)
  }
}
tmpMat<-as.matrix(dataStars_df_2)
for(i in 1:nrow(tmpMat)){
  vals<-unique(tmpMat[i,],fromLast = TRUE)
  tot<-0
  for(j in rev(order(vals))){
    ind<-which(tmpMat[i,]==vals[j])
    dataStarsRank_df_2[i,ind]<-1+tot
    tot<-tot+length(ind)
  }
}

tmpMat<-as.matrix(callStars_df_1)
for(i in 1:nrow(tmpMat)){
  vals<-unique(tmpMat[i,],fromLast = TRUE)
  tot<-0
  for(j in rev(order(vals))){
    ind<-which(tmpMat[i,]==vals[j])
    callStarsRank_df_1[i,ind]<-1+tot
    tot<-tot+length(ind)
  }
}
tmpMat<-as.matrix(callStars_df_2)
for(i in 1:nrow(tmpMat)){
  vals<-unique(tmpMat[i,],fromLast = TRUE)
  tot<-0
  for(j in rev(order(vals))){
    ind<-which(tmpMat[i,]==vals[j])
    callStarsRank_df_2[i,ind]<-1+tot
    tot<-tot+length(ind)
  }
}


tmpMat<-as.matrix(smsStars_df_1)
for(i in 1:nrow(tmpMat)){
  vals<-unique(tmpMat[i,],fromLast = TRUE)
  tot<-0
  for(j in rev(order(vals))){
    ind<-which(tmpMat[i,]==vals[j])
    smsStarsRank_df_1[i,ind]<-1+tot
    tot<-tot+length(ind)
  }
}
tmpMat<-as.matrix(smsStars_df_2)
for(i in 1:nrow(tmpMat)){
  vals<-unique(tmpMat[i,],fromLast = TRUE)
  tot<-0
  for(j in rev(order(vals))){
    ind<-which(tmpMat[i,]==vals[j])
    smsStarsRank_df_2[i,ind]<-1+tot
    tot<-tot+length(ind)
  }
}


tmpMat<-as.matrix(speedStars_df_1)
for(i in 1:nrow(tmpMat)){
  vals<-unique(tmpMat[i,],fromLast = TRUE)
  tot<-0
  for(j in rev(order(vals))){
    ind<-which(tmpMat[i,]==vals[j])
    speedStarsRank_df_1[i,ind]<-1+tot
    tot<-tot+length(ind)
  }
}
tmpMat<-as.matrix(speedStars_df_2)
for(i in 1:nrow(tmpMat)){
  vals<-unique(tmpMat[i,],fromLast = TRUE)
  tot<-0
  for(j in rev(order(vals))){
    ind<-which(tmpMat[i,]==vals[j])
    speedStarsRank_df_2[i,ind]<-1+tot
    tot<-tot+length(ind)
  }
}


#initialize each of the dataframes for the four categories in both halves of 2017
callScores_df_1 =dataScores_df_1=speedScores_df_1=smsScores_df_1= setNames(data.frame(matrix(0,ncol = 4, nrow = nrow(locat_corr))),carriers)
row.names(callScores_df_1)=row.names(dataScores_df_1)=row.names(speedScores_df_1)=row.names(smsScores_df_1)=locat_corr$id_2017_1
callScores_df_2 =dataScores_df_2=speedScores_df_2=smsScores_df_2= setNames(data.frame(matrix(0,ncol = 4, nrow = nrow(locat_corr))),carriers)
row.names(callScores_df_2)=row.names(dataScores_df_2)=row.names(speedScores_df_2)=row.names(smsScores_df_2)=locat_corr$id_2017_2

#initialize each of the dataframes for the four categories in both halves of 2017
callScoresRank_df_1 =dataScoresRank_df_1=speedScoresRank_df_1=smsScoresRank_df_1= setNames(data.frame(matrix(0,ncol = 4, nrow = nrow(locat_corr))),carriers)
row.names(callScoresRank_df_1)=row.names(dataScoresRank_df_1)=row.names(speedScoresRank_df_1)=row.names(smsScoresRank_df_1)=locat_corr$id_2017_1
callScoresRank_df_2 =dataScoresRank_df_2=speedScoresRank_df_2=smsScoresRank_df_2= setNames(data.frame(matrix(0,ncol = 4, nrow = nrow(locat_corr))),carriers)
row.names(callScoresRank_df_2)=row.names(dataScoresRank_df_2)=row.names(speedScoresRank_df_2)=row.names(smsScoresRank_df_2)=locat_corr$id_2017_2


for (i in 1:nrow(locat_corr)){
  loc1=locat_corr$id_2017_1[i]
  loc2=locat_corr$id_2017_2[i]
  for (id in 1:length(carriers)){
    callScores_df_1[i,id] = rootscore_data_1$rootscore[which(rootscore_data_1$carrier_id==id & rootscore_data_1$rootscore_index=='Call' & rootscore_data_1$collection_set_id==locat_corr$id_2017_1[i] )]
    callScoresRank_df_1[i,id] = rootscore_data_1$rank[which(rootscore_data_1$carrier_id==id & rootscore_data_1$rootscore_index=='Call' & rootscore_data_1$collection_set_id==locat_corr$id_2017_1[i] )]
    callScores_df_2[i,id] = rootscore_data_2$rootscore[which(rootscore_data_2$carrier_id==id & rootscore_data_2$rootscore_index=='Call' & rootscore_data_2$collection_set_id==locat_corr$id_2017_2[i])]
    callScoresRank_df_2[i,id] = rootscore_data_2$rank[which(rootscore_data_2$carrier_id==id & rootscore_data_2$rootscore_index=='Call' & rootscore_data_2$collection_set_id==locat_corr$id_2017_2[i])]
    
    smsScores_df_1[i,id] = rootscore_data_1$rootscore[which(rootscore_data_1$carrier_id==id & rootscore_data_1$rootscore_index=='Text' & rootscore_data_1$collection_set_id==locat_corr$id_2017_1[i])]
    smsScoresRank_df_1[i,id] = rootscore_data_1$rank[which(rootscore_data_1$carrier_id==id & rootscore_data_1$rootscore_index=='Text' & rootscore_data_1$collection_set_id==locat_corr$id_2017_1[i])]
    smsScores_df_2[i,id] = rootscore_data_2$rootscore[which(rootscore_data_2$carrier_id==id & rootscore_data_2$rootscore_index=='Text' & rootscore_data_2$collection_set_id==locat_corr$id_2017_2[i])]
    smsScoresRank_df_2[i,id] = rootscore_data_2$rank[which(rootscore_data_2$carrier_id==id & rootscore_data_2$rootscore_index=='Text' & rootscore_data_2$collection_set_id==locat_corr$id_2017_2[i])]
    
    dataScores_df_1[i,id] = rootscore_data_1$rootscore[which(rootscore_data_1$carrier_id==id & rootscore_data_1$rootscore_index=='Data' & rootscore_data_1$collection_set_id==locat_corr$id_2017_1[i])]
    dataScoresRank_df_1[i,id] = rootscore_data_1$rank[which(rootscore_data_1$carrier_id==id & rootscore_data_1$rootscore_index=='Data' & rootscore_data_1$collection_set_id==locat_corr$id_2017_1[i])]
    dataScores_df_2[i,id] = rootscore_data_2$rootscore[which(rootscore_data_2$carrier_id==id & rootscore_data_2$rootscore_index=='Data' & rootscore_data_2$collection_set_id==locat_corr$id_2017_2[i])]
    dataScoresRank_df_2[i,id] = rootscore_data_2$rank[which(rootscore_data_2$carrier_id==id & rootscore_data_2$rootscore_index=='Data' & rootscore_data_2$collection_set_id==locat_corr$id_2017_2[i])]
    
    speedScores_df_1[i,id] = rootscore_data_1$rootscore[which(rootscore_data_1$carrier_id==id & rootscore_data_1$rootscore_index=='Speed' & rootscore_data_1$collection_set_id==locat_corr$id_2017_1[i])]
    speedScoresRank_df_1[i,id] = rootscore_data_1$rank[which(rootscore_data_1$carrier_id==id & rootscore_data_1$rootscore_index=='Speed' & rootscore_data_1$collection_set_id==locat_corr$id_2017_1[i])]
    speedScores_df_2[i,id] = rootscore_data_2$rootscore[which(rootscore_data_2$carrier_id==id & rootscore_data_2$rootscore_index=='Speed' & rootscore_data_2$collection_set_id==locat_corr$id_2017_2[i])]
    speedScoresRank_df_2[i,id] = rootscore_data_2$rank[which(rootscore_data_2$carrier_id==id & rootscore_data_2$rootscore_index=='Speed' & rootscore_data_2$collection_set_id==locat_corr$id_2017_2[i])]
    
  }
}
  
col_mat = matrix(c(0,159,219, 251,223,0, 227,0,116,236,7,16), # the data elements 
                 nrow=4,ncol=3, byrow = TRUE)
 
########## Plot the RootStars Raw
for(i in 1:length(carriers)){
  
  png(filename = paste("Plots/1H/speed_star_raw_",carriers[i],'_1H2017.png'))
  hist(speedStars_df_1[,i],breaks = seq(0,5, .5),xlab='RootStars',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootStars for ',carriers[i],' 1H 2017 in Speed'))
  dev.off()
  png(filename = paste("Plots/1H/call_star_raw_",carriers[i],'_1H2017.png'))
  hist(callStars_df_1[,i],breaks = seq(0,5,.5),xlab='RootStars',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootStars for ',carriers[i],' 1H 2017 in Call'))
  dev.off()
  png(filename = paste("Plots/1H/data_star_raw_",carriers[i],'_1H2017.png'))
  hist(dataStars_df_1[,i],breaks = seq(0,5,.5),xlab='RootStars',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootStars for ',carriers[i],' 1H 2017 in Data'))
  dev.off()
  png(filename = paste("Plots/1H/sms_star_raw_",carriers[i],'_1H2017.png'))
  hist(smsStars_df_1[,i],breaks = seq(0,5,.5),xlab='RootStars',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootStars for ',carriers[i],' 1H 2017 in SMS'))
  dev.off()
  
  png(filename = paste("Plots/2H/speed_star_raw_",carriers[i],'_2H2017.png'))
  hist(speedStars_df_2[,i],breaks = seq(0,5,.5),xlab='RootStars',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootStars for ',carriers[i],' 2H 2017 in Speed'))
  dev.off()
  png(filename = paste("Plots/2H/call_star_raw_",carriers[i],'_2H2017.png'))
  hist(callStars_df_1[,i],breaks = seq(0,5,.5),xlab='RootStars',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootStars for ',carriers[i],' 2H 2017 in Call'))
  dev.off()
  png(filename = paste("Plots/2H/data_star_raw_",carriers[i],'_2H2017.png'))
  hist(dataStars_df_1[,i],breaks = seq(0,5,.5),xlab='RootStars',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootStars for ',carriers[i],' 2H 2017 in Data'))
  dev.off()
  png(filename = paste("Plots/2H/sms_star_raw_",carriers[i],'_2H2017.png'))
  hist(smsStars_df_1[,i],breaks = seq(0,5,.5),xlab='RootStars',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootStars for ',carriers[i],' 2H 2017 in SMS'))
  dev.off()
}


 ########## Plot the RootStars Rank (position each carrier came in over all locations)
 for(i in 1:length(carriers)){
   
   png(filename = paste("Plots/1H/speed_star_rank_",carriers[i],'_1H2017.png'))
   hist(speedStarsRank_df_1[,i],breaks = seq(0,4,1),xlab='RootStar Positions',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootStar Positions for ',carriers[i],' 1H 2017 in Speed'))
   dev.off()
   png(filename = paste("Plots/1H/call_star_rank_",carriers[i],'_1H2017.png'))
   hist(callStarsRank_df_1[,i],breaks = seq(0,4,1),xlab='RootStar Positions',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootStar Positions for ',carriers[i],' 1H 2017 in Call'))
   dev.off()
   png(filename = paste("Plots/1H/data_star_rank_",carriers[i],'_1H2017.png'))
   hist(dataStarsRank_df_1[,i],breaks = seq(0,4,1),xlab='RootStar Positions',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootStar Positions for ',carriers[i],' 1H 2017 in Data'))
   dev.off()
   png(filename = paste("Plots/1H/sms_star_rank_",carriers[i],'_1H2017.png'))
   hist(smsStarsRank_df_1[,i],breaks = seq(0,4,1),xlab='RootStar Positions',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootStar Positions for ',carriers[i],' 1H 2017 in SMS'))
   dev.off()
   
   png(filename = paste("Plots/2H/speed_star_rank_",carriers[i],'_2H2017.png'))
   hist(speedStarsRank_df_2[,i],breaks = seq(0,4,1),xlab='RootStar Positions',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootStar Positions for ',carriers[i],' 2H 2017 in Speed'))
   dev.off()
   png(filename = paste("Plots/2H/call_star_rank_",carriers[i],'_2H2017.png'))
   hist(callStarsRank_df_1[,i],breaks = seq(0,4,1),xlab='RootStar Positions',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootStar Positions for ',carriers[i],' 2H 2017 in Call'))
   dev.off()
   png(filename = paste("Plots/2H/data_star_rank_",carriers[i],'_2H2017.png'))
   hist(dataStarsRank_df_1[,i],breaks = seq(0,4,1),xlab='RootStar Positions',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootStar Positions for ',carriers[i],' 2H 2017 in Data'))
   dev.off()
   png(filename = paste("Plots/2H/sms_star_rank_",carriers[i],'_2H2017.png'))
   hist(smsStarsRank_df_1[,i],breaks = seq(0,4,1),xlab='RootStar Positions',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootStar Positions for ',carriers[i],' 2H 2017 in SMS'))
   dev.off()
 }

########## Plot the RootScores Raw
for(i in 1:length(carriers)){
  
  png(filename = paste("Plots/1H/speed_score_raw_",carriers[i],'_1H2017.png'))
  hist(speedScores_df_1[which(speedScores_df_1[,i]>60),i],breaks = seq(60,100,.5),xlab='RootScores',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootScores for ',carriers[i],' 1H 2017 in Speed'))
  dev.off()
  png(filename = paste("Plots/1H/call_score_raw_",carriers[i],'_1H2017.png'))
  hist(callScores_df_1[which(callScores_df_1[,i]>60),i],breaks = seq(60,100,.5),xlab='RootScores',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootScores for ',carriers[i],' 1H 2017 in Call'))
  dev.off()
  png(filename = paste("Plots/1H/data_score_raw_",carriers[i],'_1H2017.png'))
  hist(dataScores_df_1[which(dataScores_df_1[,i]>60),i],breaks = seq(60,100,.5),xlab='RootScores',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootScores for ',carriers[i],' 1H 2017 in Data'))
  dev.off()
  png(filename = paste("Plots/1H/sms_score_raw_",carriers[i],'_1H2017.png'))
  hist(smsScores_df_1[which(smsScores_df_1[,i]>60),i],breaks = seq(60,100,.5),xlab='RootScores',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootScores for ',carriers[i],' 1H 2017 in SMS'))
  dev.off()
  
  png(filename = paste("Plots/2H/speed_score_raw_",carriers[i],'_2H2017.png'))
  hist(speedScores_df_2[which(speedScores_df_2[,i]>60),i],breaks = seq(60,100,.5),xlab='RootScores',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootScores for ',carriers[i],' 2H 2017 in Speed'))
  dev.off()
  png(filename = paste("Plots/2H/call_score_raw_",carriers[i],'_2H2017.png'))
  hist(callScores_df_1[which(callScores_df_1[,i]>60),i],breaks = seq(60,100,.5),xlab='RootScores',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootScores for ',carriers[i],' 2H 2017 in Call'))
  dev.off()
  png(filename = paste("Plots/2H/data_score_raw_",carriers[i],'_2H2017.png'))
  hist(dataScores_df_1[which(dataScores_df_1[,i]>60),i],breaks = seq(60,100,.5),xlab='RootScores',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootScores for ',carriers[i],' 2H 2017 in Data'))
  dev.off()
  png(filename = paste("Plots/2H/sms_score_raw_",carriers[i],'_2H2017.png'))
  hist(smsScores_df_1[which(smsScores_df_1[,i]>60),i],breaks = seq(60,100,.5),xlab='RootScores',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootScores for ',carriers[i],' 2H 2017 in SMS'))
  dev.off()
}

########## Plot the RootStars Rank (position each carrier came in over all locations)
for(i in 1:length(carriers)){
  
  png(filename = paste("Plots/1H/speed_score_rank_",carriers[i],'_1H2017.png'))
  hist(speedScoresRank_df_1[,i],breaks = seq(.5,4.5,1),xlab='RootScore Positions',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootScore Positions for ',carriers[i],' 1H 2017 in Speed'))
  dev.off()
  png(filename = paste("Plots/1H/call_score_rank_",carriers[i],'_1H2017.png'))
  hist(callScoresRank_df_1[,i],breaks = seq(0.5,4.5,1),xlab='RootScore Positions',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootScore Positions for ',carriers[i],' 1H 2017 in Call'))
  dev.off()
  png(filename = paste("Plots/1H/data_score_rank_",carriers[i],'_1H2017.png'))
  hist(dataScoresRank_df_1[,i],breaks = seq(0.5,4.5,1),xlab='RootScore Positions',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootScore Positions for ',carriers[i],' 1H 2017 in Data'))
  dev.off()
  png(filename = paste("Plots/1H/sms_score_rank_",carriers[i],'_1H2017.png'))
  hist(smsScoresRank_df_1[,i],breaks = seq(0.5,4.5,1),xlab='RootScore Positions',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootScore Positions for ',carriers[i],' 1H 2017 in SMS'))
  dev.off()
  
  png(filename = paste("Plots/2H/speed_score_rank_",carriers[i],'_2H2017.png'))
  hist(speedScoresRank_df_2[,i],breaks = seq(0.5,4.5,1),xlab='RootScore Positions',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootScore Positions for ',carriers[i],' 2H 2017 in Speed'))
  dev.off()
  png(filename = paste("Plots/2H/call_score_rank_",carriers[i],'_2H2017.png'))
  hist(callScoresRank_df_1[,i],breaks = seq(0.5,4.5,1),xlab='RootScore Positions',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootScore Positions for ',carriers[i],' 2H 2017 in Call'))
  dev.off()
  png(filename = paste("Plots/2H/data_score_rank_",carriers[i],'_2H2017.png'))
  hist(dataScoresRank_df_1[,i],breaks = seq(0.5,4.5,1),xlab='RootScore Positions',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootScore Positions for ',carriers[i],' 2H 2017 in Data'))
  dev.off()
  png(filename = paste("Plots/2H/sms_score_rank_",carriers[i],'_2H2017.png'))
  hist(smsScoresRank_df_1[,i],breaks = seq(0.5,4.5,1),xlab='RootScore Positions',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootScore Positions for ',carriers[i],' 2H 2017 in SMS'))
  dev.off()
}
 
 
 #### create histograms for the change in stars from 1H to 2H 2017
 for(i in 1:length(carriers)){
   
   png(filename = paste("Plots/Change/speed_star_raw_",carriers[i],'_compare2017.png'))
   hist(speedStars_df_2[,i]-speedStars_df_1[,i],breaks = seq(-5.5,5.5, .5),xlab='Change RootStars',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('Change in RootStars for ',carriers[i],' From 1H to 2H 2017 in Speed'))
   dev.off()
   png(filename = paste("Plots/Change/call_star_raw_",carriers[i],'_compare2017.png'))
   hist(callStars_df_2[,i]-callStars_df_1[,i],breaks = seq(-5.5,5.5,.5),xlab='Change RootStars',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('Change in RootStars for ',carriers[i],' From 1H to 2H 2017 in Call'))
   dev.off()
   png(filename = paste("Plots/Change/data_star_raw_",carriers[i],'_compare2017.png'))
   hist(dataStars_df_2[,i]-dataStars_df_1[,i],breaks = seq(-5.5,5.5,.5),xlab='Change RootStars',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('Change in RootStars for ',carriers[i],' From 1H to 2H 2017 in Data'))
   dev.off()
   png(filename = paste("Plots/Change/sms_star_raw_",carriers[i],'_compare2017.png'))
   hist(smsStars_df_2[,i]-smsStars_df_1[,i],breaks = seq(-5.5,5.5,.5),xlab='Change RootStars',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('Change in RootStars for ',carriers[i],' From 1H to 2H 2017 in SMS'))
   dev.off()
 }
 
 
 #### create histograms for the change in stars Position from 1H to 2H 2017
 ########## Plot the RootStars Position Change
 for(i in 1:length(carriers)){
   
   png(filename = paste("Plots/Change/Rank/speed_star_rank_",carriers[i],'_compare2017.png'))
   hist(speedStarsRank_df_2[,i]-speedStarsRank_df_1[,i],breaks = seq(-3.5,3.5, 1),xlab='Change in RootStars Positions',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('Change in RootStars Positions for ',carriers[i],' From 1H to 2H 2017 in Speed'))
   dev.off()
   png(filename = paste("Plots/Change/Rank/call_star_rank_",carriers[i],'_compare2017.png'))
   hist(callStarsRank_df_2[,i]-callStarsRank_df_1[,i],breaks = seq(-3.5,3.5, 1),xlab='Change in RootStars Positions',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('Change in RootStars Positions for ',carriers[i],' From 1H to 2H 2017 in Call'))
   dev.off()
   png(filename = paste("Plots/Change/Rank/data_star_rank_",carriers[i],'_compare2017.png'))
   hist(dataStarsRank_df_2[,i]-dataStarsRank_df_1[,i],breaks = seq(-3.5,3.5, 1),xlab='Change in RootStars Positions',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('Change in RootStars Positions for ',carriers[i],' From 1H to 2H 2017 in Data'))
   dev.off()
   png(filename = paste("Plots/Change/Rank/sms_star_rank_",carriers[i],'_compare2017.png'))
   hist(smsStarsRank_df_2[,i]-smsStarsRank_df_1[,i],breaks = seq(-3.5,3.5, 1),xlab='Change in RootStars Positions',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('Change in RootStars Positions for ',carriers[i],' From 1H to 2H 2017 in SMS'))
   dev.off()
 }
 
 
 ########## Plot the the change in RootScores from 1H - 2H 2017
 for(i in 1:length(carriers)){
   
   png(filename = paste("Plots/Change/speed_score_raw_",carriers[i],'_compare2017.png'))
   hist((speedScores_df_2[,i]-speedScores_df_1[,i])[which(abs(speedScores_df_2[,i]-speedScores_df_1[,i])<15)],breaks = seq(-15,15, 1),xlab='Change RootScores',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('Change in RootScores for ',carriers[i],' From 1H to 2H 2017 in Speed'))
   dev.off()
   png(filename = paste("Plots/Change/call_score_raw_",carriers[i],'_compare2017.png'))
   hist((callScores_df_2[,i]-callScores_df_1[,i])[which(abs(callScores_df_2[,i]-callScores_df_1[,i])<15)],breaks = seq(-15,15,1),xlab='Change RootScores',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('Change in RootScores for ',carriers[i],' From 1H to 2H 2017 in Call'))
   dev.off()
   png(filename = paste("Plots/Change/data_score_raw_",carriers[i],'_compare2017.png'))
   hist((dataScores_df_2[,i]-dataScores_df_1[,i])[which(abs(dataScores_df_2[,i]-dataScores_df_1[,i])<15)],breaks = seq(-15,15,1),xlab='Change RootScores',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('Change in RootScores for ',carriers[i],' From 1H to 2H 2017 in Data'))
   dev.off()
   png(filename = paste("Plots/Change/sms_score_raw_",carriers[i],'_compare2017.png'))
   hist((smsScores_df_2[,i]-smsScores_df_1[,i])[which(abs(smsScores_df_2[,i]-smsScores_df_1[,i])<15)],breaks = seq(-15,15,1),xlab='Change RootScores',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('Change in RootScores for ',carriers[i],' From 1H to 2H 2017 in SMS'))
   dev.off()
 }
 
 
 #### create histograms for the change in scores Position from 1H to 2H 2017
 ########## Plot the RootScores Position Change
 for(i in 1:length(carriers)){
   
   png(filename = paste("Plots/Change/Rank/speed_score_rank_",carriers[i],'_compare2017.png'))
   hist(speedScoresRank_df_2[,i]-speedScoresRank_df_1[,i],breaks = seq(-3.5,3.5, 1),xlab='Change in RootScores Positions',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('Change in RootScores Positions for ',carriers[i],' From 1H to 2H 2017 in Speed'))
   dev.off()
   png(filename = paste("Plots/Change/Rank/call_score_rank_",carriers[i],'_compare2017.png'))
   hist(callScoresRank_df_2[,i]-callScoresRank_df_1[,i],breaks = seq(-3.5,3.5, 1),xlab='Change in RootScores Positions',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('Change in RootScores Positions for ',carriers[i],' From 1H to 2H 2017 in Call'))
   dev.off()
   png(filename = paste("Plots/Change/Rank/data_score_rank_",carriers[i],'_compare2017.png'))
   hist(dataScoresRank_df_2[,i]-dataScoresRank_df_1[,i],breaks = seq(-3.5,3.5, 1),xlab='Change in RootScores Positions',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('Change in RootScores Positions for ',carriers[i],' From 1H to 2H 2017 in Data'))
   dev.off()
   png(filename = paste("Plots/Change/Rank/sms_score_rank_",carriers[i],'_compare2017.png'))
   hist(smsScoresRank_df_2[,i]-smsScoresRank_df_1[,i],breaks = seq(-3.5,3.5, 1),xlab='Change in RootScores Positions',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('Change in RootScores Positions for ',carriers[i],' From 1H to 2H 2017 in SMS'))
   dev.off()
 }
 
 
 
 ########## Plot the RootScores as histograms
 for(i in 1:length(carriers)){
   
   png(filename = paste("Plots/1H/speed_score_raw_",carriers[i],'_1H2017.png'))
   hist(speedScores_df_1[which(speedScores_df_1[,i]>60),i],breaks = seq(60,100,.5),xlab='RootScores',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootScores for ',carriers[i],' 1H 2017 in Speed'))
   dev.off()
   png(filename = paste("Plots/1H/call_score_raw_",carriers[i],'_1H2017.png'))
   hist(callScores_df_1[which(callScores_df_1[,i]>60),i],breaks = seq(60,100,.5),xlab='RootScores',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootScores for ',carriers[i],' 1H 2017 in Call'))
   dev.off()
   png(filename = paste("Plots/1H/data_score_raw_",carriers[i],'_1H2017.png'))
   hist(dataScores_df_1[which(dataScores_df_1[,i]>60),i],breaks = seq(60,100,.5),xlab='RootScores',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootScores for ',carriers[i],' 1H 2017 in Data'))
   dev.off()
   png(filename = paste("Plots/1H/sms_score_raw_",carriers[i],'_1H2017.png'))
   hist(smsScores_df_1[which(smsScores_df_1[,i]>60),i],breaks = seq(60,100,.5),xlab='RootScores',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootScores for ',carriers[i],' 1H 2017 in SMS'))
   dev.off()
   
   png(filename = paste("Plots/2H/speed_score_raw_",carriers[i],'_2H2017.png'))
   hist(speedScores_df_2[which(speedScores_df_2[,i]>60),i],breaks = seq(60,100,.5),xlab='RootScores',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootScores for ',carriers[i],' 2H 2017 in Speed'))
   dev.off()
   png(filename = paste("Plots/2H/call_score_raw_",carriers[i],'_2H2017.png'))
   hist(callScores_df_1[which(callScores_df_1[,i]>60),i],breaks = seq(60,100,.5),xlab='RootScores',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootScores for ',carriers[i],' 2H 2017 in Call'))
   dev.off()
   png(filename = paste("Plots/2H/data_score_raw_",carriers[i],'_2H2017.png'))
   hist(dataScores_df_1[which(dataScores_df_1[,i]>60),i],breaks = seq(60,100,.5),xlab='RootScores',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootScores for ',carriers[i],' 2H 2017 in Data'))
   dev.off()
   png(filename = paste("Plots/2H/sms_score_raw_",carriers[i],'_2H2017.png'))
   hist(smsScores_df_1[which(smsScores_df_1[,i]>60),i],breaks = seq(60,100,.5),xlab='RootScores',col=rgb(col_mat[i,1],col_mat[i,2],col_mat[i,3],maxColorValue = 255),main = paste('RootScores for ',carriers[i],' 2H 2017 in SMS'))
   dev.off()
 }
 


#### create histograms for each of the categories accross all carriers/locations

call_hist=matrix(0,nrow = 4,ncol = 21)
sms_hist=matrix(0,nrow = 4,ncol = 21)
data_hist=matrix(0,nrow = 4,ncol = 21)
speed_hist=matrix(0,nrow = 4,ncol = 21)
for(carrier_id in 1:length(carriers)){
  carrier=carriers[carrier_id]#carrier name
  for (i in 1:nrow(locat_corr)){
    call_hist[carrier_id,((callStars_df_2[i,carrier_id]-callStars_df_1[i,carrier_id]+5.5)/.5)]=call_hist[carrier_id,((callStars_df_2[i,carrier_id]-callStars_df_1[i,carrier_id]+5.5)/.5)]+1
    sms_hist[carrier_id,((smsStars_df_2[i,carrier_id]-smsStars_df_1[i,carrier_id]+5.5)/.5)]=sms_hist[carrier_id,((smsStars_df_2[i,carrier_id]-smsStars_df_1[i,carrier_id]+5.5)/.5)]+1
    data_hist[carrier_id,((dataStars_df_2[i,carrier_id]-dataStars_df_1[i,carrier_id]+5.5)/.5)]=data_hist[carrier_id,((dataStars_df_2[i,carrier_id]-dataStars_df_1[i,carrier_id]+5.5)/.5)]+1
    speed_hist[carrier_id,((speedStars_df_2[i,carrier_id]-speedStars_df_1[i,carrier_id]+5.5)/.5)]=speed_hist[carrier_id,((speedStars_df_2[i,carrier_id]-speedStars_df_1[i,carrier_id]+5.5)/.5)]+1
  }
}

colnames(call_hist)=colnames(speed_hist)=colnames(data_hist)=colnames(sms_hist)= seq(-5,5,.5)
rownames(call_hist)=rownames(speed_hist)=rownames(data_hist)=rownames(sms_hist)=carriers

#Save data
setwd('documents/Consulting/Rootmetrics')

## save the RootStar values from 1H to 2H of 2017
write.csv(callStars_df_1,"Data/1H/call_star_raw_1H2017.csv")
write.csv(dataStars_df_1,"Data/1H/data_star_raw_1H2017.csv")
write.csv(smsStars_df_1,"Data/1H/sms_star_raw_1H2017.csv")
write.csv(speedStars_df_1,"Data/1H/speed_star_raw_1H2017.csv")
write.csv(callStars_df_2,"Data/2H/call_star_raw_2H2017.csv")
write.csv(dataStars_df_2,"Data/2H/data_star_raw_2H2017.csv")
write.csv(smsStars_df_2,"Data/2H/sms_star_raw_2H2017.csv")
write.csv(speedStars_df_2,"Data/2H/speed_star_raw_2H2017.csv")

## save the RootStar Rank values from 1H and 2H of 2017
write.csv(callStarsRank_df_1,"Data/1H/call_star_rank_1H2017.csv")
write.csv(dataStarsRank_df_1,"Data/1H/data_star_rank_1H2017.csv")
write.csv(smsStarsRank_df_1,"Data/1H/sms_star_rank_1H2017.csv")
write.csv(speedStarsRank_df_1,"Data/1H/speed_star_rank_1H2017.csv")
write.csv(callStarsRank_df_2,"Data/2H/call_star_rank_2H2017.csv")
write.csv(dataStarsRank_df_2,"Data/2H/data_star_rank_2H2017.csv")
write.csv(smsStarsRank_df_2,"Data/2H/sms_star_rank_2H2017.csv")
write.csv(speedStarsRank_df_2,"Data/2H/speed_star_rank_2H2017.csv")

## save the RootScore values from 1H to 2H of 2017
write.csv(callScores_df_1,"Data/1H/call_score_raw_1H2017.csv")
write.csv(dataScores_df_1,"Data/1H/data_score_raw_1H2017.csv")
write.csv(smsScores_df_1,"Data/1H/sms_score_raw_1H2017.csv")
write.csv(speedScores_df_1,"Data/1H/speed_score_raw_1H2017.csv")
write.csv(callScores_df_2,"Data/2H/call_score_raw_2H2017.csv")
write.csv(dataScores_df_2,"Data/2H/data_score_raw_2H2017.csv")
write.csv(smsScores_df_2,"Data/2H/sms_score_raw_2H2017.csv")
write.csv(speedScores_df_2,"Data/2H/speed_score_raw_2H2017.csv")

## save the RootScore Rank values from 1H to 2H of 2017
write.csv(callScoresRank_df_1,"Data/1H/call_score_rank_1H2017.csv")
write.csv(dataScoresRank_df_1,"Data/1H/data_score_rank_1H2017.csv")
write.csv(smsScoresRank_df_1,"Data/1H/sms_score_rank_1H2017.csv")
write.csv(speedScoresRank_df_1,"Data/1H/speed_score_rank_1H2017.csv")
write.csv(callScoresRank_df_2,"Data/2H/call_score_rank_2H2017.csv")
write.csv(dataScoresRank_df_2,"Data/2H/data_score_rank_2H2017.csv")
write.csv(smsScoresRank_df_2,"Data/2H/sms_score_rank_2H2017.csv")
write.csv(speedScoresRank_df_2,"Data/2H/speed_score_rank_2H2017.csv")


### save the change from 1half to 2half of 2017 in each of the categories
write.csv(callStars_df_2-callStars_df_1,"Data/Change/call_stars_raw_change2017.csv")
write.csv(smsStars_df_2-smsStars_df_1,"Data/Change/sms_stars_raw_change2017.csv")
write.csv(dataStars_df_2-dataStars_df_1,"Data/Change/data_stars_raw_change2017.csv")
write.csv(speedStars_df_2-speedStars_df_1,"Data/Change/speed_stars_raw_change2017.csv")

write.csv(callStarsRank_df_2-callStarsRank_df_1,"Data/Change/call_stars_rank_change2017.csv")
write.csv(smsStarsRank_df_2-smsStarsRank_df_1,"Data/Change/sms_stars_rank_change2017.csv")
write.csv(dataStarsRank_df_2-dataStarsRank_df_1,"Data/Change/data_stars_rank_change2017.csv")
write.csv(speedStarsRank_df_2-speedStarsRank_df_1,"Data/Change/speed_stars_rank_change2017.csv")

write.csv(callScores_df_2-callScores_df_1,"Data/Change/call_score_raw_change2017.csv")
write.csv(smsScores_df_2-smsScores_df_1,"Data/Change/sms_score_raw_change2017.csv")
write.csv(dataScores_df_2-dataScores_df_1,"Data/Change/data_score_raw_change2017.csv")
write.csv(speedScores_df_2-speedScores_df_1,"Data/Change/speed_score_raw_change2017.csv")

write.csv(callScoresRank_df_2-callScoresRank_df_1,"Data/Change/call_score_rank_change2017.csv")
write.csv(smsScoresRank_df_2-smsScoresRank_df_1,"Data/Change/sms_score_rank_change2017.csv")
write.csv(dataScoresRank_df_2-dataScoresRank_df_1,"Data/Change/data_score_rank_change2017.csv")
write.csv(speedScoresRank_df_2-speedScoresRank_df_1,"Data/Change/speed_score_rank_change2017.csv")

write.csv(t(call_hist),"Data/Change/call_stars_raw_hist_change2017.csv")
write.csv(t(sms_hist),"Data/Change/sms_stars_raw_hist_change2017.csv")
write.csv(t(data_hist),"Data/Change/data_stars_raw_hist_change2017.csv")
write.csv(t(speed_hist),"Data/Change/speed_stars_raw_hist_change2017.csv")



type='l'

png(filename = paste("Plots/2H/sms_score_raw_",carriers[i],'_2H2017.png'))
### plot call category for all carriers
plot(seq(-5,5,.5),call_hist[1,],type=type,lwd=3,col='red',ylim=c(0,80),
     main='Change In Call Star Ranking From 1H To 2H of 2017',
    xlab="Change in Stars",
    ylab="Frequency")
sum(seq(-5,5,.5)*call_hist[1,])/sum(call_hist[1,])
carr1avg=round(sum(seq(-5,5,.5)*call_hist[1,])/sum(call_hist[1,]),digits=2)
points(seq(-5,5,.5),call_hist[2,],type=type,lwd=3,col='blue')
carr2avg=round(sum(seq(-5,5,.5)*call_hist[2,])/sum(call_hist[2,]),digits=2)
points(seq(-5,5,.5),call_hist[3,],type=type,lwd=3,col='green')
carr3avg=round(sum(seq(-5,5,.5)*call_hist[3,])/sum(call_hist[3,]),digits=2)
points(seq(-5,5,.5),call_hist[4,],type=type,lwd=3,col='black')
carr4avg=round(sum(seq(-5,5,.5)*call_hist[4,])/sum(call_hist[4,]),digits=2)

change=c(carr1avg,carr2avg,carr3avg,carr4avg)
legend_labs=c('','','','')
for (i in 1:length(carriers)){
  legend_labs[i] = paste(c(carriers[i],change[i]),collapse = ', Avg Chg:')
}
legend("topleft",legend = paste(legend_labs),col =c("red","blue","green","black"), lty=1, lwd=5,cex=0.8)


### plot sms category for all carriers
plot(seq(-5,5,.5),sms_hist[1,],type=type,lwd=3,col='red',ylim=c(0,80),
     main='Change In SMS Star Ranking From 1H To 2H of 2017',
     xlab="Change in Stars",
     ylab="Frequency")
sum(seq(-5,5,.5)*sms_hist[1,])/sum(sms_hist[1,])
carr1avg=round(sum(seq(-5,5,.5)*sms_hist[1,])/sum(sms_hist[1,]),digits=2)
points(seq(-5,5,.5),sms_hist[2,],type=type,lwd=3,col='blue')
carr2avg=round(sum(seq(-5,5,.5)*sms_hist[2,])/sum(sms_hist[2,]),digits=2)
points(seq(-5,5,.5),sms_hist[3,],type=type,lwd=3,col='green')
carr3avg=round(sum(seq(-5,5,.5)*sms_hist[3,])/sum(sms_hist[3,]),digits=2)
points(seq(-5,5,.5),sms_hist[4,],type=type,lwd=3,col='black')
carr4avg=round(sum(seq(-5,5,.5)*sms_hist[4,])/sum(sms_hist[4,]),digits=2)

change=c(carr1avg,carr2avg,carr3avg,carr4avg)
legend_labs=c('','','','')
for (i in 1:length(carriers)){
  legend_labs[i] = paste(c(carriers[i],change[i]),collapse = ', Avg Chg:')
}
legend("topleft",legend = paste(legend_labs),col =c("red","blue","green","black"), lty=1, lwd=5,cex=0.8)




### plot data category for all carriers
plot(seq(-5,5,.5),data_hist[1,],type=type,lwd=3,col='red',ylim=c(0,80),
     main='Change In Data Star Ranking From 1H To 2H of 2017',
     xlab="Change in Stars",
     ylab="Frequency")
sum(seq(-5,5,.5)*data_hist[1,])/sum(data_hist[1,])
carr1avg=round(sum(seq(-5,5,.5)*data_hist[1,])/sum(data_hist[1,]),digits=2)
points(seq(-5,5,.5),data_hist[2,],type=type,lwd=3,col='blue')
carr2avg=round(sum(seq(-5,5,.5)*data_hist[2,])/sum(data_hist[2,]),digits=2)
points(seq(-5,5,.5),data_hist[3,],type=type,lwd=3,col='green')
carr3avg=round(sum(seq(-5,5,.5)*data_hist[3,])/sum(data_hist[3,]),digits=2)
points(seq(-5,5,.5),data_hist[4,],type=type,lwd=3,col='black')
carr4avg=round(sum(seq(-5,5,.5)*data_hist[4,])/sum(data_hist[4,]),digits=2)

change=c(carr1avg,carr2avg,carr3avg,carr4avg)
legend_labs=c('','','','')
for (i in 1:length(carriers)){
  legend_labs[i] = paste(c(carriers[i],change[i]),collapse = ', Avg Chg:')
}
legend("topleft",legend = paste(legend_labs),col =c("red","blue","green","black"), lty=1, lwd=5,cex=0.8)



### plot speed category for all carriers
plot(seq(-5,5,.5),speed_hist[1,],type=type,lwd=3,col='red',ylim=c(0,80),
     main='Change In Speed Star Ranking From 1H To 2H of 2017',
     xlab="Change in Stars",
     ylab="Frequency")
sum(seq(-5,5,.5)*speed_hist[1,])/sum(speed_hist[1,])
carr1avg=round(sum(seq(-5,5,.5)*speed_hist[1,])/sum(speed_hist[1,]),digits=2)
points(seq(-5,5,.5),speed_hist[2,],type=type,lwd=3,col='blue')
carr2avg=round(sum(seq(-5,5,.5)*speed_hist[2,])/sum(speed_hist[2,]),digits=2)
points(seq(-5,5,.5),speed_hist[3,],type=type,lwd=3,col='green')
carr3avg=round(sum(seq(-5,5,.5)*speed_hist[3,])/sum(speed_hist[3,]),digits=2)
points(seq(-5,5,.5),speed_hist[4,],type=type,lwd=3,col='black')
carr4avg=round(sum(seq(-5,5,.5)*speed_hist[4,])/sum(speed_hist[4,]),digits=2)

change=c(carr1avg,carr2avg,carr3avg,carr4avg)
legend_labs=c('','','','')
for (i in 1:length(carriers)){
  legend_labs[i] = paste(c(carriers[i],change[i]),collapse = ', Avg Chg:')
}
legend("topleft",legend = paste(legend_labs),col =c("red","blue","green","black"), lty=1, lwd=5,cex=0.8)

