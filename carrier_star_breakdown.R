#  John's Path
# market_data<-read.csv("C:/Users/john_allen/Documents/cofc/Operations Research/data for root metrics/rating_data/rating_data/market_report_sets_ratings_2H2017.csv",header=T)
# test_data<-read.csv("C:/Users/john_allen/Documents/cofc/Operations Research/data for root metrics/rating_data/rating_data/test_summary_ratings_2h2017.csv",header=T)
# rootscore_data<-read.csv('C:/Users/john_allen/Documents/cofc/Operations Research/data for root metrics/rating_data/rating_data/rootscore_ranks_2H2017.csv',header = T)
#  Austin's Path

which_half = '2H2017'
save_directory = "Data/within_category_carrier_compare/"
save=TRUE

market_data<-read.csv(paste(c("rating_data/rating_data/market_report_sets_ratings_",which_half,'.csv'),collapse = ''),header=T)
test_data<-read.csv(paste(c("test_summary_data/test_summary_ratings_",which_half,'.csv'),collapse = ''),header=T)
rootscore_data<-read.csv(paste(c('rating_data/rating_data/rootscore_ranks_',which_half,'.csv'),collapse = ''),header = T)
collection_sets<-read.csv(paste(c('rating_data/rating_data/collection_sets_',which_half,'.csv'),collapse = ''),header = T)



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

call_df = data.frame(matrix(0,nrow = 3,ncol=4))
data_df = sms_df = speed_df = data.frame(matrix(0,nrow = 6,ncol=4))

colnames(call_df)=colnames(data_df)=colnames(sms_df)=colnames(speed_df)=carriers
rownames(call_df) = c('co_drop','co_block','m2mo_block')
rownames(data_df) = c('ldrs_task_success','dsd_task_success','percentDLThrough','dsu_task_success','percentULThrough','UDP_dat_mean')
rownames(sms_df) = c('sms_access_success_inter','sms_access_success_intra','sms_task_success_inter',
                     'sms_task_success_intra','ldrs_task_success','mean_ldrs_task_speed_max')
rownames(speed_df) = c('dsd_effective_throughput_05p','dsd_time_to_first_byte_50p','dsd_effective_throughput_95p',
                       'dsu_effective_throughput_05p','liteData95Quant','MM95Quant')

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
    
    #### come back to this......
    indTest<-length(which(na.omit(tmp_testDat[which(tmp_testDat$test_type_id==26),]$ldrs_task_speed_max)<2000))/length(na.omit(tmp_testDat[which(tmp_testDat$test_type_id==26),]$ldrs_task_speed_max))
    smsStars[i]<-smsStars[i]+if(indTest<=.98){.5}else{0}
    
    
    sms_df[1,carrier_id] = sms_df[1,carrier_id] + if(market_data[data_ind[i],6]>=.99){1}else if(market_data[data_ind[i],6]>=.97){.5}else{0}
    sms_df[2,carrier_id] = sms_df[2,carrier_id] + if(market_data[data_ind[i],7]>=.99){1}else if(market_data[data_ind[i],7]>=.97){.5}else{0}
    sms_df[3,carrier_id] = sms_df[3,carrier_id] + if(market_data[data_ind[i],8]>=.99){1}else if(market_data[data_ind[i],8]>=.97){.5}else{0}
    sms_df[4,carrier_id] = sms_df[4,carrier_id] + if(market_data[data_ind[i],9]>=.99){1}else if(market_data[data_ind[i],9]>=.97){.5}else{0}
    sms_df[5,carrier_id] = sms_df[5,carrier_id] + if(market_data[data_ind[i],10]>=.98){.5}else{0}
    sms_df[6,carrier_id] = sms_df[6,carrier_id] + if(indTest<=.98){.5}else{0}
    
    #mobile to landline call drop
    callStars[i]=callStars[i]+ifelse(market_data$co_drop[data_ind[i]]<=.01,2.5,ifelse(market_data$co_drop[data_ind[i]]<=.015,2,ifelse(market_data$co_drop[data_ind[i]]<=.02,1.5,ifelse(market_data$co_drop[data_ind[i]]<=.025,1,ifelse(market_data$co_drop[data_ind[i]]<=.03,.5,0)))))
    #mobile to landline call block
    callStars[i]=callStars[i]+ifelse(market_data$co_block[data_ind[i]]<=.002,1.5,ifelse(market_data$co_block[data_ind[i]]<=.005,1,ifelse(market_data$co_block[data_ind[i]]<=.01,.5,0)))
    #mobile to landline call block
    callStars[i]=callStars[i]+ifelse(market_data$m2mo_block[data_ind[i]]<=.015,1,ifelse(market_data$m2mo_block[data_ind[i]]<=.02,.5,0))

    call_df[1,carrier_id]=call_df[1,carrier_id]+ifelse(market_data$co_drop[data_ind[i]]<=.01,2.5,ifelse(market_data$co_drop[data_ind[i]]<=.015,2,ifelse(market_data$co_drop[data_ind[i]]<=.02,1.5,ifelse(market_data$co_drop[data_ind[i]]<=.025,1,ifelse(market_data$co_drop[data_ind[i]]<=.03,.5,0)))))
    call_df[2,carrier_id]=call_df[2,carrier_id]+ifelse(market_data$co_block[data_ind[i]]<=.002,1.5,ifelse(market_data$co_block[data_ind[i]]<=.005,1,ifelse(market_data$co_block[data_ind[i]]<=.01,.5,0)))
    call_df[3,carrier_id]=call_df[3,carrier_id]+ifelse(market_data$m2mo_block[data_ind[i]]<=.015,1,ifelse(market_data$m2mo_block[data_ind[i]]<=.02,.5,0))
    
    #Lite Data Secure
    dataStars[i]=dataStars[i]+ifelse(market_data$ldrs_task_success[data_ind[i]]>=.99,.5,0)
    # Download Task Success
    dataStars[i]=dataStars[i]+ifelse(market_data$dsd_task_success[data_ind[i]]>=.99,.5,0)

    percentDLThrough=sum(!is.na(tmp_testDat$dsd_effective_download_test_speed)&tmp_testDat$dsd_effective_download_test_speed>=1000)/sum(!is.na(tmp_testDat$dsd_effective_download_test_speed))
    # % DL Throughput
    dataStars[i]=dataStars[i]+ifelse(percentDLThrough>=.97,2,ifelse(percentDLThrough>=.95,1.5,ifelse(percentDLThrough>=.92,1,ifelse(percentDLThrough>=.9,.5,0))))
    # Upload Task
    dataStars[i]=dataStars[i]+ifelse(market_data$dsu_task_success[data_ind[i]]>=.99,.5,0)
    # % UL Through (dsu_effective_upload_test_speed >= 500)/count(dsu_effective_upload_test_speed) -- where dsdu_effective_upload_test_speed not NULL
    percentULThrough=sum(!is.na(tmp_testDat$dsu_effective_upload_test_speed)&tmp_testDat$dsu_effective_upload_test_speed>=500)/sum(!is.na(tmp_testDat$dsd_effective_download_test_speed))
    dataStars[i]=dataStars[i]+ifelse(percentULThrough>=.97,1,ifelse(percentULThrough>=.92,.5,0))

    #UDP Packet Drop Rate mean(udp_packet_drop_rate) -- where test_type_id = 25
    UDP_dat_mean=mean(na.omit(tmp_testDat[which(tmp_testDat$test_type_id==25),]$udp_avg_packet_drop))
    dataStars[i]=dataStars[i]+ifelse(UDP_dat_mean<=.05,.5,0)
  
    data_df[1,carrier_id]=data_df[1,carrier_id]+ifelse(market_data$ldrs_task_success[data_ind[i]]>=.99,.5,0)
    data_df[2,carrier_id]=data_df[2,carrier_id]+ifelse(market_data$dsd_task_success[data_ind[i]]>=.99,.5,0)
    data_df[3,carrier_id]=data_df[3,carrier_id]+ifelse(percentDLThrough>=.97,2,ifelse(percentDLThrough>=.95,1.5,ifelse(percentDLThrough>=.92,1,ifelse(percentDLThrough>=.9,.5,0))))
    data_df[4,carrier_id]=data_df[4,carrier_id]+ifelse(market_data$dsu_task_success[data_ind[i]]>=.99,.5,0)
    data_df[5,carrier_id]=data_df[5,carrier_id]+ifelse(percentULThrough>=.97,1,ifelse(percentULThrough>=.92,.5,0))
    data_df[6,carrier_id]=data_df[6,carrier_id]+ifelse(UDP_dat_mean<=.05,.5,0)
    
    #Calculate Speed and Performance stars for each of the regions
    speedStars[i]=speedStars[i]+ifelse(market_data$dsd_effective_throughput_05p[data_ind[i]]>=5000,1.5,ifelse(market_data$dsd_effective_throughput_05p[data_ind[i]]>=3000,1,ifelse(market_data$dsd_effective_throughput_05p[data_ind[i]]>=2000,0.5,0)))
    
    speedStars[i]=speedStars[i]+ifelse(market_data$dsd_time_to_first_byte_50p[data_ind[i]]<=400,1,ifelse(market_data$dsd_time_to_first_byte_50p[data_ind[i]]<=700,0.5,0))
    
    speedStars[i]=speedStars[i]+ifelse(market_data$dsd_effective_throughput_95p[data_ind[i]]>=75000,.5,0)
    
    speedStars[i]=speedStars[i]+ifelse(market_data$dsu_effective_throughput_05p[data_ind[i]]>=1500,1,ifelse(market_data$dsu_effective_throughput_05p[data_ind[i]]>=1000,0.5,0)) 
  
    liteData95Quant=quantile(na.omit(tmp_testDat[which(tmp_testDat$test_type_id==26),]$ldrs_task_speed_max),probs=c(.95))
    speedStars[i]=speedStars[i]+ifelse(liteData95Quant<=1000,.5,0)
    
    MM95Quant=quantile(na.omit(tmp_testDat[which(tmp_testDat$test_type_id==23 & tmp_testDat$flag_access_success=='t'),]$m2mo_total_call_setup_duration),probs=c(.95))
    speedStars[i]=speedStars[i]+ifelse(MM95Quant<=7000,.5,0)
    
    speed_df[1,carrier_id]=speed_df[1,carrier_id]+ifelse(market_data$dsd_effective_throughput_05p[data_ind[i]]>=5000,1.5,ifelse(market_data$dsd_effective_throughput_05p[data_ind[i]]>=3000,1,ifelse(market_data$dsd_effective_throughput_05p[data_ind[i]]>=2000,0.5,0)))
    speed_df[2,carrier_id]=speed_df[2,carrier_id]+ifelse(market_data$dsd_time_to_first_byte_50p[data_ind[i]]<=400,1,ifelse(market_data$dsd_time_to_first_byte_50p[data_ind[i]]<=700,0.5,0))
    speed_df[3,carrier_id]=speed_df[3,carrier_id]+ifelse(market_data$dsd_effective_throughput_95p[data_ind[i]]>=75000,.5,0)
    speed_df[4,carrier_id]=speed_df[4,carrier_id]+ifelse(market_data$dsu_effective_throughput_05p[data_ind[i]]>=1500,1,ifelse(market_data$dsu_effective_throughput_05p[data_ind[i]]>=1000,0.5,0)) 
    speed_df[5,carrier_id]=speed_df[5,carrier_id]+ifelse(liteData95Quant<=1000,.5,0)
    speed_df[6,carrier_id]=speed_df[6,carrier_id]+ifelse(MM95Quant<=7000,.5,0)
  }
  
  
  callStars_df[,carrier_id]=callStars
  dataStars_df[,carrier_id]=dataStars
  speedStars_df[,carrier_id]=speedStars
  smsStars_df[,carrier_id]=smsStars
  
}
data

call_df=call_df/length(unique_locs)
data_df=data_df/length(unique_locs)
sms_df=sms_df/length(unique_locs)
speed_df=speed_df/length(unique_locs)



call_df['Max_Score']=c(2.5,1.5,1)
data_df['Max_Score']=c(.5,.5,2,.5,1,.5)
sms_df['Max_Score'] = c(1,1,1,1,.5,.5)
speed_df['Max_Score'] = c(1.5,1,.5,1,.5,.5)

if(save){
  ## save the RootScore Rank values from 1H to 2H of 2017
  write.csv(call_df,paste(c(save_directory,"call_breakdown_",which_half,'.csv'),collapse = ''))
  write.csv(data_df,paste(c(save_directory,"data_breakdown_",which_half,'.csv'),collapse = ''))
  write.csv(sms_df,paste(c(save_directory,"sms_breakdown_",which_half,'.csv'),collapse = ''))
  write.csv(speed_df,paste(c(save_directory,"speed_breakdown_",which_half,'.csv'),collapse = ''))
}


call_change_df = call_df[,-5]-call_df1[,-5]
data_change_df = data_df[,-5]-data_df1[,-5]
sms_change_df = sms_df[,-5]-sms_df1[,-5]
speed_change_df = speed_df[,-5]-speed_df1[,-5]


## save the RootScore Rank values from 1H to 2H of 2017
write.csv(call_change_df,paste(c(save_directory,'call_breakdown_change.csv'),collapse = ''))
write.csv(data_change_df,paste(c(save_directory,'data_breakdown_change.csv'),collapse = ''))
write.csv(sms_change_df,paste(c(save_directory,'sms_breakdown_change.csv'),collapse = ''))
write.csv(speed_change_df,paste(c(save_directory,'speed_breakdown_change.csv'),collapse = ''))




