generate_individ_scores<-function(carrier='Verizon',radius_miles = 30,targ_latlon = c(34.0007,-81.0348)){
  #install.packages('geosphere')
  test_dict<-read.csv('Data/individ_data_sc/aggregate_calculations.csv',header = T)
  test_data<-read.csv('Data/individ_data_sc/sc_test_locations.csv',header = T)
  library(geosphere)
  
  #Columbia SC lat,long
  #targ_latlon = c(34.0007,-81.0348)
  meters_per_mile = 1609.34
  
  # 1-AT&T 2-Sprint 3-T-Mobile 4-Verizon
  carriers=c('AT&T','Sprint','T-Mobile','Verizon')
  carrier_id = which(carriers==carrier)
  test_data = test_data[which(test_data$carrier_id==carrier_id),]
  targ_mat = matrix(0,nrow=nrow(test_data),ncol=2)
  for (i in 1:(nrow(test_data))){
    targ_mat[i,] = targ_latlon
  }
  
  test_subset=test_data[which((distVincentyEllipsoid(targ_mat, cbind(test_data$start_lat,test_data$start_lon), a=6378137, b=6356752.3142, f=1/298.257223563)/meters_per_mile)<radius_miles),]
  
  ## call metrics
  co_block = length(which(as.character(test_subset$flag_access_success)=='f' & test_subset$test_type_id==16))  /   length(which(as.character(test_subset$flag_access_success)!='' & test_subset$test_type_id==16))
  co_drop = length(which(as.character(test_subset$co_flag_retain_success)=='f' & test_subset$test_type_id==16))  /   length(which(as.character(test_subset$co_flag_retain_success)!='' & test_subset$test_type_id==16))
  m2mo_block = length(which(as.character(test_subset$flag_access_success)=='f' & test_subset$test_type_id==23))  /   length(which(as.character(test_subset$flag_access_success)!='' & test_subset$test_type_id==23))
  
  ## data metrics
  ldrs_task_success = mean(na.omit(test_subset$percentage_task_success[which(test_subset$test_type_id==26)]))
  dsd_task_success = mean(na.omit(test_subset$percentage_task_success[which(test_subset$test_type_id==20)]))
  dsu_task_success = mean(na.omit(test_subset$percentage_task_success[which(test_subset$test_type_id==19)]))
  percentDLThrough=sum(!is.na(test_data$dsd_effective_download_test_speed)&test_data$dsd_effective_download_test_speed>=1000)/sum(!is.na(test_data$dsd_effective_download_test_speed))
  # % UL Through (dsu_effective_upload_test_speed >= 500)/count(dsu_effective_upload_test_speed) -- where dsdu_effective_upload_test_speed not NULL
  percentULThrough=sum(!is.na(test_data$dsu_effective_upload_test_speed)&test_data$dsu_effective_upload_test_speed>=500)/sum(!is.na(test_data$dsd_effective_download_test_speed))
  #UDP Packet Drop Rate mean(udp_packet_drop_rate) -- where test_type_id = 25
  UDP_dat_mean=0#####mean(na.omit(test_data[which(test_data$test_type_id==25),]$udp_avg_packet_drop))
  
  #speed metrics
  dsd_effective_throughput_05p = as.numeric(quantile(na.omit(test_subset$dsd_effective_download_test_speed[which(test_subset$test_type_id==20)]),.05))
  dsd_time_to_first_byte_50p=as.numeric(quantile(na.omit(test_subset$dsd_time_to_first_byte_median[which(test_subset$test_type_id==20)]),.5))
  dsd_effective_throughput_95p =as.numeric(quantile(na.omit(test_subset$dsd_effective_download_test_speed[which(test_subset$test_type_id==20)]),.95))
  dsu_effective_throughput_05p=as.numeric(quantile(na.omit(test_subset$dsu_effective_upload_test_speed[which(test_subset$test_type_id==19)]),.05))
  liteData95Quant=0#quantile(na.omit(test_subset[which(test_subset$test_type_id==26),]$ldrs_task_speed_max),probs=c(.95))
  MM95Quant=0#quantile(na.omit(test_data[which(test_data$test_type_id==23 & tmp_testDat_1$flag_access_success=='t'),]$m2mo_total_call_setup_duration),probs=c(.95))
  
  
  callStars = dataStars = speedStars = smsStars = 0
  
  # assign call stars
  #mobile to landline call drop
  callStars=callStars+ifelse(co_drop<=.01,2.5,ifelse(co_drop<=.015,2,ifelse(co_drop<=.02,1.5,ifelse(co_drop<=.025,1,ifelse(co_drop<=.03,.5,0)))))
  #mobile to landline call block
  callStars=callStars+ifelse(co_block<=.002,1.5,ifelse(co_block<=.005,1,ifelse(co_block<=.01,.5,0)))
  #mobile to landline call block
  callStars=callStars+ifelse(m2mo_block<=.015,1,ifelse(m2mo_block<=.02,.5,0))
  
  
  #assign data stars
  #Lite Data Secure
  dataStars=dataStars+ifelse(ldrs_task_success>=.99,.5,0)
  # Download Task Success
  dataStars=dataStars+ifelse(dsd_task_success>=.99,.5,0)
  # % DL Throughput
  dataStars=dataStars+ifelse(percentDLThrough>=.97,2,ifelse(percentDLThrough>=.95,1.5,ifelse(percentDLThrough>=.92,1,ifelse(percentDLThrough>=.9,.5,0))))
  # Upload Task
  dataStars=dataStars+ifelse(dsd_task_success>=.99,.5,0)
  # 
  dataStars=dataStars+ifelse(percentULThrough>=.97,1,ifelse(percentULThrough>=.92,.5,0))
  #Currently missing this data......
  dataStars=dataStars+ifelse(UDP_dat_mean<=.05,.5,0)
  
  
  
  
  #assign speed stars
  #Calculate Speed and Performance stars for each of the regions
  speedStars=speedStars+ifelse(dsd_effective_throughput_05p>=5000,1.5,ifelse(dsd_effective_throughput_05p>=3000,1,ifelse(dsd_effective_throughput_05p>=2000,0.5,0)))
  
  speedStars=speedStars+ifelse(dsd_time_to_first_byte_50p<=400,1,ifelse(dsd_time_to_first_byte_50p<=700,0.5,0))
  
  speedStars=speedStars+ifelse(dsd_effective_throughput_95p>=75000,.5,0)
  
  speedStars=speedStars+ifelse(dsu_effective_throughput_05p>=1500,1,ifelse(dsu_effective_throughput_05p>=1000,0.5,0)) 
  
  #need this val
  speedStars=speedStars+ifelse(liteData95Quant<=1000,.5,0)
  
  speedStars=speedStars+ifelse(MM95Quant<=7000,.5,0)
  print(paste(c('Using a',radius_miles,'mile radius of test data around the location,',carrier,'acheived the following scores:'),collapse=' '))
  print(paste(c('Call Stars:',callStars),collapse=' '))
  print(paste(c('Data Stars:',dataStars),collapse=' '))
  print(paste(c('Speed Stars:',speedStars),collapse=' '))
}

