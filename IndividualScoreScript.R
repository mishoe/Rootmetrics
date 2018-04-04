generate_individ_scores<-function(targ_locat = "66 George St, Charleston, SC 29424",carrier='Verizon',radius_miles = 30,in_app=FALSE){
  library(plotly)
  library(maps)
  library(sp)
  library(plotrix)
  library(fiftystater)
  library(mgcv)  
  library(RJSONIO)
  library(RCurl)
  library(leaflet)
  google_api_key = "AIzaSyDxY76Lf1EjK1T3sY-ViCrt5qvtZE3Uwk0"
  Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoiYWxsZW5yZW5jaDIyMiIsImEiOiJjamV4MTZ2anYxMnh2MndvNDQ4MDBzNjRkIn0.RD3zOxD_veDhoQG1wmHqiA')
  pre_path = ifelse(in_app==TRUE, '../','')
  
  
  
  targ_locat = "66 George St, Charleston, SC 29424"
  carrier='Verizon'
  radius_miles = 30
  # this function accepts either single values or vectors as inputs for the lat and lon attributes.
  # the return dataframe is in the form of what is required for the add_polygon function(plotter)
  generate_poly_latlon <- function(lat,lon,radius,num_vertex){
    angle = round(360/num_vertex)
    return_df=NULL
    for (iter in 1:length(lat)){
      x.long<-NULL
      y.lat<-NULL
      for(i in seq(1,360,angle)){
        x.long<-c(x.long,radius*cos(i/360*2*pi)+lon[iter])
        y.lat<-c(y.lat,radius*sin(i/360*2*pi)+lat[iter])
      }
      x.long<-c(x.long,x.long[1])
      y.lat<-c(y.lat,y.lat[1])
      return_df=rbind(return_df,cbind(rep(iter,length(x.long)),x.long,y.lat))
    }
    return_df=data.frame(return_df)
    colnames(return_df) = c("Group","Longitude","Latitude")
    return(return_df)
  }
  
  #this function takes an address(string) and geocodes it into a latitude,longitude pair
  getGeoData <- function(location){
    location <- gsub(' ','+',location)
    geo_data <- getURL(paste("https://maps.googleapis.com/maps/api/geocode/json?address=",location,"&key=",google_api_key, sep=""))
    raw_data_2 <- fromJSON(geo_data)
    return(raw_data_2)
  }
  geo_data=getGeoData(targ_locat)
  latlng = geo_data$results[[1]]$geometry$location
  targ_latlon = c(as.numeric(latlng[1]),as.numeric(latlng[2]))
  
  #install.packages('geosphere')
  test_dict<-read.csv(paste(pre_path,'Data/individ_data_sc/aggregate_calculations.csv',sep=''),header = T)
  test_data<-read.csv(paste(pre_path,'Data/individ_data_sc/sc_test_locations.csv',sep=''),header = T)
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
  
  
  callStars = dataStars = speedStars = smsStars = overallStars=0
  
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
  print(targ_locat)
  print(paste(c('Using a',radius_miles,'mile radius of test data around the location,',carrier,'acheived the following scores:'),collapse=' '))
  print(paste(c('Call Stars:',callStars),collapse=' '))
  print(paste(c('Data Stars:',dataStars),collapse=' '))
  print(paste(c('Speed Stars:',speedStars),collapse=' '))
  print(paste(c('SMS Stars:',smsStars),collapse=' '))
  print(paste(c('Overall Stars:',overallStars),collapse=' '))
  
  output_list = c(targ_locat,
             paste(c('Using a',radius_miles,'mile radius of test data around the location,',carrier,'acheived the following scores:'),collapse=' '),
             paste(c('Call Stars:',callStars),collapse=' '),
             paste(c('Data Stars:',dataStars),collapse=' '),
             paste(c('Speed Stars:',speedStars),collapse=' '),
             paste(c('SMS Stars:',smsStars),collapse=' '),
             paste(c('Overall Stars:',overallStars),collapse=' '))

  
  lat.cur<-targ_latlon[1]
  long.cur<-targ_latlon[2]
  rad.cur<-.1
  
  x.long<-NULL
  for(i in 1:360){
    x.long<-c(x.long,rad.cur*cos(i/360*2*pi)+long.cur)
  }
  x.long<-c(x.long,x.long[1])
  
  
  y.lat<-NULL
  for(i in 1:360){
    y.lat<-c(y.lat,rad.cur*sin(i/360*2*pi)+lat.cur)
  }
  y.lat<-c(y.lat,y.lat[1])

  outer_circle_df = generate_poly_latlon(lat=targ_latlon[1],lon=targ_latlon[2],radius=.1,num_vertex = 360)
  center_df = generate_poly_latlon(lat=targ_latlon[1],lon=targ_latlon[2],radius=.01,num_vertex = 360)
  test_values_df = generate_poly_latlon(lat=unique(test_subset$end_lat),lon=unique(test_subset$end_lon),radius=.001,num_vertex = 3)
  
  
  dat <- map_data("state") %>% group_by(group)
  sc.state<-which(dat[,5]=='south carolina')
  
  
  
  m <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(lng=targ_latlon[2], lat=targ_latlon[1], popup=targ_locat)%>%
    #addPolygons(lng=test_values_df$Longitude,lat=test_values_df$Latitude,data=group_by(test_values_df,Group), color=I("#FF0000"),opacity=.4)
    addCircles(lng=test_subset$end_lon, lat=test_subset$end_lat, weight = 3, radius=40, 
               color="#ff0000", stroke = TRUE, fillOpacity = 0.8)
  m
  
  
  
  
  
}

