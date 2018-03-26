#Draw Hexagons over United States
library(grDevices)
library(jpeg)
city_data<-read.csv("data_safe/uscities.csv",header=T)
all.loc<-read.csv("data_safe/USloc.csv",header=T)
hex.loc<-read.csv("data_safe/locations_of_cities_on_hex_graph_1.5.csv",header=T)
hex.loc<-as.matrix(hex.loc)
hex.loc<-hex.loc[,2:length(hex.loc[1,])]
root.jpg<-readJPEG("data_safe/rootmetrics_logo_jpeg.jpg")

carrier.plotter<-function(car.dr='At.t',star.df,city.data,all.loc=all.loc,hex.loc=hex.loc,key=TRUE,col.on=FALSE,star.type){
  require(sp)
  require(plotrix)
  require(fiftystater)
  require(mgcv)
  require(jpeg)
  require(SDMTools)
  
  carriers=c('AT&T','Sprint','T-Mobile','Verizon')
  stars.rank<-star.df[,2:4]
  carrier.draw<-which(colnames(stars.rank)==car.dr)
  
  
  if(col.on==TRUE){
  ver.col<-c(236,7,16)
  tmob.col<-c(227,0,116)
  sprint.col<-c(251,223,0)
  att.col<-c(0,159,219)
  }else{
    ver.col<-c(0,0,0)
    tmob.col<-c(0,0,0)
    sprint.col<-c(0,0,0)
    att.col<-c(0,0,0)
  }
  carriers.col<-rbind(att.col,sprint.col,tmob.col,ver.col)
  
  
  #draw the United States map
  
  unique.reg<-unique(fifty_states[,7])
  plot(0,0,xlim=c(-130,-55),ylim=c(20,50),col='white',axes=F,frame.plot=F,xlab='',ylab='',main=paste(carriers[carrier.draw],star.type,'Stars'))
  rasterImage(root.jpg,-70,42,-55,57)
  for(i in 1:length(unique.reg)){
    inds.reg<-which(fifty_states[,7]==unique.reg[i])
    polygon(fifty_states[inds.reg,1],fifty_states[inds.reg,2])
    lat<-city_data[1,8]
    long<-city_data[1,9]
  }
  
  
  #Draw hexagons over the US
  size<-1.5
  
  on.off<-1
  count<-0
  suppressWarnings(
  for(i in seq(floor(min(fifty_states[,1])-size),ceiling(max(fifty_states[,1])),size)){
    on.off<-1
    for(j in seq(floor(min(fifty_states[,2])-1*size)-size,ceiling(max(fifty_states[,2])+size),size)){
      inds.min<-which(fifty_states[,2]<=(j+1*size) & fifty_states[,2]>=(j-1*size))
      inds.min.v<-which(fifty_states[,1]<=(i+1*size) & fifty_states[,1]>=(i-1*size))
      
      col.hex<-if(length(which(hex.loc[,count]>0)>0)){rgb(255-mean(stars.rank[which(hex.loc[,count]>0),carrier.draw])/5*(255-carriers.col[carrier.draw,1]),255-mean(stars.rank[which(hex.loc[,count]>0),carrier.draw])/5*(255-carriers.col[carrier.draw,2]),255-mean(stars.rank[which(hex.loc[,count]>0),carrier.draw])/5*(255-carriers.col[carrier.draw,3]),maxColorValue = 255)}else{NULL}
      bord.hex<-if(length(which(hex.loc[,count]>0)>0)){200}else{200}
      
      if(i>=min(fifty_states[inds.min,1]-1) & i<=max(fifty_states[inds.min,1]) & j>=min(fifty_states[inds.min.v,2]-size) & j<=max(fifty_states[inds.min.v,2]-1)){
        if(on.off==1){
          hexagon(i,j,unitcell = size,border = rgb(bord.hex,bord.hex,bord.hex,maxColorValue = 255),col=col.hex)
        }else{hexagon(i+.5*size,j,unitcell = size,border = rgb(bord.hex,bord.hex,bord.hex,maxColorValue = 255),col=col.hex)}
      }else{}
      on.off<-on.off*-1
      count<-count+1
    }
  })
  
  for(i in 1:length(unique.reg)){
    inds.reg<-which(fifty_states[,7]==unique.reg[i])
    polygon(fifty_states[inds.reg,1],fifty_states[inds.reg,2],border = rgb(100,100,100,maxColorValue = 255))
    lat<-city_data[1,8]
    long<-city_data[1,9]
  }
  
  if(key==TRUE){
  
  leg.xpts<-c(-70,-65,-65,-70)
  leg.ypts<-c(33,33,23,23)
  leg.points<-cbind(x=leg.xpts,y=leg.ypts)
  
  #make 100 colors of color
  all.col<-NULL
  for(i in seq(0,5,.5)){
    new.col<-rgb(255-i/5*(255-carriers.col[carrier.draw,1]),255-i/5*(255-carriers.col[carrier.draw,2]),255-i/5*(255-carriers.col[carrier.draw,3]),maxColorValue = 255)
    all.col<-c(all.col,new.col)
  }
  
  
  legend.gradient(leg.points,cols = all.col,title = 'Stars',limits = c(0,5))
  
  }
  
  
  
}

carrier.plotter.rank<-function(car.dr=1,rank.df,city.data,all.loc,hex.loc,key=TRUE,col.on=FALSE,star.type){
  require(sp)
  require(plotrix)
  require(fiftystater)
  require(mgcv)
  require(jpeg)
  require(SDMTools)
  carriers=c('AT&T','Sprint','T-Mobile','Verizon')
  stars.rank<-rank.df[,2:4]
  carrier.draw<-which(colnames(stars.rank)==car.dr)
  
  if(col.on==TRUE){
    ver.col<-c(236,7,16)
    tmob.col<-c(227,0,116)
    sprint.col<-c(251,223,0)
    att.col<-c(0,159,219)
  }else{
    ver.col<-c(0,0,0)
    tmob.col<-c(0,0,0)
    sprint.col<-c(0,0,0)
    att.col<-c(0,0,0)
  }
  carriers.col<-rbind(att.col,sprint.col,tmob.col,ver.col)
  
  
  inds.city<-NULL
  # for(i in 1:length(all.loc[,1])){
  #   cit.new<-which(as.character(city_data[,1])==as.character(all.loc[i,3]) & as.character(city_data[,3])==as.character(all.loc[i,4]))
  #   if(length(cit.new)==0){
  #     cit.new<-which(as.character(city_data[,1])==as.character(all.loc[i,7]) & as.character(city_data[,3])==as.character(all.loc[i,4]))}
  #   
  #   inds.city<-c(inds.city,cit.new)
  # }
  
  
  #draw the United States map
  
  unique.reg<-unique(fifty_states[,7])
  plot(0,0,xlim=c(-130,-55),ylim=c(20,50),col='white',axes=F,frame.plot=F,xlab='',ylab='',main=paste(carriers[carrier.draw],star.type,'Rankings'))
  rasterImage(root.jpg,-70,42,-55,57)
  for(i in 1:length(unique.reg)){
    inds.reg<-which(fifty_states[,7]==unique.reg[i])
    polygon(fifty_states[inds.reg,1],fifty_states[inds.reg,2])
    lat<-city_data[1,8]
    long<-city_data[1,9]
  }
  
  
  #Draw hexagons over the US
  size<-1.5
  
  on.off<-1
  count<-0
  suppressWarnings(
  for(i in seq(floor(min(fifty_states[,1])-size),ceiling(max(fifty_states[,1])),size)){
    on.off<-1
    for(j in seq(floor(min(fifty_states[,2])-1*size)-size,ceiling(max(fifty_states[,2])+size),size)){
      inds.min<-which(fifty_states[,2]<=(j+1*size) & fifty_states[,2]>=(j-1*size))
      inds.min.v<-which(fifty_states[,1]<=(i+1*size) & fifty_states[,1]>=(i-1*size))
      
      col.hex<-if(length(which(hex.loc[,count]>0)>0)){rgb(255-(5-mean(stars.rank[which(hex.loc[,count]>0),carrier.draw]))/4*(255-carriers.col[carrier.draw,1]),255-(5-mean(stars.rank[which(hex.loc[,count]>0),carrier.draw]))/4*(255-carriers.col[carrier.draw,2]),255-(5-mean(stars.rank[which(hex.loc[,count]>0),carrier.draw]))/4*(255-carriers.col[carrier.draw,3]),maxColorValue = 255)}else{NULL}
      bord.hex<-if(length(which(hex.loc[,count]>0)>0)){200}else{200}
      
      if(i>=min(fifty_states[inds.min,1]-1) & i<=max(fifty_states[inds.min,1]) & j>=min(fifty_states[inds.min.v,2]-size) & j<=max(fifty_states[inds.min.v,2]-1)){
        if(on.off==1){
          hexagon(i,j,unitcell = size,border = rgb(bord.hex,bord.hex,bord.hex,maxColorValue = 255),col=col.hex)
        }else{hexagon(i+.5*size,j,unitcell = size,border = rgb(bord.hex,bord.hex,bord.hex,maxColorValue = 255),col=col.hex)}
      }else{}
      on.off<-on.off*-1
      count<-count+1
    }
  })
  
  for(i in 1:length(unique.reg)){
    inds.reg<-which(fifty_states[,7]==unique.reg[i])
    polygon(fifty_states[inds.reg,1],fifty_states[inds.reg,2],border = rgb(100,100,100,maxColorValue = 255))
    lat<-city_data[1,8]
    long<-city_data[1,9]
  }
  
  if(key==TRUE){
    
    leg.xpts<-c(-70,-65,-65,-70)
    leg.ypts<-c(33,33,23,23)
    leg.points<-cbind(x=leg.xpts,y=leg.ypts)
    
    #make colors
    all.col<-NULL
    for(i in seq(1,4,1)){
      new.col<-rgb(255-i/4*(255-carriers.col[carrier.draw,1]),255-i/4*(255-carriers.col[carrier.draw,2]),255-i/4*(255-carriers.col[carrier.draw,3]),maxColorValue = 255)
      all.col<-c(all.col,new.col)
    }
    
    
    legend.gradient(leg.points,cols = all.col,title = 'Stars',limits = c(4,1))
    
  }
  
  
  
}

full.star<-function(x,y,v.peak,v.trough,col,border){
  for(j in 1:length(x)){
    star.ray.l<-v.peak
    star.ray.s<-v.trough
    #get points on outer star
    x.out<-x[j]+star.ray.l*cos(seq(0+18/180*pi,2*pi+18/180*pi,2*pi/5))
    y.out<-y[j]+star.ray.l*sin(seq(0+18/180*pi,2*pi+18/180*pi,2*pi/5))
    
    #get points on inner star
    x.in<-x[j]+star.ray.s*cos(seq(0+18/180*pi,2*pi+18/180*pi,2*pi/5))
    y.in<-y[j]+-star.ray.s*sin(seq(0+18/180*pi,2*pi+18/180*pi,2*pi/5))
    
    #polygon Points
    points.pol.x<-NULL
    points.pol.y<-NULL
    for(i in 1:5){
      points.pol.x<-c(points.pol.x,c(x.out[i],x.in[6-i]))
      points.pol.y<-c(points.pol.y,c(y.out[i],y.in[6-i]))
    }
    
    
    polygon(points.pol.x,points.pol.y,col=col,border = border)
  }
}

half.star<-function(x,y,v.peak,v.trough,col,border){
  for(j in 1:length(x)){
    star.ray.l<-v.peak
    star.ray.s<-v.trough
    #get points on outer star
    x.out<-x[j]+star.ray.l*cos(seq(0+18/180*pi,2*pi+18/180*pi,2*pi/5))
    y.out<-y[j]+star.ray.l*sin(seq(0+18/180*pi,2*pi+18/180*pi,2*pi/5))
    
    #get points on inner star
    x.in<-x[j]+star.ray.s*cos(seq(0+18/180*pi,2*pi+18/180*pi,2*pi/5))
    y.in<-y[j]+-star.ray.s*sin(seq(0+18/180*pi,2*pi+18/180*pi,2*pi/5))
    
    #polygon Points
    points.pol.x<-NULL
    points.pol.y<-NULL
    for(i in 2:4){
      points.pol.x<-c(points.pol.x,c(x.out[i],x.in[6-i]))
      points.pol.y<-c(points.pol.y,c(y.out[i],y.in[6-i]))
    }
    
    
    polygon(points.pol.x,points.pol.y,col=col,border = border)
  }
}

Star_Report<-function(car.dr='AT.T',year.h=1,loc.cit='Charleston',city.data,all.loc,star.type){


require(graphics)
require(sp)
require(plotrix)
require(fiftystater)
require(mgcv)
require(jpeg)
require(SDMTools)


#Star Drawing Function
location<-loc.cit
if(year.h==1){
callstars.df<-callStars_df_1
speedstars.df<-speedStars_df_1
datastars.df<-dataStars_df_1
smsstars.df<-smsStars_df_1
}
else if(year.h==2){
  callstars.df<-callStars_df_2
  speedstars.df<-speedStars_df_2
  datastars.df<-dataStars_df_2
  smsstars.df<-smsStars_df_2
}


if(year.h==1){
  callrank.df<-callStarsRank_df_1
  speedrank.df<-speedStarsRank_df_1
  datarank.df<-dataStarsRank_df_1
  smsrank.df<-smsStarsRank_df_1
}
else if(year.h==2){
  callrank.df<-callStarsRank_df_2
  speedrank.df<-speedStarsRank_df_2
  datarank.df<-dataStarsRank_df_2
  smsrank.df<-smsStarsRank_df_2
}


carrier.draw<-which(colnames(callrank.df)==car.dr)
carriers=c('AT&T','Sprint','T-Mobile','Verizon')

cats<-c('Call','SMS','Data','Speed')


ver.col<-c(236,7,16)
tmob.col<-c(227,0,116)
sprint.col<-c(251,223,0)
att.col<-c(0,159,219)

carriers.col<-rbind(att.col,sprint.col,tmob.col,ver.col)
par(mar=c(2,2,2,2))

if(length(match(location,all.loc[,3]))>0){
  inds.loc<-which(all.loc[,3]==location)
  par(mfrow=c(length(inds.loc),1))
  
  
  plot(0,0,xlim=c(-15,120),ylim=c(0,50),col='white',axes=F,frame.plot=F,xlab='',ylab='')
  rasterImage(root.jpg,95,35,120,60)
  for(j in 1:4){
    stars.cur<-if(j==1){callstars.df}else if(j==2){smsstars.df}else if(j==3){datastars.df}else if(j==4){speedstars.df}
    inds.loc.cit<-which(stars.cur[,1]==all.loc[inds.loc,2])
    print(inds.loc.cit)
    place<-if(j==1){callrank.df[inds.loc.cit,carrier.draw]}else if(j==2){smsrank.df[inds.loc.cit,carrier.draw]}else if(j==3){datarank.df[inds.loc.cit,carrier.draw]}else if(j==4){speedrank.df[inds.loc.cit,carrier.draw]}
    star.x<-c(12,31,50,69,88)#
    star.y<-c(4+10*(j-1),4+10*(j-1),4+10*(j-1),4+10*(j-1),4+10*(j-1))
    star.ray.l<-3
    star.ray.s<-1.25
    for(i in 1:length(star.x)){
      full.star(star.x[i],star.y[i],v.peak = star.ray.l,v.trough = star.ray.s,col='white',border='black')
    }
    #number Filled
    

    star.num<-stars.cur[inds.loc.cit,carrier.draw]
    num.fill<-floor(star.num)
    half.fill<-star.num-floor(star.num)
    
    if(num.fill>0){
    for(i in 1:num.fill){
      full.star(star.x[i],star.y[i],v.peak = star.ray.l,v.trough = star.ray.s,col=rgb(255,240,30,maxColorValue = 255),border='black')
    }}
    
    if(half.fill==.5){
      half.star(star.x[num.fill+1],star.y[num.fill+1],v.peak = star.ray.l,v.trough = star.ray.s,col=rgb(255,240,30,maxColorValue = 255),border='black')
    }
    
    text(-10,4+10*(j-1),labels = cats[j],cex=1)
    
    suffix<-if(place==1){'st'}else if(place==2){'nd'}else if(place==3){'rd'}else if(place==4){'th'}
    
    if(place==1){
      draw.ellipse(105,4+10*(j-1),a=4,b=4,border =rgb(carriers.col[carrier.draw-1,1],carriers.col[carrier.draw-1,2],carriers.col[carrier.draw-1,3],maxColorValue = 255) ,col=rgb(carriers.col[carrier.draw-1,1],carriers.col[carrier.draw-1,2],carriers.col[carrier.draw-1,3],maxColorValue = 255))
      text(105,4+10*(j-1),labels = paste(place,suffix,sep=''),cex=1,col='white')
    }else{
      text(105,4+10*(j-1),labels = paste(place,suffix,sep=''),cex=1)
    }
    
  }
  text(0,45,pos = 4,labels = paste(carriers[carrier.draw-1],all.loc[inds.loc,3],',',all.loc[inds.loc,4]),cex=1.5)
}
}

