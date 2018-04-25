#build the composite scores for the star methods


source('PullDataMethods.R')
pull_data_2017_1H(score = T)
pull_data_2017_2H(score = T)
#pull_data_2017_All()
#pull_data_2017_Compare()

col_names = c('Location','AT.T','Sprint','T.Mobile','Verizon')

composite_star_1<-round(.4*callStars_df_1+.275*dataStars_df_1+.275*speedStars_df_1+.05*smsStars_df_1,digits = 2)
composite_star_2<-round(.4*callStars_df_2+.275*dataStars_df_2+.275*speedStars_df_2+.05*smsStars_df_2,digits = 2)


colnames(composite_star_1)<-col_names
colnames(composite_star_2)<-col_names



write.csv(composite_star_1,'Data/1H/overall_star_raw_1H2017.csv')
write.csv(composite_star_2,'Data/2H/overall_star_raw_2H2017.csv')


composite_star_1 = round(2*composite_star_1)/2
composite_star_2 = round(2*composite_star_2)/2




#overall star rankings
composite_star_rank_1<-matrix(0,nrow = 125,ncol = 4)

for(i in 1:length(composite_star_1[,1])){
  vals<-unique(as.numeric(composite_star_1[i,2:5]))
  ord.vals<-order(vals,decreasing = T)
  count<-0
  for(j in 1:length(vals)){
    inds<-which(composite_star_1[i,2:5]==as.numeric(vals[ord.vals[j]]))
    composite_star_rank_1[i,inds]<-count+1
    count<-count+length(inds)
  }
  
}

composite_star_rank_1<-cbind(composite_star_1[,1],composite_star_rank_1)
colnames(composite_star_rank_1)<-col_names

write.csv(composite_star_rank_1,'Data/1H/overall_star_rank_1H2017.csv')


composite_star_rank_2<-matrix(0,nrow = 125,ncol = 4)

for(i in 1:length(composite_star_2[,1])){
  vals<-unique(as.numeric(composite_star_2[i,2:5]))
  ord.vals<-order(vals,decreasing = T)
  count<-0
  for(j in 1:length(vals)){
    inds<-which(composite_star_2[i,2:5]==as.numeric(vals[ord.vals[j]]))
    composite_star_rank_2[i,inds]<-count+1
    count<-count+length(inds)
  }
  
}

composite_star_rank_2<-cbind(composite_star_2[,1],composite_star_rank_2)
colnames(composite_star_rank_2)<-col_names

write.csv(composite_star_rank_2,'Data/2H/overall_star_rank_2H2017.csv')


LocationLookup<-read.csv("Data/LocationLookup.csv",header = T)

rootscore_data<-read.csv('rating_data/rating_data/rootscore_ranks_2H2017.csv',header = T)


#make the overall rankings for each half
composite_score_1<-NULL
composite_score_2<-NULL
loc<-unique(rootscore_data[,2])
for(i in 1:length(loc)){
  com.score<-NULL
  for(j in 1:4){
    ind<-which(loc[i]==rootscore_data[,2] & rootscore_data[,3]=='Overall' & rootscore_data[,4]==j)
    com.score<-c(com.score,rootscore_data[ind,5])
  }
  composite_score_2<-rbind(composite_score_2,c(loc[i],com.score))
}
colnames(composite_score_2)<-col_names
ord.all<-NULL
for(i in 1:length(composite_score_2[,1])){
  ord.all<-c(ord.all,which(composite_score_2[,1]==as.numeric(LocationLookup[i,2])))
}
composite_score_2<-composite_score_2[ord.all,]
rootscore_rank_df_2 = composite_score_2
rootscore_df_2 = data.frame(matrix(0,ncol = 5,nrow=nrow(LocationLookup)))
colnames(rootscore_df_2) = col_names
rootscore_df_2$Location = composite_score_2[,1]

for (i in 1:nrow(rootscore_df_2)){
  rootscore_df_2$AT.T[i] = rootscore_data$rootscore[which(rootscore_data$carrier_id==1 & rootscore_data$rootscore_index=='Overall' & rootscore_data$collection_set_id==rootscore_df_2$Location[i])]
  rootscore_df_2$Sprint[i] = rootscore_data$rootscore[which(rootscore_data$carrier_id==2 & rootscore_data$rootscore_index=='Overall' & rootscore_data$collection_set_id==rootscore_df_2$Location[i])]
  rootscore_df_2$T.Mobile[i] = rootscore_data$rootscore[which(rootscore_data$carrier_id==3 & rootscore_data$rootscore_index=='Overall' & rootscore_data$collection_set_id==rootscore_df_2$Location[i])]
  rootscore_df_2$Verizon[i] = rootscore_data$rootscore[which(rootscore_data$carrier_id==4 & rootscore_data$rootscore_index=='Overall' & rootscore_data$collection_set_id==rootscore_df_2$Location[i])]
}

write.csv(as.matrix(rootscore_df_2),'Data/2H/overall_score_raw_2H2017.csv')
write.csv(composite_score_2,'Data/2H/overall_score_rank_2H2017.csv')


rootscore_data<-read.csv('rating_data/rating_data/rootscore_ranks_1H2017.csv',header = T)
composite_score_1<-NULL
composite_score_2<-NULL
loc<-unique(rootscore_data[,3])
for(i in 1:length(loc)){
  com.score<-NULL
  for(j in 1:4){
    ind<-which(loc[i]==rootscore_data[,3] & rootscore_data[,6]=='Overall' & rootscore_data[,7]==j)
    com.score<-c(com.score,rootscore_data[ind,9])
  }
  composite_score_2<-rbind(composite_score_2,c(loc[i],com.score))
}
colnames(composite_score_2)<-col_names
ord.all<-NULL
for(i in 1:length(composite_score_2[,1])){
  ord.all<-c(ord.all,which(composite_score_2[,1]==as.numeric(LocationLookup[i,1])))
}
composite_score_2<-composite_score_2[ord.all,]

rootscore_rank_df_1 = composite_score_2
rootscore_df_1 = data.frame(matrix(0,ncol = 5,nrow=nrow(LocationLookup)))
colnames(rootscore_df_1) = col_names
rootscore_df_1$Location = composite_score_2[,1]

for (i in 1:nrow(rootscore_df_1)){
  rootscore_df_1$AT.T[i] = rootscore_data$rootscore[which(rootscore_data$carrier_id==1 & rootscore_data$rootscore_index=='Overall' & rootscore_data$collection_set_id==rootscore_df_1$Location[i])]
  rootscore_df_1$Sprint[i] = rootscore_data$rootscore[which(rootscore_data$carrier_id==2 & rootscore_data$rootscore_index=='Overall' & rootscore_data$collection_set_id==rootscore_df_1$Location[i])]
  rootscore_df_1$T.Mobile[i] = rootscore_data$rootscore[which(rootscore_data$carrier_id==3 & rootscore_data$rootscore_index=='Overall' & rootscore_data$collection_set_id==rootscore_df_1$Location[i])]
  rootscore_df_1$Verizon[i] = rootscore_data$rootscore[which(rootscore_data$carrier_id==4 & rootscore_data$rootscore_index=='Overall' & rootscore_data$collection_set_id==rootscore_df_1$Location[i])]
}

write.csv(as.matrix(rootscore_df_1),'Data/1H/overall_score_raw_1H2017.csv')
write.csv(composite_score_2,'Data/1H/overall_score_rank_1H2017.csv')


overall_score_change = rootscore_df_2[,2:5] - rootscore_df_1[,2:5]
overall_score_rank_change = rootscore_rank_df_2[,2:5] - rootscore_rank_df_1[,2:5]
overall_star_change = composite_star_2[,2:5]-composite_star_1[,2:5]
overall_star_rank_change = composite_star_rank_2[,2:5] - composite_star_rank_1[,2:5]
rownames(overall_score_change) = rownames(overall_score_rank_change) = rownames(overall_star_change) = rownames(overall_star_rank_change) = rootscore_df_2$Location


write.csv(overall_score_change,'Data/Change/overall_score_raw_change2017.csv')
write.csv(overall_score_rank_change,'Data/Change/overall_score_rank_change2017.csv')
write.csv(overall_star_change,'Data/Change/overall_star_raw_change2017.csv')
write.csv(overall_star_rank_change,'Data/Change/overall_star_rank_change2017.csv')

