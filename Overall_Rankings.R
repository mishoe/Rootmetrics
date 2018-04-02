#build the composite scores for the star methods


source('PullDataMethods.R')
pull_data_2017_1H(score = T)
pull_data_2017_2H(score = T)
pull_data_2017_All()
pull_data_2017_Compare()


composite_star_1<-round(.4*callStars_df_1+.275*dataStars_df_1+.275*speedStars_df_1+.05*smsStars_df_1,digits = 2)
composite_star_2<-round(.4*callStars_df_2+.275*dataStars_df_2+.275*speedStars_df_2+.05*smsStars_df_2,digits = 2)


colnames(composite_star_1)<-c('Location','AT.T','Sprint','T.Mobile','Verizon')
colnames(composite_star_2)<-c('Location','AT.T','Sprint','T.Mobile','Verizon')



write.csv(composite_star_1,'Data/1H/overallstar_df_1.csv')
write.csv(composite_star_2,'Data/2H/overallstar_df_2.csv')


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
colnames(composite_star_rank_1)<-c('Location','AT.T','Sprint','T.Mobile','Verizon')

write.csv(composite_star_rank_1,'Data/1H/overallstar_rank_1.csv')






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
colnames(composite_star_rank_2)<-c('Location','AT.T','Sprint','T.Mobile','Verizon')

write.csv(composite_star_rank_2,'Data/2H/overallstar_rank_2.csv')







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
colnames(composite_score_2)<-c('Location','AT.T','Sprint','T.Mobile','Verizon')
ord.all<-NULL
for(i in 1:length(composite_score_2[,1])){
  ord.all<-c(ord.all,which(composite_score_2[,1]==as.numeric(LocationLookup[i,2])))
}
composite_score_2<-composite_score_2[ord.all,]




write.csv(composite_score_2,'Data/2H/overallscorerank_df_2.csv')


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
colnames(composite_score_2)<-c('Location','AT.T','Sprint','T.Mobile','Verizon')
ord.all<-NULL
for(i in 1:length(composite_score_2[,1])){
  ord.all<-c(ord.all,which(composite_score_2[,1]==as.numeric(LocationLookup[i,1])))
}
composite_score_2<-composite_score_2[ord.all,]




write.csv(composite_score_2,'Data/1H/overallscorerank_df_1.csv')
