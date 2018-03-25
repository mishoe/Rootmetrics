pull_data_2017_All<-function(star=TRUE,score=FALSE){
  pull_data_2017_1H(star=star,score=score)
  pull_data_2017_2H(star=star,score=score)
}

pull_data_2017_1H<-function(star=TRUE,score=FALSE){
  if(star==TRUE){
    callStars_df_1<<-read.csv("Data/1H/call_star_raw_1H2017.csv",header = T)
    dataStars_df_1<<-read.csv("Data/1H/data_star_raw_1H2017.csv",header = T)
    smsStars_df_1<<-read.csv("Data/1H/sms_star_raw_1H2017.csv",header = T)
    speedStars_df_1<<-read.csv("Data/1H/speed_star_raw_1H2017.csv",header = T)
    callStarsRank_df_1<<-read.csv("Data/1H/call_star_rank_1H2017.csv",header = T)
    dataStarsRank_df_1<<-read.csv("Data/1H/data_star_rank_1H2017.csv",header = T)
    smsStarsRank_df_1<<-read.csv("Data/1H/sms_star_rank_1H2017.csv",header = T)
    speedStarsRank_df_1<<-read.csv("Data/1H/speed_star_rank_1H2017.csv",header = T)
  }
  if(score==TRUE){
    callScores_df_1<<-read.csv("Data/1H/call_score_raw_1H2017.csv",header = T)
    dataScores_df_1<<-read.csv("Data/1H/data_score_raw_1H2017.csv",header = T)
    smsScores_df_1<<-read.csv("Data/1H/sms_score_raw_1H2017.csv",header = T)
    speedScores_df_1<<-read.csv("Data/1H/speed_score_raw_1H2017.csv",header = T)
    callScoresRank_df_1<<-read.csv("Data/1H/call_score_rank_1H2017.csv",header = T)
    dataScoresRank_df_1<<-read.csv("Data/1H/data_score_rank_1H2017.csv",header = T)
    smsScoresRank_df_1<<-read.csv("Data/1H/sms_score_rank_1H2017.csv",header = T)
    speedScoresRank_df_1<<-read.csv("Data/1H/speed_score_rank_1H2017.csv",header = T)
  }
}

pull_data_2017_2H<-function(star=TRUE,score=FALSE){
  if(star==TRUE){
    callStars_df_2<<-read.csv("Data/2H/call_star_raw_2H2017.csv",header = T)
    dataStars_df_2<<-read.csv("Data/2H/data_star_raw_2H2017.csv",header = T)
    smsStars_df_2<<-read.csv("Data/2H/sms_star_raw_2H2017.csv",header = T)
    speedStars_df_2<<-read.csv("Data/2H/speed_star_raw_2H2017.csv",header = T)
    callStarsRank_df_2<<-read.csv("Data/2H/call_star_rank_2H2017.csv",header = T)
    dataStarsRank_df_2<<-read.csv("Data/2H/data_star_rank_2H2017.csv",header = T)
    smsStarsRank_df_2<<-read.csv("Data/2H/sms_star_rank_2H2017.csv",header = T)
    speedStarsRank_df_2<<-read.csv("Data/2H/speed_star_rank_2H2017.csv",header = T)
  }
  if(score==TRUE){
    callScores_df_2<<-read.csv("Data/2H/call_score_raw_2H2017.csv",header = T)
    dataScores_df_2<<-read.csv("Data/2H/data_score_raw_2H2017.csv",header = T)
    smsScores_df_2<<-read.csv("Data/2H/sms_score_raw_2H2017.csv",header = T)
    speedScores_df_2<<-read.csv("Data/2H/speed_score_raw_2H2017.csv",header = T)
    callScoresRank_df_2<<-read.csv("Data/2H/call_score_rank_2H2017.csv",header = T)
    dataScoresRank_df_2<<-read.csv("Data/2H/data_score_rank_2H2017.csv",header = T)
    smsScoresRank_df_2<<-read.csv("Data/2H/sms_score_rank_2H2017.csv",header = T)
    speedScoresRank_df_2<<-read.csv("Data/2H/speed_score_rank_2H2017.csv",header = T)
  }
}

pull_data_2017_Compare<-function(star=TRUE,score=FALSE){
  if(star==TRUE){
    callStars_df_change<<-read.csv("Data/Change/call_stars_raw_change2017.csv",header = T)
    dataStars_df_change<<-read.csv("Data/Change/data_stars_raw_change2017.csv",header = T)
    smsStars_df_change<<-read.csv("Data/Change/sms_stars_raw_change2017.csv",header = T)
    speedStars_df_change<<-read.csv("Data/Change/speed_stars_raw_change2017.csv",header = T)
    callStarsRank_df_change<<-read.csv("Data/Change/call_stars_rank_change2017.csv",header = T)
    dataStarsRank_df_change<<-read.csv("Data/Change/data_stars_rank_change2017.csv",header = T)
    smsStarsRank_df_change<<-read.csv("Data/Change/sms_stars_rank_change2017.csv",header = T)
    speedStarsRank_df_change<<-read.csv("Data/Change/speed_stars_rank_change2017.csv",header = T)
  }
  if(score==TRUE){
    callScores_df_change<<-read.csv("Data/Change/call_score_raw_change2017.csv",header = T)
    dataScores_df_change<<-read.csv("Data/Change/data_score_raw_change2017.csv",header = T)
    smsScores_df_change<<-read.csv("Data/Change/sms_score_raw_change2017.csv",header = T)
    speedScores_df_change<<-read.csv("Data/Change/speed_score_raw_change2017.csv",header = T)
    callScoresRank_df_change<<-read.csv("Data/Change/call_score_rank_change2017.csv",header = T)
    dataScoresRank_df_change<<-read.csv("Data/Change/data_score_rank_change2017.csv",header = T)
    smsScoresRank_df_change<<-read.csv("Data/Change/sms_score_rank_change2017.csv",header = T)
    speedScoresRank_df_change<<-read.csv("Data/Change/speed_score_rank_change2017.csv",header = T)
  }
}
setwd('documents/Consulting/Rootmetrics')
pull_data_2017_1H()
pull_data_2017_2H()
pull_data_2017_All()
pull_data_2017_Compare(star=TRUE,score=TRUE)

#Save data
setwd('documents/Consulting/Rootmetrics')

write.csv(callStars_df_2,"Data/2H/call_star_raw_2H2017.csv")
write.csv(dataStars_df_2,"Data/2H/data_star_raw_2H2017.csv")
write.csv(smsStars_df_2,"Data/2H/sms_star_raw_2H2017.csv")
write.csv(speedStars_df_2,"Data/2H/speed_star_raw_2H2017.csv")


write.csv(callStarsRank_df_2,"Data/2H/call_star_rank_2H2017.csv")
write.csv(dataStarsRank_df_2,"Data/2H/data_star_rank_2H2017.csv")
write.csv(smsStarsRank_df_2,"Data/2H/sms_star_rank_2H2017.csv")
write.csv(speedStarsRank_df_2,"Data/2H/speed_star_rank_2H2017.csv")


write.csv(callScores_df_2,"Data/2H/call_score_raw_2H2017.csv")
write.csv(dataScores_df_2,"Data/2H/data_score_raw_2H2017.csv")
write.csv(smsScores_df_2,"Data/2H/sms_score_raw_2H2017.csv")
write.csv(speedScores_df_2,"Data/2H/speed_score_raw_2H2017.csv")


write.csv(callScoresRank_df_2,"Data/2H/call_score_rank_2H2017.csv")
write.csv(dataScoresRank_df_2,"Data/2H/data_score_rank_2H2017.csv")
write.csv(smsScoresRank_df_2,"Data/2H/sms_score_rank_2H2017.csv")
write.csv(speedScoresRank_df_2,"Data/2H/speed_score_rank_2H2017.csv")
