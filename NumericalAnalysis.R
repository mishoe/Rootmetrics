### Call Pull Data functions
source('PullDataMethods.R')

pull_data_2017_All(star=TRUE,score=TRUE)
#pull_data_2017_Compare(star=TRUE,score=TRUE)
locat_corr = read.csv('Data/LocationLookup.csv')


call_rank_change_score_score = callScoresRank_df_2 - callScoresRank_df_1
call_rank_change_score_star = callStarsRank_df_2 - callScoresRank_df_1
data_rank_change_score_score = dataScoresRank_df_2 - dataScoresRank_df_1
data_rank_change_score_star = dataStarsRank_df_2 - dataScoresRank_df_1
sms_rank_change_score_score = smsScoresRank_df_2 - smsScoresRank_df_1
sms_rank_change_score_star = smsStarsRank_df_2 - smsScoresRank_df_1
speed_rank_change_score_score = speedScoresRank_df_2 - speedScoresRank_df_1
speed_rank_change_score_star = speedStarsRank_df_2 - speedScoresRank_df_1
#overall_rank_change_score_score =
#overall_rank_change_score_star = 

compare_system_var = data.frame(matrix(0,nrow=4,ncol=5))
colnames(compare_system_var) = c("Call","Data","Sms","Speed","Overall")
rownames(compare_system_var) = c("mu_score","mu_star","sigma_score","sigma_star")

compare_system_var[1,1] = mean(as.matrix(call_rank_change_score_score[2:5]))
compare_system_var[2,1] = mean(as.matrix(call_rank_change_score_star[2:5]))
compare_system_var[3,1] = sd(as.matrix(call_rank_change_score_score[2:5]))
compare_system_var[4,1] = sd(as.matrix(call_rank_change_score_star[2:5]))

compare_system_var[1,2] = mean(as.matrix(data_rank_change_score_score[2:5]))
compare_system_var[2,2] = mean(as.matrix(data_rank_change_score_star[2:5]))
compare_system_var[3,2] = sd(as.matrix(data_rank_change_score_score[2:5]))
compare_system_var[4,2] = sd(as.matrix(data_rank_change_score_star[2:5]))

compare_system_var[1,3] = mean(as.matrix(sms_rank_change_score_score[2:5]))
compare_system_var[2,3] = mean(as.matrix(sms_rank_change_score_star[2:5]))
compare_system_var[3,3] = sd(as.matrix(sms_rank_change_score_score[2:5]))
compare_system_var[4,3] = sd(as.matrix(sms_rank_change_score_star[2:5]))

compare_system_var[1,4] = mean(as.matrix(speed_rank_change_score_score[2:5]))
compare_system_var[2,4] = mean(as.matrix(speed_rank_change_score_star[2:5]))
compare_system_var[3,4] = sd(as.matrix(speed_rank_change_score_score[2:5]))
compare_system_var[4,4] = sd(as.matrix(speed_rank_change_score_star[2:5]))