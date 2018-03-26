create_heatmaps<-function(folder_to_save='Plots/Change/Heatmaps/'){
  ### Call Pull Data functions
  source('PullDataMethods.R')
  pull_data_2017_All(star=TRUE,score=TRUE)
  pull_data_2017_Compare(star=TRUE,score=TRUE)
  locat_corr = read.csv('Data/LocationLookup.csv')
  
  library(plotly)
  
  # 1-AT&T 2-Sprint 3-T-Mobile 4-Verizon
  carriers=c('AT&T','Sprint','T-Mobile','Verizon')
  categories = c('Call','Data','SMS','Speed')
  for (carrier_id in 2:(length(carriers)+1)){
    print(paste(c('Currently computing',carriers[(carrier_id-1)]),collapse=' '))
    callHeatMat = dataHeatMat = smsHeatMat = speedHeatMat = matrix(0,ncol=7,nrow=7)
    # run through locations and extract heatmap matricies
    for (locat_id in 1:nrow(smsScores_df_change)){
      callHeatMat[(4+callScoresRank_df_change[locat_id,carrier_id]),(4+callStarsRank_df_change[locat_id,carrier_id])]=callHeatMat[(4+callScoresRank_df_change[locat_id,carrier_id]),(4+callStarsRank_df_change[locat_id,carrier_id])]+1
      dataHeatMat[(4+dataScoresRank_df_change[locat_id,carrier_id]),(4+dataStarsRank_df_change[locat_id,carrier_id])]=dataHeatMat[(4+dataScoresRank_df_change[locat_id,carrier_id]),(4+dataStarsRank_df_change[locat_id,carrier_id])]+1
      smsHeatMat[(4+smsScoresRank_df_change[locat_id,carrier_id]),(4+smsStarsRank_df_change[locat_id,carrier_id])]=smsHeatMat[(4+smsScoresRank_df_change[locat_id,carrier_id]),(4+smsStarsRank_df_change[locat_id,carrier_id])]+1
      speedHeatMat[(4+speedScoresRank_df_change[locat_id,carrier_id]),(4+speedStarsRank_df_change[locat_id,carrier_id])]=speedHeatMat[(4+speedScoresRank_df_change[locat_id,carrier_id]),(4+speedStarsRank_df_change[locat_id,carrier_id])]+1
    }
    # plot heatmaps
    for (cat_id in 1:length(categories)){
      if(cat_id==1){plot_mat=callHeatMat
      }else if(cat_id==2){plot_mat=dataHeatMat
      }else if(cat_id==3){plot_mat=smsHeatMat
      }else{plot_mat=speedHeatMat}
      
      title = paste(c('Between System Change in 2017 for',carriers[(carrier_id-1)],'in',categories[cat_id]),collapse=' ')
      vals <- unique(scales::rescale(c(plot_mat)))
      o <- order(vals, decreasing = FALSE)
      cols <- scales::col_numeric("Blues", domain = NULL)(vals)
      colz <- setNames(data.frame(vals[o], cols[o]), NULL)
      
      filename = paste(c(folder_to_save,categories[cat_id],"_change_rank_",carriers[(carrier_id-1)],"_2017.png"),collapse = '')
      p=plot_ly(x=seq(-3,3,1),y=seq(-3,3,1),z = plot_mat, colorscale = colz,, type = "heatmap") %>%
        layout(
          title = title,
          xaxis = list(title = "Change in RootStar Rank"),
          yaxis = list(title = "Change in RootScore Rank")
        )
      export(p,filename)
    }
  }
}