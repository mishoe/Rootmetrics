#app to display data

library(shiny)
library(ggplot2)
library(leaflet)
source('../IndividualScoreScript.R')



# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Rootmetrics RootStar Vs RootScore Analysis"
  ),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
      
      tabPanel('Individual',
               submitButton(text = "Apply Changes", icon = NULL, width = NULL),
               textInput("userloc","Input User Location:",'66 George Street, Charleston, SC'),
               selectInput('carrier','select which carrier',choices = c('AT&T','Sprint','T-Mobile','Verizon')),
               numericInput('milesrad','Input a Radius to check:',value=5,min=0,max=100)),
      tabPanel('Raw Distributions',
               submitButton(text = "Apply Changes", icon = NULL, width = NULL),
               selectInput("carrier1",
                           "Select Carrier 1:",
                           choices = c('1 - AT.T','2 - Sprint','3 - T.Mobile','4 - Verizon','All')),
               selectInput("carrier2",
                           "Select Carrier 2:",
                           choices = c('1 - AT.T','2 - Sprint','3 - T.Mobile','4 - Verizon','All')),
               selectInput("data_type",
                           "Select Data Type:",
                           choices = c('call','data','sms','speed')),
               selectInput("stars",
                           "Select Method:",
                           choices = c('star_raw','star_rank','score_raw','score_rank')),
               selectInput("Year_Half",
                           "Select Time Period:",
                           choices = c('1st Half','2nd Half'))
               
      ),
      tabPanel('Comparisons',
               submitButton(text = "Apply Changes", icon = NULL, width = NULL),
               selectInput("carrcomp1",
                           "Carrier 1 to Explore:",
                           choices = c('1 - AT.T','2 - Sprint','3 - T.Mobile','4 - Verizon')),
               selectInput("carrcomp2",
                           "Carrier 2 to Explore:",
                           choices = c('4 - Verizon','3 - T.Mobile','2 - Sprint','1 - AT.T')),
               selectInput("data_type1",
                           "Select Data Type:",
                           choices = c('call','data','sms','speed')),
               selectInput("Year_Halfcomp1",
                           "Select time period 1:",
                           choices = c('1st Half','2nd Half')),
               selectInput("syscomp1",
                           "select first system to see change from:",
                           choices = c('RootScore','RootStar')),
               selectInput("Year_Halfcomp2",
                           "Select time period 2:",
                           choices = c('2nd Half','1st Half')),
               selectInput("syscomp2",
                           "select second system to see change to:",
                           choices = c('RootScore','RootStar'))
      )
      
      
    )
    
    
    ),
    
    # Show a plot of the generated distribution
    
    mainPanel(tabsetPanel(
      tabPanel('individual Scores',leafletOutput('mapplot')),
      tabPanel('Raw Distributions', 
               column(width =  8,plotOutput("distPlot",height = '100%'),plotOutput("distPlot1")),column(width=4,textOutput('mean1'),br(),textOutput('sd1'),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),textOutput('mean2'),br(),textOutput('sd2')))
      ,tabPanel('Comparisons',align='center',textOutput('titlecomp'),
                tags$head(tags$style("#titlecomp{color: black;
                                 font-size: 20px;
                                   text-align: center;
                                     }")),
                br(),br(),plotOutput('comp11'),tableOutput('tablechanges'),column(width=6,tableOutput('table1data')),column(width=6,tableOutput('table2data')))
    )
    
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #first carrier
  #import the currently selected data
  #data file for comparing the distributions
  filedata <- reactive({
    if (is.null(input$stars)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    if(input$Year_Half=='1st Half'){read.csv(paste('../Data/1H/',input$data_type,'_',input$stars,'_1H2017.csv',sep = ''))}
    else if(input$Year_Half=='2nd Half'){read.csv(paste('../Data/2H/',input$data_type,'_',input$stars,'_2H2017.csv',sep = ''))}
    
  })
  

  
  
  
  ver.col<-rgb(236,7,16,maxColorValue = 255)
  tmob.col<-rgb(227,0,116,maxColorValue = 255)
  sprint.col<-rgb(251,223,0,maxColorValue = 255)
  att.col<-rgb(0,159,219,maxColorValue = 255)
  
  carriers.col<-c(att.col,sprint.col,tmob.col,ver.col)
  
  
  
  ind.cur<-reactive({which(names_all[,1]==input$stars)})
  x    <- reactive({read.csv(names_all[ind.cur(),2])})
  
  
  output$test<-renderText({'stuff goes here'})
  
  #carrier plot 1
  output$distPlot <- renderPlot(height = 300,{
    # draw the histogram with the specified number of bins
    bins<-reactive({if(grepl('score_raw',input$stars)==T){1}
      else if(grepl('star_raw',input$stars)==T){.25}
      else if(grepl('rank',input$stars)==T){.5}})
    
    xmin.plot<-reactive({if(grepl('score_raw',input$stars)==T){70}
      else if(grepl('star_raw',input$stars)==T){0}
      else if(grepl('rank',input$stars)==T){0}})
    
    xmax.plot<-reactive({if(grepl('score_raw',input$stars)==T){100.5}
      else if(grepl('star_raw',input$stars)==T){5.5}
      else if(grepl('rank',input$stars)==T){4.5}})
    
    carr<-reactive({if(grepl('AT.T',input$carrier1)==T){2}
      else if(grepl('Sprint',input$carrier1)==T){3}
      else if(grepl('T.Mobile',input$carrier1)==T){4}
      else if(grepl('Verizon',input$carrier1)==T){5}
    })
    
    col.car<-reactive({if(grepl('AT.T',input$carrier1)==T){rgb(0,159,219,maxColorValue = 255)}
      else if(grepl('Sprint',input$carrier1)==T){rgb(251,223,0,maxColorValue = 255)}
      else if(grepl('T.Mobile',input$carrier1)==T){rgb(227,0,116,maxColorValue = 255)}
      else if(grepl('Verizon',input$carrier1)==T){rgb(236,7,16,maxColorValue = 255)}
    })
    
    
    filedata1<-reactive({
      
      if(grepl('score_raw',input$stars)==T){
        data.cur<-matrix(0,nrow = (xmax.plot()-xmin.plot())/bins()+1,ncol = 4)
        
        for(j in 1:4){
          for(i in seq(xmin.plot(),xmax.plot(),bins())){
            data.cur[(i-xmin.plot())/bins(),j]<-length(which(filedata()[,j+1]>=i & filedata()[,j+1]<(i+bins())))
          }}
        row.names(data.cur)<-seq(xmin.plot(),xmax.plot(),bins())
      }
      else{
        data.cur<-matrix(0,nrow = length(seq(0,xmax.plot(),bins()*2)),ncol = 4)
        for(j in 1:4){
          for(i in seq(0,xmax.plot(),bins()*2)){
            data.cur[i/(bins()*2)+1,j]<-length(which(filedata()[,j+1]==i))
            
          }}
        row.names(data.cur)<-seq(xmin.plot(),xmax.plot(),bins()*2)
      }
      
      
      
      
      
      colnames(data.cur)<-colnames(filedata())[2:5]
      data.cur<-t(data.cur)
      
      return(data.cur)
    })
    
    
    #qplot(filedata()[,carr()],binwidth= bins(),fill=I(col.car()), geom = 'histogram', main = paste(input$carrier,substr(gsub('_',' ',input$stars),1,nchar(input$stars)-7)),xlim = c(xmin.plot(),xmax.plot()),xlab = 'Score',ylab='Frequency')
    if(input$carrier1=='All'){barplot(filedata1(), main="All Carriers",
                                      xlab="Score", beside=TRUE,col=carriers.col)}else{
                                        qplot(filedata()[,carr()],binwidth= bins(),col=I('black'),fill=I(col.car()), geom = 'histogram', main = paste(substr(input$carrier1,5,nchar(input$carrier1)),input$data_type,input$stars,input$Year_Half,sep = ' '),xlim = c(xmin.plot(),xmax.plot()),xlab = 'Score',ylab='Frequency')
                                        
                                      }
  })
  output$mean1<-renderText({
    carr<-reactive({if(grepl('AT.T',input$carrier1)==T){2}
      else if(grepl('Sprint',input$carrier1)==T){3}
      else if(grepl('T.Mobile',input$carrier1)==T){4}
      else if(grepl('Verizon',input$carrier1)==T){5}
    })
    
    if(input$carrier1=='All'){paste('Mean All:',round(mean(unlist(filedata()[,2:5])),digits = 4))
    }else(paste('Mean Carrier 1:',round(mean(filedata()[,carr()]),digits = 4)))
    
    })
  output$sd1<-renderText({
    carr<-reactive({if(grepl('AT.T',input$carrier1)==T){2}
      else if(grepl('Sprint',input$carrier1)==T){3}
      else if(grepl('T.Mobile',input$carrier1)==T){4}
      else if(grepl('Verizon',input$carrier1)==T){5}
    })
    if(input$carrier1=='All'){paste('Sigma All:',round(sd(unlist(filedata()[,2:5])),digits = 4))
    }else(paste('Sigma Carrier 1:',round(sd(filedata()[,carr()]),digits = 4)))

    
  })
  
  
  #carrier 2 plot
  output$distPlot1 <- renderPlot(height = 300,{
    # draw the histogram with the specified number of bins
    bins<-reactive({if(grepl('score_raw',input$stars)==T){1}
      else if(grepl('star_raw',input$stars)==T){.25}
      else if(grepl('rank',input$stars)==T){.5}})
    
    xmin.plot<-reactive({if(grepl('score_raw',input$stars)==T){70}
      else if(grepl('star_raw',input$stars)==T){0}
      else if(grepl('rank',input$stars)==T){0}})
    
    xmax.plot<-reactive({if(grepl('score_raw',input$stars)==T){100.5}
      else if(grepl('star_raw',input$stars)==T){5.5}
      else if(grepl('rank',input$stars)==T){4.5}})
    
    carr<-reactive({if(grepl('AT.T',input$carrier2)==T){2}
      else if(grepl('Sprint',input$carrier2)==T){3}
      else if(grepl('T.Mobile',input$carrier2)==T){4}
      else if(grepl('Verizon',input$carrier2)==T){5}
    })
    
    col.car<-reactive({if(grepl('AT.T',input$carrier2)==T){rgb(0,159,219,maxColorValue = 255)}
      else if(grepl('Sprint',input$carrier2)==T){rgb(251,223,0,maxColorValue = 255)}
      else if(grepl('T.Mobile',input$carrier2)==T){rgb(227,0,116,maxColorValue = 255)}
      else if(grepl('Verizon',input$carrier2)==T){rgb(236,7,16,maxColorValue = 255)}
    })
    
    
    filedata1<-reactive({
      
      if(grepl('score_raw',input$stars)==T){
        data.cur<-matrix(0,nrow = (xmax.plot()-xmin.plot())/bins()+1,ncol = 4)
        
        for(j in 1:4){
          for(i in seq(xmin.plot(),xmax.plot(),bins())){
            data.cur[(i-xmin.plot())/bins(),j]<-length(which(filedata()[,j+1]>=i & filedata()[,j+1]<(i+bins())))
          }}
        row.names(data.cur)<-seq(xmin.plot(),xmax.plot(),bins())
      }
      else{
        data.cur<-matrix(0,nrow = length(seq(0,xmax.plot(),bins()*2)),ncol = 4)
        for(j in 1:4){
          for(i in seq(0,xmax.plot(),bins()*2)){
            data.cur[i/(bins()*2)+1,j]<-length(which(filedata()[,j+1]==i))
            
          }}
        row.names(data.cur)<-seq(xmin.plot(),xmax.plot(),bins()*2)
      }
      
      
      
      
      
      colnames(data.cur)<-colnames(filedata())[2:5]
      data.cur<-t(data.cur)
      
      return(data.cur)
    })
    
    
    if(input$carrier2=='All'){barplot(filedata1(), main="All Carriers",
                                      xlab="Score", beside=TRUE,col=carriers.col)}else{qplot(filedata()[,carr()],binwidth= bins(),col=I('black'),fill=I(col.car()), geom = 'histogram', main = paste(substr(input$carrier2,5,nchar(input$carrier2)),input$data_type,input$stars,input$Year_Half,sep = ' '),xlim = c(xmin.plot(),xmax.plot()),xlab = 'Score',ylab='Frequency')
                                        
                                      }
  })
  output$mean2<-renderText({
    carr<-reactive({if(grepl('AT.T',input$carrier2)==T){2}
      else if(grepl('Sprint',input$carrier2)==T){3}
      else if(grepl('T.Mobile',input$carrier2)==T){4}
      else if(grepl('Verizon',input$carrier2)==T){5}
      
    })
    
    if(input$carrier2=='All'){paste('Mean All:',round(mean(unlist(filedata()[,2:5])),digits = 4))
    }else(paste('Mean Carrier 2:',round(mean(filedata()[,carr()]),digits = 4)))
    
  })
  output$sd2<-renderText({
    carr<-reactive({if(grepl('AT.T',input$carrier2)==T){2}
      else if(grepl('Sprint',input$carrier2)==T){3}
      else if(grepl('T.Mobile',input$carrier2)==T){4}
      else if(grepl('Verizon',input$carrier2)==T){5}
    })
    
    if(input$carrier2=='All'){
      paste('Sigma All:',round(sd(unlist(filedata()[,2:5])),digits = 4))
    }else(paste('Sigma Carrier 2:',round(sd(filedata()[,carr()]),digits = 4)))
  })
  
  
  
  
  
  
  #data file for first portion comparison plot 1
  filedata2 <- reactive({
    #build the file name
    if(input$Year_Halfcomp1=='1st Half'){
      read.csv(paste('../Data/1H/',input$data_type1,if(input$syscomp1=='RootScore'){'_score_rank'}else{'_star_rank'},'_1H2017.csv',sep = ''))
    }else{read.csv(paste('../Data/2H/',input$data_type1,if(input$syscomp1=='RootScore'){'_score_rank'}else{'_star_rank'},'_2H2017.csv',sep = ''))
      
      
    }
    
  })
  
  #data file for first portion comparison plot 2
  filedata3 <- reactive({
    #build the file name
    if(input$Year_Halfcomp2=='1st Half'){
      read.csv(paste('../Data/1H/',input$data_type1,if(input$syscomp2=='RootScore'){'_score_rank'}else{'_star_rank'},'_1H2017.csv',sep = ''))
    }else{read.csv(paste('../Data/2H/',input$data_type1,if(input$syscomp2=='RootScore'){'_score_rank'}else{'_star_rank'},'_2H2017.csv',sep = ''))
      
      
    }
    
  })
  
  #comparison plot
  output$comp11 <- renderPlot(height = 300,{

    col.car1<-reactive({if(grepl('AT.T',input$carrcomp1)==T){rgb(0,159,219,maxColorValue = 255)}
      else if(grepl('Sprint',input$carrcomp1)==T){rgb(251,223,0,maxColorValue = 255)}
      else if(grepl('T.Mobile',input$carrcomp1)==T){rgb(227,0,116,maxColorValue = 255)}
      else if(grepl('Verizon',input$carrcomp1)==T){rgb(236,7,16,maxColorValue = 255)}
    })
    
    col.car2<-reactive({if(grepl('AT.T',input$carrcomp2)==T){rgb(0,159,219,maxColorValue = 255)}
      else if(grepl('Sprint',input$carrcomp2)==T){rgb(251,223,0,maxColorValue = 255)}
      else if(grepl('T.Mobile',input$carrcomp2)==T){rgb(227,0,116,maxColorValue = 255)}
      else if(grepl('Verizon',input$carrcomp2)==T){rgb(236,7,16,maxColorValue = 255)}
    })
    
    
    carr1<-reactive({if(grepl('AT.T',input$carrcomp1)==T){2}
      else if(grepl('Sprint',input$carrcomp1)==T){3}
      else if(grepl('T.Mobile',input$carrcomp1)==T){4}
      else if(grepl('Verizon',input$carrcomp1)==T){5}
    })
    carr2<-reactive({if(grepl('AT.T',input$carrcomp2)==T){2}
      else if(grepl('Sprint',input$carrcomp2)==T){3}
      else if(grepl('T.Mobile',input$carrcomp2)==T){4}
      else if(grepl('Verizon',input$carrcomp2)==T){5}
    })
    
    
    
    DF <- rbind(data.frame(fill=col.car1(), obs=filedata3()[,carr1()]-filedata2()[,carr1()]),
                data.frame(fill=col.car2(), obs=filedata3()[,carr2()]-filedata2()[,carr2()]))
    
    ggplot(DF, aes(x=obs, fill=fill)) +
      geom_histogram(binwidth = 1 ,colour="black", position="dodge") +
      scale_fill_identity()+
      labs(title(main = paste('Comparing',input$data_type1,'rankings of',input$carrcomp1,'and',input$carrcomp2,'change from',input$Year_Halfcomp1,input$syscomp1, 'to',input$Year_Halfcomp2,input$syscomp2)),
           x='Rank Change',y='Frequency')
    
    
  })
  
  output$titlecomp<-renderText({paste('Comparing',input$data_type1,'rankings of',substr(input$carrcomp1,5,nchar(input$carrcomp1)),'and',substr(input$carrcomp2,5,nchar(input$carrcomp2)),'change from',input$Year_Halfcomp1,input$syscomp1, 'to',input$Year_Halfcomp2,input$syscomp2)})
  
  output$mapplot<-renderLeaflet({
    generate_individ_scores(targ_locat =input$userloc,carrier=input$carrier,radius_miles = input$milesrad,in_app=T)
    })
  
  output$tablechanges<-renderTable({
    carr1<-reactive({if(grepl('AT.T',input$carrcomp1)==T){2}
      else if(grepl('Sprint',input$carrcomp1)==T){3}
      else if(grepl('T.Mobile',input$carrcomp1)==T){4}
      else if(grepl('Verizon',input$carrcomp1)==T){5}
    })
    carr2<-reactive({if(grepl('AT.T',input$carrcomp2)==T){2}
      else if(grepl('Sprint',input$carrcomp2)==T){3}
      else if(grepl('T.Mobile',input$carrcomp2)==T){4}
      else if(grepl('Verizon',input$carrcomp2)==T){5}
    })
    
    tab.mean<-round(apply(as.matrix(filedata3()[,2:5]-filedata2()[,2:5]),2,mean),digits = 3)
    tab.sd<-round(apply(as.matrix(filedata3()[,2:5]-filedata2()[,2:5]),2,sd),digits=3)
    table.change<-rbind(tab.mean,tab.sd)

    table.change<-cbind(c('Mean of change','SD of change'),table.change)
    colnames(table.change)<-c(' ','AT&T','Sprint','T-Mobile','Verizon')
    table.change
    
    })
  
  
  output$table1data<-renderTable({

    
    tab1.mean<-round(apply(as.matrix(filedata2()[,2:5]),2,mean),digits = 3)
    tab1.sd<-round(apply(as.matrix(filedata2()[,2:5]),2,sd),digits=3)
    table1data<-rbind(tab1.mean,tab1.sd)
    
    table1data<-cbind(c('Mean of 1st data set','SD 1st data set'),table1data)
    colnames(table1data)<-c(' ','AT&T','Sprint','T-Mobile','Verizon')
    table1data
    
  })
  
  
  output$table2data<-renderTable({

    
    tab2.mean<-round(apply(as.matrix(filedata3()[,2:5]),2,mean),digits = 3)
    tab2.sd<-round(apply(as.matrix(filedata3()[,2:5]),2,sd),digits=3)
    table2data<-rbind(tab2.mean,tab2.sd)
    
    table2data<-cbind(c('Mean of 2nd data set','SD 2nd data set'),table2data)
    colnames(table2data)<-c(' ','AT&T','Sprint','T-Mobile','Verizon')
    table2data
    
  })
  
  
  
  
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)

