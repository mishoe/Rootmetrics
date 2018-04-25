#app to display data

library(shiny)
library(ggplot2)
library(leaflet)
source('../IndividualScoreScript.R')



# Define UI for application that draws a histogram
ui <- navbarPage(h4('Rootmetrics RootStar Vs RootScore Analysis'),
      
      tabPanel('Intro Page',
               tags$style(HTML("
                               .container-fluid {background-color: #dbe8ff;}
                               .well {background-color:[#dbe8ff];}
                               body {background-color: #dbe8ff; }
                               a:link {
                               color: #707070; 
                               background-color: transparent; 
                               text-decoration: none;
                               }
                               h1{color: #707070;}")),
               h1('Thank You For Using Our Application'),column(width=12,htmltools::HTML('<p><img src="4AA_LogoHorizontal_Color_rgb.jpg"/></p>')),
                                                                column(width = 6,h5('This application was developed through a partnership with Rootmetrics and the College of Charleston by John Allen and Austin Mishoe of Four Alpha Analytics.
                                                                                    Four Alpha Analytics is a data science group in Charleston, SC wrorking to help businesses and academics explore data in a meaningful way.
                                                                                    For more information about Four Alpha Analytics please visit our website.'),actionButton(inputId='ab1', label="Visit Our Website", icon = icon("far fa-check-square"),onclick ="location.href='http://www.fouralphagroup.com';")),column(width = 6,leafletOutput('ourloc'))

               
               ),
      tabPanel('individual Scores',column(width=3,
                                          textInput("userloc","Input User Location:",'66 George Street, Charleston, SC'),
                                          selectInput('carrier','select which carrier',choices = c('AT&T','Sprint','T-Mobile','Verizon')),
                                          numericInput('milesrad','Input a Radius to check:',value=5,min=0,max=100),
                                          submitButton(text = "Apply Changes", icon = NULL, width = NULL)
      ),
      column(width=9,wellPanel(leafletOutput('mapplot'),style = "max-height: 800px"))),
      tabPanel('Raw Distributions', column(width=3,
                                           
                                           selectInput("carrier1",
                                                       "Select Carrier 1:",
                                                       choices = c('1 - AT.T','2 - Sprint','3 - T.Mobile','4 - Verizon','All')),
                                           selectInput("Year_Half",
                                                       "Select Carrier 1 Time Period:",
                                                       choices = c('1st Half','2nd Half')),
                                           selectInput("stars",
                                                       "Select carrier 1 Method:",
                                                       choices = c('star_raw','star_rank','score_raw','score_rank')),
                                           selectInput("carrier2",
                                                       "Select Carrier 2:",
                                                       choices = c('1 - AT.T','2 - Sprint','3 - T.Mobile','4 - Verizon','All')),
                                           selectInput("Year_Half1",
                                                       "Select Carrier 2 Time Period:",
                                                       choices = c('1st Half','2nd Half')),
                                           selectInput("stars1",
                                                       "Select Carrier 2 Method:",
                                                       choices = c('star_raw','star_rank','score_raw','score_rank')),
                                           selectInput("data_type",
                                                       "Select Data Type:",
                                                       choices = c('call','data','sms','speed','overall')),
                                           submitButton(text = "Apply Changes", icon = NULL, width = NULL)
      ),
      column(width =  4,plotOutput("distPlot",height = '100%'),textOutput('mean1'),textOutput('sd1')),column(width =  4,plotOutput("distPlot1",height = '100%'),textOutput('mean2'),textOutput('sd2')))
      ,tabPanel('Comparisons',align='center',column(width = 3,
        selectInput("data_type1",
                    "Select Data Type:",
                    choices = c('call','data','sms','speed','overall')),
        selectInput("carrcomp1",
                    "Carrier 1:",
                    choices = c('1 - AT.T','2 - Sprint','3 - T.Mobile','4 - Verizon')),
        selectInput("Year_Halfcomp1",
                    "Select Time 1:",
                    choices = c('1st Half','2nd Half')),
        selectInput("syscomp1",
                    "select Data 1 Sys.:",
                    choices = c('RootScore','RootStar')),
        br(),
        selectInput("carrcomp2",
                    "Carrier 2:",
                    choices = c('4 - Verizon','3 - T.Mobile','2 - Sprint','1 - AT.T')),
        selectInput("Year_Halfcomp2",
                    "Select Time 2:",
                    choices = c('2nd Half','1st Half')),
        selectInput("syscomp2",
                    "select Data 2 Sys.:",
                    choices = c('RootScore','RootStar')),
        submitButton(text = "Apply Changes", icon = NULL, width = NULL)
      ),
      
      wellPanel(textOutput('titlecomp'),
                          tags$head(tags$style("#titlecomp{color: black;
                                                          font-size: 20px;
                                                          text-align: center;
                                                            }")),
                          column(width=6,plotOutput('comp11')),column(width=6,h4('Data Set 1'), tableOutput('table1data'),h4('Data Set 2'),tableOutput('table2data'),h4('Changes Between System'),tableOutput('tablechanges')),style = "overflow-y:scroll; max-height: 550px"))
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
  
  filedata4 <- reactive({
    if (is.null(input$stars1)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    if(input$Year_Half1=='1st Half'){read.csv(paste('../Data/1H/',input$data_type,'_',input$stars1,'_1H2017.csv',sep = ''))}
    else if(input$Year_Half1=='2nd Half'){read.csv(paste('../Data/2H/',input$data_type,'_',input$stars1,'_2H2017.csv',sep = ''))}
    
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
    if(input$carrier1=='All'){barplot(filedata1(), main="All Carriers",ylim = c(0,100),
                                      xlab="Score", beside=TRUE,col=carriers.col)}else{
                                        qplot(filedata()[,carr()],binwidth= bins(),col=I('black'),fill=I(col.car()), geom = 'histogram', main = paste(substr(input$carrier1,5,nchar(input$carrier1)),input$data_type,input$stars,input$Year_Half,sep = ' '),xlim = c(xmin.plot(),xmax.plot()),ylim = c(0,125),xlab = 'Score',ylab='Frequency')
                                        
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
    bins<-reactive({if(grepl('score_raw',input$stars1)==T){1}
      else if(grepl('star_raw',input$stars1)==T){.25}
      else if(grepl('rank',input$stars1)==T){.5}})
    
    xmin.plot<-reactive({if(grepl('score_raw',input$stars1)==T){70}
      else if(grepl('star_raw',input$stars1)==T){0}
      else if(grepl('rank',input$stars1)==T){0}})
    
    xmax.plot<-reactive({if(grepl('score_raw',input$stars1)==T){100.5}
      else if(grepl('star_raw',input$stars1)==T){5.5}
      else if(grepl('rank',input$stars1)==T){4.5}})
    
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
      
      if(grepl('score_raw',input$stars1)==T){
        data.cur<-matrix(0,nrow = (xmax.plot()-xmin.plot())/bins()+1,ncol = 4)
        
        for(j in 1:4){
          for(i in seq(xmin.plot(),xmax.plot(),bins())){
            data.cur[(i-xmin.plot())/bins(),j]<-length(which(filedata4()[,j+1]>=i & filedata()[,j+1]<(i+bins())))
          }}
        row.names(data.cur)<-seq(xmin.plot(),xmax.plot(),bins())
      }
      else{
        data.cur<-matrix(0,nrow = length(seq(0,xmax.plot(),bins()*2)),ncol = 4)
        for(j in 1:4){
          for(i in seq(0,xmax.plot(),bins()*2)){
            data.cur[i/(bins()*2)+1,j]<-length(which(filedata4()[,j+1]==i))
            
          }}
        row.names(data.cur)<-seq(xmin.plot(),xmax.plot(),bins()*2)
      }
      
      
      
      
      
      colnames(data.cur)<-colnames(filedata4())[2:5]
      data.cur<-t(data.cur)
      
      return(data.cur)
    })
    
    
    if(input$carrier2=='All'){barplot(filedata1(), main="All Carriers",ylim = c(0,100),
                                      xlab="Score", beside=TRUE,col=carriers.col)}else{qplot(filedata4()[,carr()],binwidth= bins(),col=I('black'),fill=I(col.car()), geom = 'histogram', main = paste(substr(input$carrier2,5,nchar(input$carrier2)),input$data_type1,input$stars1,input$Year_Half1,sep = ' '),xlim = c(xmin.plot(),xmax.plot()),ylim = c(0,125),xlab = 'Score',ylab='Frequency')
                                        
                                      }
  })
  output$mean2<-renderText({
    carr<-reactive({if(grepl('AT.T',input$carrier2)==T){2}
      else if(grepl('Sprint',input$carrier2)==T){3}
      else if(grepl('T.Mobile',input$carrier2)==T){4}
      else if(grepl('Verizon',input$carrier2)==T){5}
      
    })
    
    if(input$carrier2=='All'){paste('Mean All:',round(mean(unlist(filedata4()[,2:5])),digits = 4))
    }else(paste('Mean Carrier 2:',round(mean(filedata4()[,carr()]),digits = 4)))
    
  })
  output$sd2<-renderText({
    carr<-reactive({if(grepl('AT.T',input$carrier2)==T){2}
      else if(grepl('Sprint',input$carrier2)==T){3}
      else if(grepl('T.Mobile',input$carrier2)==T){4}
      else if(grepl('Verizon',input$carrier2)==T){5}
    })
    
    if(input$carrier2=='All'){
      paste('Sigma All:',round(sd(unlist(filedata4()[,2:5])),digits = 4))
    }else(paste('Sigma Carrier 2:',round(sd(filedata4()[,carr()]),digits = 4)))
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

    table.change<-cbind(c('Mean','SD'),table.change)
    colnames(table.change)<-c(' ','AT&T','Sprint','T-Mobile','Verizon')
    table.change
    
    })
  
  
  output$table1data<-renderTable({

    
    tab1.mean<-round(apply(as.matrix(filedata2()[,2:5]),2,mean),digits = 3)
    tab1.sd<-round(apply(as.matrix(filedata2()[,2:5]),2,sd),digits=3)
    table1data<-rbind(tab1.mean,tab1.sd)
    
    table1data<-cbind(c('Mean','SD'),table1data)
    colnames(table1data)<-c(' ','AT&T','Sprint','T-Mobile','Verizon')
    table1data
    
  })
  
  
  output$table2data<-renderTable({

    
    tab2.mean<-round(apply(as.matrix(filedata3()[,2:5]),2,mean),digits = 3)
    tab2.sd<-round(apply(as.matrix(filedata3()[,2:5]),2,sd),digits=3)
    table2data<-rbind(tab2.mean,tab2.sd)
    
    table2data<-cbind(c('Mean','SD'),table2data)
    colnames(table2data)<-c(' ','AT&T','Sprint','T-Mobile','Verizon')
    table2data
    
  })
  
  initial_lat = 32.7765
  initial_lng = -79.9311
  initial_zoom = 11
  
  output$ourloc<-renderLeaflet({
    m <- leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap)%>%  # Add default OpenStreetMap map tiles
    setView(lat = initial_lat, lng = initial_lng, zoom = initial_zoom)%>%
      addCircleMarkers(lat = initial_lat, lng = initial_lng,radius=10,color='#e88700',fillColor='#e88700',opacity=.7,fillOpacity = .7,stroke=F,
                       label="FOUR ALPHA ANALYTICS",
                       labelOptions = labelOptions(noHide = F, direction = "top",
                                                   style = list(
                                                     "color" = "#1a4919",
                                                     "font-family" = "serif",
                                                     "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                     "font-size" = "18px",
                                                     "border-color" = "rgba(0,0,0,0)"
                                                   )))
      
  })
  
  
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)

