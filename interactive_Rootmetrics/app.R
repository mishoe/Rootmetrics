#app to display data

library(shiny)
library(ggplot2)




# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Data Histograms"
  ),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("data_type",
                  "Select Data Type:",
                  choices = c('call','data','sms','speed')),
      selectInput("stars",
                  "Select Method:",
                  choices = c('star_raw','star_rank','score_raw','score_rank')),
      
      selectInput("Year_Half",
                  "Select Time Period:",
                  choices = c('1st Half','2nd Half')),
      selectInput("carrier1",
                  "Select Carrier 1:",
                  choices = c('1 - AT.T','2 - Sprint','3 - T.Mobile','4 - Verizon','All')),
      selectInput("carrier2",
                  "Select Carrier 2:",
                  choices = c('1 - AT.T','2 - Sprint','3 - T.Mobile','4 - Verizon','All'))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("distPlot1")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #first carrier
  #import the currently selected data
  filedata <- reactive({
    if (is.null(input$stars)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    if(input$Year_Half=='1st Half'){read.csv(paste('C:/RProjects/Rootmetrics/data/1H/',input$data_type,'_',input$stars,'_1H2017.csv',sep = ''))}
    else if(input$Year_Half=='2nd Half'){read.csv(paste('C:/RProjects/Rootmetrics/data/2H/',input$data_type,'_',input$stars,'_2H2017.csv',sep = ''))}
    
  })
  ver.col<-rgb(236,7,16,maxColorValue = 255)
  tmob.col<-rgb(227,0,116,maxColorValue = 255)
  sprint.col<-rgb(251,223,0,maxColorValue = 255)
  att.col<-rgb(0,159,219,maxColorValue = 255)
  
  carriers.col<-c(att.col,sprint.col,tmob.col,ver.col)
  
  
  
  
  
  ind.cur<-reactive({which(names_all[,1]==input$stars)})
  x    <- reactive({read.csv(names_all[ind.cur(),2])})
  #first plot
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
    
    
    #qplot(filedata()[,carr()],binwidth= bins(),fill=I(col.car()), geom = 'histogram', main = paste(input$carrier,substr(gsub('_',' ',input$stars),1,nchar(input$stars)-7)),xlim = c(xmin.plot(),xmax.plot()),xlab = 'Score',ylab='Frequency')
    if(input$carrier2=='All'){barplot(filedata1(), main="All Carriers",
                                      xlab="Score", beside=TRUE,col=carriers.col)}else{
                                        qplot(filedata()[,carr()],binwidth= bins(),col=I('black'),fill=I(col.car()), geom = 'histogram', main = paste(substr(input$carrier2,5,nchar(input$carrier2)),input$data_type,input$stars,input$Year_Half,sep = ' '),xlim = c(xmin.plot(),xmax.plot()),xlab = 'Score',ylab='Frequency')
                                        
                                      }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

