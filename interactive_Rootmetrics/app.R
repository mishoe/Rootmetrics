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
         selectInput("stars",
                     "Select Data:",
                      choices = names_all[,1]),
         selectInput("carrier",
                     "Select Carrier:",
                     choices = c('1 - AT.T','2 - Sprint','3 - T.Mobile','4 - Verizon','All'))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         textOutput('ind.cur')
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  name_H1<-list.files('data/1H')
  name_H2<-list.files('data/2H')
  
  names_all<-NULL
  for(i in 1:length(name_H1)){
    names_all<-rbind(names_all,c(substr(name_H1[i],1,nchar(name_H1[i])-4),paste('C:/RProjects/Rootmetrics/data/1H/',name_H1[i],'.csv',sep = '')))
  }
  
  for(i in 1:length(name_H2)){
    names_all<-rbind(names_all,c(substr(name_H2[i],1,nchar(name_H2[i])-4),paste('C:/RProjects/Rootmetrics/data/2H/',name_H2[i],'.csv',sep = '')))
  }
  
  
  filedata <- reactive({
    if (is.null(input$stars)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    if(grepl('1H2017',input$stars)==T){read.csv(paste('C:/RProjects/Rootmetrics/data/1H/',input$stars,'.csv',sep = ''))}
    else if(grepl('2H2017',input$stars)==T){read.csv(paste('C:/RProjects/Rootmetrics/data/2H/',input$stars,'.csv',sep = ''))}
    
  })
  ver.col<-rgb(236,7,16,maxColorValue = 255)
  tmob.col<-rgb(227,0,116,maxColorValue = 255)
  sprint.col<-rgb(251,223,0,maxColorValue = 255)
  att.col<-rgb(0,159,219,maxColorValue = 255)
  
  carriers.col<-c(att.col,sprint.col,tmob.col,ver.col)

  
  
  
  
  

  
  ind.cur<-reactive({which(names_all[,1]==input$stars)})
  x    <- reactive({read.csv(names_all[ind.cur(),2])})
  
   output$distPlot <- renderPlot({
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
     
     carr<-reactive({if(grepl('AT.T',input$carrier)==T){2}
       else if(grepl('Sprint',input$carrier)==T){3}
       else if(grepl('T.Mobile',input$carrier)==T){4}
       else if(grepl('Verizon',input$carrier)==T){5}
       })
     
     col.car<-reactive({if(grepl('AT.T',input$carrier)==T){rgb(0,159,219,maxColorValue = 255)}
       else if(grepl('Sprint',input$carrier)==T){rgb(251,223,0,maxColorValue = 255)}
       else if(grepl('T.Mobile',input$carrier)==T){rgb(227,0,116,maxColorValue = 255)}
       else if(grepl('Verizon',input$carrier)==T){rgb(236,7,16,maxColorValue = 255)}
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
     if(input$carrier=='All'){barplot(filedata1(), main="All Carriers",
             xlab="Score", beside=TRUE,col=carriers.col)}else{
               qplot(filedata()[,carr()],binwidth= bins(),col=I('black'),fill=I(col.car()), geom = 'histogram', main = paste(input$carrier,substr(gsub('_',' ',input$stars),1,nchar(input$stars)-7)),xlim = c(xmin.plot(),xmax.plot()),xlab = 'Score',ylab='Frequency')
               
            }
               })
}

# Run the application 
shinyApp(ui = ui, server = server)

