

library(shiny)
library(plotly)

shinyUI(fluidPage(
  
  sidebarLayout(
    sidebarPanel(width = 3,
      
      p("Paste the link to your PDF document and/or uppload a PDF document from your disk",
         style="color:#666633"),


      textInput("link", label = "Provide link to your PDF document",
                value = ""),
      
# actionButton("update", "Go"),
# 
# hr(),

      
      fileInput('file1', 'Choose PDF File',
                accept=c('.pdf')),
     
 

      sliderInput("freq",
                  em("Minimum Frequency:",style="text-align:center;color:#663300;font-size:80%"),
                  min = 1,  max = 50, value = 15),
      sliderInput("max",
                  em("Maximum Number of Words:",style="text-align:center;color:#663300;font-size:80%"),
                  min = 1,  max = 300,  value = 100)
     
    ),
    mainPanel(
      
      tags$h3('PDF Extraction and Summarization App',style="text-align:center;color:#4000ff;font-size:200%"),
      
fluidRow(
        column(width = 5,
               
              plotOutput("wordcloud")
             
              ),
        
        
        column(width = 7,
             
               
               plotlyOutput("myplot")
               
        ))
      
    )
  )
))

