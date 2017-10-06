library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
dashboardPage(skin="blue",
              dashboardHeader(title="PDF Extractor",titleWidth=300),
              
              dashboardSidebar(width=250,
                               sidebarMenu(
                                 br(),
                                 menuItem(tags$em("Provide PDFs",style="font-size:150%"),icon=icon("upload"),tabName="data"),
                                 menuItem(tags$em("Summaries",style="font-size:150%"),icon=icon("bar-chart-o"),tabName="summary"),
                                 menuItem(tags$em("Search and Filter",style="font-size:150%"),icon=icon("search"),tabName="search")
                                 
                                 
                                 
                                 )
                                 ),
              
              dashboardBody(
                tabItems(
                  tabItem(tabName="data",
                           
                          
                          br(),
                          br(),
                          tags$h4("A PDF document is not so great in terms of searching and indexing
                                  and it becomes an overwhelming task to search through many documents 
                                  individually or compare two or more documents manually."),
                        


                      tags$h4("This application helps to get useful insights from PDF documents 
                           by creating visualizations and summarizations. It also enables searching, sorting and filtering. 
                           We can browse through lots of documents in a single click and
                           get a summary and comparison of the documents instantly.",style="color:#009900"),

                      tags$h4("Upload PDF documents (books, journals, surveys, etc.), then go to the", tags$span("Summaries",style="color:red"), tags$span("section in the sidebar to get summaries of the uploaded documents.
                              We can search one or more terms and see their distribution across the uploaded documents in 
                              the" , tags$span("Search and Filter",style="color:red"), tags$span("menu item. We can also filter to display words with certain frequency range only."))),
                      
                      
                          br(),
                          br(),
                          br(),
                      
                      column(width = 4,
                             fileInput('file1', em('Choose PDF File',style="text-align:center;color:red;font-size:120%"),multiple = TRUE,
                                       accept=c('.pdf')),
                          
#                           column(width = 5,
#                           textInput("link", label = p("Provide link(s) to your PDF document(s) separated by comma",style="text-align:center;color:#990099;font-size:110%"),
#                                     value = ""),
#                           
#                                          br()
#                           ),
#                           
#                           column(width = 3,
#                                          
#                                     tags$h3("And/Or", style="text-align:center;color:blue;font-size:120%"),
#                           br()
#                          
#                           ),
#                           column(width = 4,
#                           fileInput('file1', em('Choose PDF File',style="text-align:center;color:red;font-size:120%"),multiple = TRUE,
#                                     accept=c('.pdf')),
                          
                          br(),
                           br(),
                           br(),
                          br()
                          ),
                          br()
                          
                        ),
                  
                  
                  tabItem(tabName="summary",
                          fluidRow(
                            tabBox(width=12,
                                   tabPanel(tags$em("Word Cloud",style="font-size:150%"),
        
                                            column(width = 8,
                                          plotOutput("wordcloud")),
                                            
                                          column(width = 4,
                                                 br(),
                                          uiOutput("minfreq"),
                                              br(),
                                            uiOutput("maxwords"),
                                              br(),
                                          uiOutput("forEach"))
                                            
                                            ),
                                   
                                   
                                  tabPanel(tags$em("Plotly Bar graph",style="font-size:150%"),
                                            
                                            plotlyOutput("myplot",height = "700px"),
                                           br(),
                                   
                                              uiOutput("numwords")
                                           
                                           )
                                   
                                   
                                            
                                            
                                            ))),
             
                  
                  
                  
                  
                  tabItem(tabName="search",
                        
                            
                            DT::dataTableOutput("DataTable"),
                             br(),
                            uiOutput("text"),
                            uiOutput('forsearch'),
                            uiOutput('searchbutton'),
                            plotlyOutput("searched",height = '600px')
                          
                            
                            
                                       
                                       )
                          )))
         























