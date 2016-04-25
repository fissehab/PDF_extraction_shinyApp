library(shiny)
library(pdftools)
library(stringr)
library(stringi)
library(tm)
library(ggplot2)
library(dplyr)
library(wordcloud)
library(plotly)



shinyServer(function(input, output) {

  options(shiny.maxRequestSize=500*1024^2) 

  mypdf1 <- reactive({
    
   
        withProgress({
         
          if(nchar(input$link)>0){
            setProgress(message = "Downloading Document...")
          pdf_text(str_trim(input$link))
               }else(return(NULL))
          
        })
    
    })


mypdf2<-reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      
      withProgress({
        setProgress(message = "Computing...")
      pdf_text(inFile$datapath)
      })
    }
    
  })
  


mymatrix<-reactive({


 
  
  txt=c(mypdf1(),mypdf2())
 

    
   if(is.null(txt))
     
     return(NULL)
    
    # Create corpus
    corpus=Corpus(VectorSource(txt))
    # Convert to lower-case
    corpus=tm_map(corpus,tolower)
    corpus = tm_map(corpus, PlainTextDocument)
    corpus = tm_map(corpus, removePunctuation)
    
    # Remove stopwords
    corpus=tm_map(corpus,function(x) removeWords(x,stopwords("english")))
    
    frequencies = DocumentTermMatrix(corpus)
    sparse = removeSparseTerms(frequencies,0.9)
    sparse =as.matrix(sparse)
    sparse=apply(sparse,2,sum)
    sparse=sparse[order(sparse,decreasing = T)]
    
    Term=names(sparse)
    Frequency=as.vector(sparse)
    
    sparse=as.data.frame(list(Term=Term,Frequency=Frequency))
    sparse$Term = stri_trans_totitle(sparse$Term) 
    sparse
   
 
  })


  
 
  
  
  
  
  output$wordcloud <- renderPlot({
    
    if(is.null(mymatrix()))
      return(NULL)
    
      sparse=mymatrix()
      
    pal2 <- brewer.pal(8,"Dark2")
    
    wordcloud(sparse$Term,sparse$Frequency, min.freq=input$freq, max.words=input$max,
              random.order=FALSE,
              rot.per=0.0, use.r.layout=FALSE, colors=pal2)
    
  })
  
  
  output$myplot <- renderPlotly({
    
    if(is.null(mymatrix()))
      return(NULL)
    sparse=mymatrix()
    sparse=sparse[1:25,]
    
    q=sparse%>%mutate(Term = factor(Term,levels = Term[order(Frequency,decreasing =F)]))%>%
      ggplot(aes(x=Term,y=Frequency))+geom_bar(stat='identity',color='#c2c2a3',fill='#b35900')+
      xlab("")+ggtitle('Most frequent words')+
      coord_flip()+ylab('Frequency')
    
    p=ggplotly(q)
    p
    
    
  })
  


})

