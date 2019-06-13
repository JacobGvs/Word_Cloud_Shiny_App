library(shiny)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)

shinyServer(function(input,output, session) {
  text_data <- reactive({
    #change when the update button is pressed, but not for anything else
    input$Update
    #the isolate is just waiting when the update is processing
    isolate ({
      withProgress({
        # this is to show the end user of the progress
        setProgress(message = "Processing corpus...")
        
        #create input file, if there is no file input, it will analysis the text of the text mining which I put it.
        text_file <- input$wcfile
        if (! is.null(text_file)){
        wc_text <- readLines(text_file$datapath, n=2000)## put n=2000 for only read 2000 lines as some file may be too big, shiny app is unable to load it.
        }
        else
        {
          wc_text <- "Text mining methods allow us to highlight the most frequently
          used keywords in a paragraph of texts. One can create a word cloud, also 
          referred as text cloud or tag cloud, which is a visual representation of text data."
        }
        # create corpus
        Mycorpus <- Corpus(VectorSource(wc_text))
        # clean text: putting text to lower case
        Mycorpus_clean <- tm_map(Mycorpus,tolower)
        
        # clean text: remove all punctuation
        Mycorpus_clean <- tm_map(Mycorpus_clean, removePunctuation)
        
        # clean text: removing numbers
        Mycorpus_clean <- tm_map(Mycorpus_clean, removeNumbers)
        
        # clean text: removing stopwords
        Mycorpus_clean <- tm_map(Mycorpus_clean,removeWords,stopwords('english'))
        
        # clean text: define remove function
        remove <- function(x)
          {gsub('http[[:alnum:]]*', '',x)
          gsub('\\b+RT', '', x) ## Remove RT
          gsub('#\\S+', '', x) ## Remove Hashtags
          gsub('@\\S+', '', x) ## Remove Mentions
          gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
          gsub("\\d", '', x) ## Remove Controls and special characters
        }
        Mycorpus_clean <- tm_map(Mycorpus_clean, content_transformer(remove))
        #add stopwords
        #stopwords are words which do not contain much significance.
        #These words are usually filtered out because they return vast amount of unnecessary information.
        mystopwords <- c(stopwords("english"),"rt","íí","get","like","just","yes","know","will","good","day","people","twitter","iphonert","androidrt")
        
        #remove stopwords
        Mycorpus_clean <- tm_map(Mycorpus_clean,removeWords,mystopwords)
        
        
        # clean text: remove white spaces
        Mycorpus_clean <- tm_map(Mycorpus_clean,stripWhitespace)
        
        # steming
        Mycorpus_clean <- tm_map(Mycorpus_clean,stemDocument)
        # need to convert to structured data: term document matrix
        tdm <- TermDocumentMatrix(Mycorpus_clean, control = list(stemming=TRUE))
        #find all total number of terms(this will take several minutes due to the large documents)
        
        w <- rowSums(as.matrix(tdm))
        #calculate word frequencies
        freqs <- sort(w,decreasing = TRUE)
        
        
      })
        
      })
    })
  wordcloud_rep <- repeatable(wordcloud)
  
  output$wcplot <- renderPlot({
    withProgress({
      setProgress(message = "Creating wordcloud...")
    Mycorpus <- text_data()
    wordcloud(names(Mycorpus), Mycorpus, scale =c(5,0.4),
             min.freq = input$freq,max.words =input$max,
              colors = brewer.pal(8,"Dark2"),random.order = F, rot.per = .30)
     
      
    })
  })
      
    
})
    
  