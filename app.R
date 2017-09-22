#
# This is a Shiny web application. This app takes and input text and creates a textGraph.
# It also does the sentiment analysis of the text
# 
#

# Loading libraries
library(shiny)
library(plyr)
library(tm)
library(stringr)
library(visNetwork)
library(igraph)
library(RSentiment)
library(shinyBS)
library(ngram)
library(sqldf)

# Define UI for application that draws a textGraph
ui <- fluidPage(
   
   # Application title
   titlePanel("Text Graph and Sentiment Analysis"),
   
   # Sidebar with a text input and slider input 
   sidebarLayout(
      sidebarPanel(
        textInput("text_in","Input Paragraph"),
        actionButton("submittext","Analyze Text"),
        br(),
         sliderInput("topnodes",
                     "Percent of Top Nodes",
                     min = 1,
                     max = 100,
                     value = 100),
        wellPanel("This app takes an input text and creates a textGraph. It also provides the sentiment of the the text. It takes a paragraph in the above text box and generates a text graph. Text graphs can be used to find the context of the text and analyse the links among different words. The graph is created by considering each words in the paragraphs as nodes and edges represent the sequential occurance between two words. The nodes size depends on the degree of a node. Thus bigger node size means high frequency of a particuler word. The graph is clustered using louvain method and different clusters are given different colors. Different clusters may represent different sub context. Hover your mouse on each of the objects to know more. Try to limit your paragraph size to 500 words to experience fast response."),
        bsTooltip(id = "text_in", title = "Enter you input paragraph here."),
        bsTooltip(id = "submittext", title = "Click this button to create the text graph and perform sentiment analysis"),
        bsTooltip(id = "topnodes", title = "Set this slider to select the percent of top nodes (with respect to node degree) to show in the graph")
      ),
      
      # Show a graph of the text
      mainPanel(
        visNetworkOutput("vismodel"),
        h3("Sentiment of the Text"),
        wellPanel(textOutput("sentimentout")),
        bsTooltip(id = "vismodel",title="View the Text Graph here. You click on a node or select a node from dropdown to highlight it. You can drag and reorganize the graph."),
        bsTooltip(id = "sentimentout",title="View the sentiment of the text")
      )
   )
)

# Define server logic required to draw a textgraph and do sentiment analysis
server <- function(input, output) {
 
# Getting english stopwords
   stopWords <- stopwords("en")
  
# generating VisNetwork output
  output$vismodel <- renderVisNetwork({
    input$submittext
    isolate(text <- input$text_in)
    if (text=="")
      return()

    # Cleansing the text
     mod_text<-gsub('[[:punct:]]', ' ', text)
     mod_text <- tolower(mod_text)
     mod_text<-gsub("\\s+", " ", str_trim(mod_text))
     mod_text <-  unlist(strsplit(mod_text, " "))
     mod_text <- mod_text[!(mod_text %in% stopWords)]
     mod_text <-mod_text[mod_text!=""]
     
    # Generating Bigrams
     bigram <- get.ngrams(ngram(paste(mod_text, collapse = " "), n = 2L))
     bigram_frame=read.table(text =bigram,sep=" ")
     colnames(bigram_frame)=c("from","to")
     
    # getting node list for textGraph 
     nodes<- data.frame(id=c(as.character(bigram_frame$from),as.character(bigram_frame$to)))
     nodes<-sqldf("select id,count(id) value from nodes group by id")
     nodes$title=nodes$id
     nodes<- nodes[order(-nodes$value),]
     
    # Calculating number of top nodes with respect to node degree
     topn<-input$topnodes*nrow(nodes)/100
     nodes<-nodes[1:topn,]
     
    # generating network clusters
     ig = graph_from_data_frame(bigram_frame, directed=F)
     clusters<- cluster_louvain(ig)
     cluster=data.frame(id=names(membership(clusters)),group=clusters$membership)
     nodes=merge(nodes,cluster)
     
    # Plotting Text Graph
     visNetwork(nodes,bigram_frame) %>%  visOptions(highlightNearest = TRUE, selectedBy = "group",nodesIdSelection = TRUE) %>%
       visInteraction(navigationButtons = TRUE)
   })
  
  ######### Generating Sentiment
  output$sentimentout <- renderText({
    input$submittext
    isolate(text <- input$text_in)
    if (text=="")
      return()
    sentiment<-calculate_sentiment(text)
    as.character(sentiment$sentiment)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

