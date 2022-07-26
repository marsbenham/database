#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

#' 
library(shiny)
library(shinyjs)
library(shinythemes)
library(markdown)
library(bslib)
library(mongolite)
library(DT)
library(methods)
library(shinydashboard)
library(pool)
library(uuid)
library(dplyr)
library(DBI)
library(tidyverse)
library(rsconnect)

rsconnect::setAccountInfo(name='mariahnbenham',
                          token='E561997986E438961865BF2AA4451D54',
                          secret='yLle0t2VVQoD4/46vKP4U9LXDtdoXFYbqJ/u1bc7')

# Define UI for application that draws a histogram



ui <- fluidPage(
  theme = bs_theme(bootswatch="lux"),
  navbarPage("TOP 1000 IMDB MOVIES",
             tabPanel("IMDB Data Table",
                  
                    h5("IMDB Movie Explorer", align="center"),
                    h6("Mariah Benham, Database Applications DSCI-D532, Summer 2022", align="center"),
                    br(),
                    fluidRow(
                    actionButton("add_button", "Add", icon("plus")),
                    actionButton("delete_button", "Delete", icon("trash-alt")),
                    ),
                    fluidRow(
                     DT::dataTableOutput("imdb_table", width = "100%")
                            ),
                    h6("Data sourced from Kaggle user Harshit Shankhdar- IMDB Movies Dataset", align='center')
             ),
             tabPanel("Explore Movies by Genre", 
                      sidebarLayout(
                        sidebarPanel(
                      selectInput('genre',
                                                     "Select a genre:",
                                                     c('Action', 'Adventure', 'Animation', 'Biography','Comedy',
                                                       'Crime', 'Drama', 'Family', 'Fantasy', 'Film-Noir',
                                                       'Horror', 'Mystery', 'Thriller', 'Western'))),
                      mainPanel(
                        DT::dataTableOutput('genre', width='100%')
                      )
             )),
             tabPanel("Top 50 IMDB Movies by Rank",
                      fluidRow(
                        h3("Explore the Top 50 IMDB Movies!"),
                        h5("Ranks, titles, and brief overviews are provided. For more information about each film please see the IMDB Data Table tab."),
                        h6("IMDB Rank is determined by the number of votes from IMDB Voters. Rank is not determined by critic scores or ratings.")),
                      fluidRow(
                      DT::dataTableOutput('top_50'))
             ),
             tabPanel("Bottom 50 IMDB Movies by Rank",
                        fluidRow(
                          h3("Explore the Bottom 50 IMDB Movies!"),
                          h5("Ranks, titles, and brief overviews are provided. For more information about each film please see the IMDB Data Table tab."),
                          h6("IMDB Rank is determined by the number of votes from IMDB Voters. Rank is not determined by critic scores or ratings.")),
                        fluidRow(
                          DT::dataTableOutput('bottom_50'))
                        )
            
                        
             )
)



server <- function(input, output, session) {
  connection = 'mongodb+srv://mnbenham:MoopMoop212@dsci-590.elo0a.mongodb.net/test'
  imdb_top_1000 = mongo(collection="films", db="IMDB_Top_1000", url=connection, verbose=TRUE)
  
  documents <- imdb_top_1000$find(query='{}',sort='{"IMDB_Rank":1}')
  links <- imdb_top_1000$find(fields='{"Poster_Link":1, "_id":0}')
  documents$Poster_Link <- paste0('<img src="', documents$Poster_Link,'"</a>')
  

  
    output$imdb_table = DT::renderDataTable(
     (documents),
     server=FALSE,
      editable=TRUE,
     rownames= FALSE, escape=FALSE, options = list(searchHighlight=TRUE, autoWidth= TRUE, columnDefs= list(list(className= 'c', class='cell-border-stripe', width='200', targets= '_all')), scrollX= TRUE)
    )
  
    proxyTable = dataTableProxy(outputId = 'imdb_table')
    values <- reactiveValues(documents = documents)
    
    #ADD
    user_table <-
      documents %>% 
      slice(1) %>% 
      # transpose the first row of test into two columns
      gather(key = "column_name", value = "value") %>%
      # replace all values with ""
      mutate(value = "") %>%
      # reshape the data from long to wide
      spread(column_name, value) %>%
      # rearrange the column order to match that of test
      select(colnames(documents))
    
    observeEvent(input$add_button, {
      proxyTable %>% addRow(user_table)
    })
    
    #DEL
    
   observeEvent(input$delete_button, {
     if (!is.null(input$imdb_table_rows_selected)) {
       
       values$documents <- values$documents[-as.numeric(input$imdb_table_rows_selected),]
     }
     output$imdb_table <- renderDataTable(
       values$documents,
       server=FALSE, rownames=FALSE, escape=FALSE, options = list(searchHighlight=TRUE, autoWidth= TRUE, columnDefs= list(list(className= 'c', width='200', targets= '_all')), scrollX= TRUE)
   )
   
   
   })
   
   #genre input table
   genre_films = imdb_top_1000$find(query='{}', sort='{"IMDB_Rank":1}', fields='{ "genre":1,"Poster_Link":1, "Series_Title":1, "Overview":1,"_id":0}')
   genre_films$Poster_Link <- paste0('<img src="', genre_films$Poster_Link,'"</a>')
   
   
   
   output$genre <- DT::renderDataTable(
     genre_films %>%
       filter(genre_films$genre==input$genre),
   server=FALSE,
   editable=FALSE,
   rownames= FALSE, escape=FALSE, options = list(searchHighlight=TRUE, autoWidth= TRUE, columnDefs= list(list(className= 'c', class='cell-border-stripe', width='200', targets= '_all')), scrollX= TRUE)
   )
     
   
   #top 50 table
   
   top_films = imdb_top_1000$find('{"IMDB_Rank":{"$lte":50}}', sort='{"IMDB_Rank":1}', fields='{"Poster_Link":1, "Series_Title":1, "Overview":1, "_id":0}')
   top_films$Poster_Link <- paste0('<img src="', top_films$Poster_Link,'"</a>')
   
   output$top_50 = DT::renderDataTable(
     (top_films),
     server=FALSE,
     editable=FALSE,
     rownames= FALSE, escape=FALSE, options = list(searchHighlight=TRUE, autoWidth= TRUE, columnDefs= list(list(className= 'c', class='cell-border-stripe', width='200', targets= '_all')), scrollX= TRUE)
   )

   
   #bottom 50 table
   
   bottom_films = imdb_top_1000$find('{"IMDB_Rank":{"$gte":951}}', sort='{"IMDB_Rank":1}', fields='{"Poster_Link":1, "Series_Title":1, "Overview":1, "_id":0}')
   bottom_films$Poster_Link <- paste0('<img src="', bottom_films$Poster_Link,'"</a>')
   
   output$bottom_50 = DT::renderDataTable(
     (bottom_films),
     server=FALSE,
     editable=FALSE,
     rownames= FALSE, escape=FALSE, options = list(searchHighlight=TRUE, autoWidth= TRUE, columnDefs= list(list(className= 'c', class='cell-border-stripe', width='200', targets= '_all')), scrollX= TRUE)
   )
   
   
}


# Run the application 
shinyApp(ui = ui, server = server)

