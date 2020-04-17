
# loading and installing the needed packages.
pacman::p_load(jsonlite, rworldmap, tidyverse, shinycssloaders, glue)

# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=0.5)

### USER INTERFACE - UI
ui <- fluidPage(
  # Application title
  titlePanel("Twitter Sentiment Analysis"),
  
  # Sidebar 
  sidebarPanel(
                  numericInput(inputId = "num_tweets_to_download",
                               label = "Number of tweets:",
                               min = 100,
                               max = 300,
                               value = 10,
                               step = 100),
                  textInput(inputId = "hashtag_to_search",
                            label = "Hashtag to search:",
                            value = "Coronavirus"),
                  actionButton(inputId = "get_data", label = "Request tweets", class = "btn-primary")
               ),
    
  # Show results
  mainPanel(
              tabPanel("", withSpinner(plotOutput(outputId = "plotOutputMap", height="700px", width="1050px"), type = 3))
              # plotOutput("Test") %>% withSpinner(color="#0dc5c1")  
            )
)


### SERVER 
server <- function(input, output) {
  
  # Twitter sentiment analysis API URL
  API_url <- eventReactive( input$get_data,
                            {
                              glue("http://123.4.5.6:7000/GetTweetsSentiment/{input$hashtag_to_search}/{input$num_tweets_to_download}")
                            }
                           )
  
  # output$url_text <- renderText(API_url())
  
  twitter_API_Data <- reactive({
                                  req(API_url())
                                  ### Getting the data from the API and changing it to a dataframe ###
                                    API_raw_results <- httr::GET(API_url())
                                    API_content <- httr::content(API_raw_results, as= 'text')
                                    twitter_API_Data <- jsonlite::fromJSON(API_content)
                                  ### ---- ###
                                
                                  ### Using geolocation(longitude,latitude) to identify each tweet country ###
                                    # Getting the geolocation(longitude,latitude) from twitter_API_Data 
                                    twitter_API_Data_geolocation <- dplyr::select(twitter_API_Data,longitude,latitude)
                                    countries <- getMap(resolution='low')
                                    # convert our list of points to a SpatialPoints object
                                    pointsSP = SpatialPoints(twitter_API_Data_geolocation, proj4string=CRS(proj4string(countries)))
                                    # use 'over' to get indices of the Polygons object containing each point
                                    indices = over(pointsSP, countries)
                                    # Adding the countries to a new column in the data called country
                                    twitter_API_Data$country <- indices$GU_A3
                                  ### ---- ###
                                
                                  # Removing the NA observations
                                  # twitter_API_Data <- twitter_API_Data[!is.na(twitter_API_Data)]
                                
                                  # Getting the average of the positivenes_score for each country adding it to a new avgPositivenessScore column
                                  twitter_API_Data <-  twitter_API_Data %>%
                                                                group_by(country) %>%
                                                                summarise(avgPositivenessScore = mean(positiveness_score))
                              
                                  twitter_API_Data
                              })

  output$plotOutputMap <- renderPlot({  
                                        ### Creating the map ###
                                          req(twitter_API_Data())
                                          
                                          worldMap <- joinCountryData2Map(twitter_API_Data(),
                                                              nameJoinColumn = "country",
                                                              joinCode = "NAME",
                                                              nameCountryColumn = "country"
                                          )
      
                                          # Pallet used to color the map and its legent From Red to Green
                                          colourPalette <- RColorBrewer::brewer.pal(10, 'RdYlGn')
                      
                                          mapParams <- mapPolys(worldMap,
                                          nameColumnToPlot='avgPositivenessScore',
                                          catMethod = "fixedWidth",
                                          numCats=100,
                                          colourPalette = colourPalette,
                                          addLegend=TRUE,
                                          mapTitle = "Twitter Sentiment Analysis - Positiveness by Country")
                                          mtext("Negative                    -                    Neutral                    -                    Positive",side=1,line=-1)
                                        ### ---- ###
                                        
                                      })
}

# Run the application 
shinyApp(ui = ui, server = server)
