library(rsconnect)
library(curl)
library(rstudioapi)

library(dplyr)
library(caret)
library(DT)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(shinythemes)
library(tidyr)
library(wordcloud)
library(fresh)
library(tidytext)
library(tigris)
library(sp)
#library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)
library(fastDummies)


#getwd()
##### reading in data + combining into one file #####
abb_2020 <- read.csv("listing2020.csv",header=T)
abb_2021 <- read.csv("listing2021.csv",header=T)
glimpse(abb_2020)
glimpse(abb_2021)

#manually add a column(year) in 2021
abb_2020<-mutate(abb_2020, year_m='2020')
glimpse(abb_2020)

#manually add a column(year) in 2021
abb_2021<-mutate(abb_2021, year_m='2021')
glimpse(abb_2021)

#combine the datasets 2020,2021
AB_All<-rbind(abb_2020,abb_2021)
AB_All

#select the columns we need
AB_All<-select(AB_All,c("id","name","host_id","neighborhood_overview","host_name","neighbourhood_group_cleansed","neighbourhood_cleansed",
                        "latitude","longitude","room_type","price","longitude","room_type","reviews_per_month",
                        "minimum_nights","number_of_reviews","calculated_host_listings_count","availability_365","year_m","description",
                        "review_scores_accuracy","review_scores_cleanliness","review_scores_checkin",
                        "review_scores_communication","review_scores_location","review_scores_value",
                        "amenities","room_type","property_type","accommodates","listing_url"))


##### cleaning data (removing outliers, replacing missing values, etc.) #####
summary(AB_All)

#cleaning name
AB_All <- AB_All %>%
  filter(AB_All$name != "")
str(AB_All)

#cleaning host id
AB_All <- AB_All[!is.na(AB_All$host_id), ] #removing the rows that had NA values for host ID
summary(AB_All)

#cleaning host name
AB_All <- AB_All %>%
  filter(AB_All$host_name != "")

#cleaning neighborhood group cleansed
table(AB_All$neighbourhood_group_cleansed) #cleaning not needed

#cleaning neighborhood cleansed
table(AB_All$neighbourhood_cleansed) #cleaning not needed

#cleaning neighborhood_overview
#create a dataframe for wordcloud
AB_All_new<-select(AB_All,c("host_id","neighbourhood_group_cleansed","neighborhood_overview"))
AB_All_new <-AB_All_new%>%
  unnest_tokens(output=word, input=neighborhood_overview)%>%
  anti_join(stop_words)

#manually add stop_words
custom_stop_words <- bind_rows(data_frame(word = c("br","brooklyn","neighborhood","walk","central",
                                                   "square","street","located","manhattan","williamsburg",
                                                   "york","west","east","walking","village","close",
                                                   "located","distance","block","minutes","building","harlem",
                                                   "bushwick","minute","times","apartment","upper","avenue",
                                                   "nyc","midtown","city","location","district","queens",
                                                   "heights","hudson","mi","neighborhoods","local","bed","house",
                                                   "bedford","chelsea","st","corner","nearby","hill","min",
                                                   "short","access","lots","stuy","ave","including","kitchen",
                                                   "community","slope","columbia","plenty","stuyvesant","ny","clinton",
                                                   "major","time","stop","washington","brownstones","feel","public",
                                                   "tons","bronx","barclays","south","north","lines","buildings",
                                                   "plenty","day","lincoln","options","caribbean","ride","columbus","steps",
                                                   "offer","day","brownstone","offers","views","brooklyn's","mins","apollo",
                                                   "eateries","door","tribeca","filled","hour","coming","circle","bam","filled",
                                                   "blocks"), 
                                          lexicon = c("custom")), 
                               stop_words)
AB_All_new<-AB_All_new %>%
  anti_join(custom_stop_words)

#clean the "word" column
AB_All_new <- AB_All_new[!(AB_All_new$word==""), ]    #remove all the blank
anyNA(AB_All_new$word)

AB_All_new <- AB_All_new %>%     #delete the numeric words
  filter(!(substr(word,start=1,stop=1)%in%c(0:9)))

AB_All_new<-AB_All_new %>%  #create a count column 
  count(neighbourhood_group_cleansed, word)

#cleaning longitude and latitude
summary(AB_All) #no missing values 

#cleaning room type
table(AB_All$room_type) #cleaning not needed

#cleaning price
AB_All$price<-sub(".", "", AB_All$price)
AB_All$price<- as.numeric(AB_All$price)
AB_All$price
str(AB_All)
#boxplot(AB_All$price, data = AB_All, main="Price")
#hist(AB_All$price)

#cleaning reviews per month
str(AB_All)
AB_All$reviews_per_month <- ifelse(is.na(AB_All$reviews_per_month), 0, AB_All$reviews_per_month) #setting NA values equal to zero reviews

#cleaning minimum nights
AB_All <- AB_All %>%
  filter(AB_All$minimum_nights <= 365) #deleting anything greater than 1 year

#cleaning number of reviews
#hist(AB_All$number_of_reviews) #skewed but we are not deleting large values

#cleaning calculated_host_listings_count
summary(AB_All)
str(AB_All) #no missing values, no cleaning needed

#cleaning review_scores
AB_All$review_scores_accuracy <- ifelse(AB_All$review_scores_accuracy == 0, NA, AB_All$review_scores_accuracy)
AB_All$review_scores_checkin <- ifelse(AB_All$review_scores_checkin == 0, NA, AB_All$review_scores_checkin)
AB_All$review_scores_cleanliness <- ifelse(AB_All$review_scores_cleanliness == 0, NA, AB_All$review_scores_cleanliness)
AB_All$review_scores_communication <- ifelse(AB_All$review_scores_communication == 0, NA, AB_All$review_scores_communication)
AB_All$review_scores_location <- ifelse(AB_All$review_scores_location == 0, NA, AB_All$review_scores_location)
AB_All$review_scores_value <- ifelse(AB_All$review_scores_value == 0, NA, AB_All$review_scores_value)

#cleaning accommodates
AB_All <- AB_All %>%
  filter(AB_All$accommodates != 0)

#creating dummy variables for amenities
#important amenities to include, according to airbnb:
#wifi, workspace, pet friendly, kitchens, TV, air conditioning, 
#heating, breakfast, hair dryer, self check-in, parking, extras and essentials
#https://www.airbnb.com/resources/hosting-homes/a/the-amenities-guests-want-25

library(stringr)

AB_All$wifi <- ifelse(grepl("wifi", AB_All$amenities), 1, 0)
AB_All$wifi <- ifelse(grepl("Wifi", AB_All$amenities), 1, AB_All$wifi)
sum(AB_All$wifi)

AB_All$workspace <- ifelse(grepl("workspace", AB_All$amenities), 1, 0)
AB_All$workspace <- ifelse(grepl("Workspace", AB_All$amenities), 1, AB_All$workspace)
AB_All$workspace <- ifelse(grepl("work space", AB_All$amenities), 1, AB_All$workspace)
AB_All$workspace <- ifelse(grepl("Work space", AB_All$amenities), 1, AB_All$workspace)
sum(AB_All$workspace)

#AB_All$petfriendly <- ifelse(grepl("pet", AB_All$amenities), 1, 0)
#AB_All$petfriendly <- ifelse(grepl("Pet", AB_All$amenities), 1, AB_All$petfriendly)
#sum(AB_All$petfriendly)

AB_All$kitchen <- ifelse(grepl("kitchen", AB_All$amenities), 1, 0)
AB_All$kitchen <- ifelse(grepl("Kitchen", AB_All$amenities), 1, AB_All$kitchen)
sum(AB_All$kitchen)

AB_All$TV <- ifelse(grepl("TV", AB_All$amenities), 1, 0)
AB_All$TV <- ifelse(grepl("Tv", AB_All$amenities), 1, AB_All$TV)
AB_All$TV <- ifelse(grepl("tv", AB_All$amenities), 1, AB_All$TV)
sum(AB_All$TV)

AB_All$airconditioning <- ifelse(grepl("air", AB_All$amenities), 1, 0)
AB_All$airconditioning <- ifelse(grepl("Air", AB_All$amenities), 1, AB_All$airconditioning)
sum(AB_All$airconditioning)

AB_All$heating <- ifelse(grepl("Heating", AB_All$amenities), 1, 0)
AB_All$heating <- ifelse(grepl("heating", AB_All$amenities), 1, AB_All$heating)
sum(AB_All$heating)

AB_All$breakfast <- ifelse(grepl("Breakfast", AB_All$amenities), 1, 0)
AB_All$breakfast <- ifelse(grepl("breakfast", AB_All$amenities), 1, AB_All$breakfast)
sum(AB_All$breakfast)

#AB_All$hairdryer <- ifelse(grepl("hair", AB_All$amenities), 1, 0)
#AB_All$hairdryer <- ifelse(grepl("Hair", AB_All$amenities), 1, AB_All$hairdryer)
#sum(AB_All$hairdryer)

#AB_All$selfcheckin <- ifelse(grepl("self", AB_All$amenities), 1, 0)
#AB_All$selfcheckin <- ifelse(grepl("Self", AB_All$amenities), 1, AB_All$selfcheckin)
#sum(AB_All$selfcheckin)

AB_All$parking <- ifelse(grepl("parking", AB_All$amenities), 1, 0)
AB_All$parking <- ifelse(grepl("Parking", AB_All$amenities), 1, AB_All$parking)
sum(AB_All$parking)

AB_All$essentials <- ifelse(grepl("Essentials", AB_All$amenities), 1, 0)
AB_All$essentials <- ifelse(grepl("essentials", AB_All$amenities), 1, AB_All$essentials)
sum(AB_All$essentials)

AB_All$extras <- ifelse(grepl("extra", AB_All$amenities), 1, 0)
AB_All$extras <- ifelse(grepl("Extra", AB_All$amenities), 1, AB_All$extras)
sum(AB_All$extras)


# 
##### EDA and more cleaning for prediction #####

#checking for multicollinearity
library(gplots)
colfunc <- colorRampPalette(c("green", "white", "red"))
heatmap.2(cor(Filter(is.numeric,AB_All), use = "complete.obs"), Rowv = FALSE, 
          Colv = FALSE, dendrogram = "none", lwid=c(.1,5), lhei=c(.1,5), 
          col = colfunc(15), 
          cellnote = round(cor(Filter(is.numeric,AB_All), use ="complete.obs"),2),
          notecol = "black", key = FALSE, trace = 'none')

# all of the rating scores are highly correlated. We will just use one (value rating score) in our prediction. 

hist(AB_All$number_of_reviews)

neighborhoodgrp <- table(AB_All$neighbourhood_group_cleansed)
barplot(neighborhoodgrp)

accom <- table(AB_All$accommodates)
barplot(accom)

rmtype <- table(AB_All$room_type)
barplot(rmtype)


sapply(AB_All, function(x) sum(is.na(x)))

#deleting rows that are missing price
AB_All <- AB_All[!is.na(AB_All$price), ]

hist(AB_All$review_scores_value)
#imputing mean score for NAs in review scores_value
AB_All$review_scores_value[which(is.na(AB_All$review_scores_value))] = mean(AB_All$review_scores_value , na.rm = TRUE)
sapply(AB_All, function(x) sum(is.na(x)))

#setting binary amenities equal to factor variables
str(AB_All)

AB_All$wifi = as.factor(AB_All$wifi)
AB_All$workspace = as.factor(AB_All$workspace)
AB_All$kitchen = as.factor(AB_All$kitchen)
AB_All$TV = as.factor(AB_All$TV)
AB_All$airconditioning = as.factor(AB_All$airconditioning)
AB_All$heating = as.factor(AB_All$heating)
AB_All$breakfast = as.factor(AB_All$breakfast)
AB_All$parking = as.factor(AB_All$parking)
AB_All$essentials = as.factor(AB_All$essentials)
AB_All$extras = as.factor(AB_All$extras)

#making dummies for neighborhood group

AB_pred <- AB_All
AB_pred <- fastDummies::dummy_cols(AB_pred,
                      select_columns = "neighbourhood_group_cleansed",
                      remove_first_dummy = FALSE,
                      remove_selected_columns=FALSE)

#making dummies for room_type
AB_pred <- fastDummies::dummy_cols(AB_pred,
                      select_columns = "room_type",
                      remove_first_dummy = FALSE,
                      remove_selected_columns=FALSE)
#kept original dummy cols because they are used to filter by input 

names(AB_pred)
names(AB_pred)[45] <- "hotelRoom"
names(AB_pred)[46] <- "privateRoom"
names(AB_pred)[47] <- "sharedRoom"
names(AB_pred)



#standardizing min-max for number of reviews
AB_pred$number_of_reviews <- (AB_pred$number_of_reviews - min(AB_pred$number_of_reviews)) / (max(AB_pred$number_of_reviews) - min(AB_pred$number_of_reviews))

head(AB_pred,5)

#standardizing min-max review_scores_value
AB_pred$review_scores_value <- (AB_pred$review_scores_value - min(AB_pred$review_scores_value)) / (max(AB_pred$review_scores_value) - min(AB_pred$review_scores_value))



##### Prediction Prep #####
names(AB_pred)
cols <- c('price', 'number_of_reviews', 'wifi', 'neighbourhood_group_cleansed_Brooklyn', 'neighbourhood_group_cleansed_Manhattan','neighbourhood_group_cleansed_Queens','neighbourhood_group_cleansed_Bronx', 'hotelRoom', 'privateRoom', 'sharedRoom','workspace','kitchen', 'TV', 'airconditioning', 'heating', 'breakfast', 'parking', 'essentials', 'extras', 'review_scores_value','accommodates')

data2 <- AB_pred %>% select(cols)

head(data2)
set.seed(42)

splitting <- createDataPartition(y = data2$price,  
                                 p = .8,   
                                 list = F)

train <- data2[splitting,]  # training data set
test <- data2[-splitting,]  # test data set


lm_model = lm(formula = price ~ .,data = train)
summary(lm_model)


y_lm_train = predict(lm_model, train)

##### ui #####

library(shiny)

ui <- fluidPage(

  #####input####  
  # Set Shiny Theme
  theme = shinytheme("superhero"),
  
  # Application title
  titlePanel("New York City Airbnb Guest Guide", windowTitle = "Airbnb Data forGuests"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "neighbourhood", 
                  label = "Select Neighborhood Group(s)",
                  choices = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"),
                  multiple = TRUE),
      numericInput("accommodates",
                  "Set Number of Guests",
                  value = 1,
                  min = 1,
                  max = 10,
                  step = 1
                  ),
      selectInput(inputId = "room_types_input", 
                  label = "Select Room Type(s)",
                  choices = c("Entire home/apt",
                              "Private room",
                              "Shared room",
                              "Hotel room"),
                  multiple= TRUE),
    strong(p("Select Desired Amenities", style = "font-size:12.5px")),
    checkboxInput(inputId = "wifi",
                label = "Wifi", FALSE),
    checkboxInput(inputId = "workspace",
                  label = "Workspace", FALSE),
    checkboxInput(inputId = "kitchen",
                  label = "Kitchen", FALSE),
    checkboxInput(inputId = "TV",
                  label = "TV", FALSE),
    checkboxInput(inputId = "airconditioning",
                  label = "Air Conditioning", FALSE),
    checkboxInput(inputId = "heating",
                  label = "Heating", FALSE),
    checkboxInput(inputId = "breakfast",
                  label = "Breakfast", FALSE),
    checkboxInput(inputId = "parking",
                  label = "Parking", FALSE),
    checkboxInput(inputId = "essentials",
                  label = "Essentials", FALSE),
    checkboxInput(inputId = "extras",
                  label = "Extras", FALSE),
    actionButton(inputId = "viewlistings", 
                 label = "View Listings >",
                 style="color: #fff; background-color: #FF5A5F")
    ),
    
  #####output####
    # Output of Shiny app
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("About",
                           br(),
                           h3("Don’t know how to find the best listings on Airbnb for your stay?"),
                           br(),
                           h3("Let us help you!"),
                           p("Our app helps those who want to find the ideal listings in New York City on Airbnb to gain a general understanding of the listing numbers, price, and to further formulate the best filter combination."),
                           p("In addition, we provide you with useful listing suggestions based on your traveling companions and purposes. Moreover, we even have a convenient table that directly takes you to the listing websites and to have a closer look into the listing details that fit all your needs!")
                           ),
                  tabPanel("Map of Listings",
                           h3("Explore New York City Airbnb Listings"),
                           p(textOutput("count")),
                           p("Click a point to view information about that listing."),
                           br(),
                           leafletOutput(outputId = "map"),
                           br()),
                  tabPanel("Price Info",
                           h3("Predicted Price"),
                           p(textOutput("priceestimate")),
                           br(),
                           h3("Average Price by Neighborhood Based on Filters"),
                           p("Constrained by budget? Based on your needs, look for the location that suits you the most!"),
                           br(),
                           leafletOutput("pricebyn"),
                           br()),
                  tabPanel("Listing Scores",
                           h3("Listing Scores vs. Price"),
                           p("Learn about the listing review scores based on the neighborhood you choose."),
                           br(),
                           h4("1. Price vs. Accuracy Score"),
                           br(),
                           plotOutput("accuracy_vs_price"),
                           br(),
                           h4("2. Price vs. Cleanliness Score"),
                           br(),
                           plotOutput("clean_vs_price"),
                           br(),
                           h4("3. Price vs. Check-in Score"),
                           br(),
                           plotOutput("checkin_vs_price"),
                           br(),
                           h4("4. Price vs. Communication Score"),
                           br(),
                           plotOutput("communication_vs_price"),
                           br(),
                           h4("5. Price vs. Location Score"),
                           br(),
                           plotOutput("location_vs_price"),
                           br(),
                           h4("6. Price vs. Value Score"),
                           br(),
                           plotOutput("value_vs_price"),
                           br()),                  
                  tabPanel("Suggestions and Neighborhood Info",
                           h3("Get some suggestions and helpful information about neighborhoods!"),
                           p("What is the primary purpose of your stay?"),
                           selectInput(inputId = "staytype", 
                                       label = "Select Stay Type",
                                       choices = c("Business",
                                                   "Friends",
                                                   "Family",
                                                   "Alone")),
                           uiOutput("amenitiesrec"),
                           br(),
                           h4("Number of Listings by Neighborhood"),
                           uiOutput("plots"),
                           br(),
                           h4("Neighborhood Overviews"),
                           uiOutput("wordcloudoverview"),
                           br()
                           ),
                  tabPanel("Listings Table",
                           h3("Listings Table"),
                           p("Explore listings based on your filter in the table below."),
                           DT::dataTableOutput("prices"),
                           br()),
                  tabPanel("Acknowledgements",
                           br(),
                           br(),
                           p("Our app referenced code from another shiny app called Airbnb PriceR: Airbnb Analytics. We have redesigned the input panel to add additional filtering options including multiple selection, redesigned the descriptive analysis, used data for NYC, added one more tab to show the price by map, and provided insightful suggestions in our app, among other changes."),
                           p("App Link: https://geraldlee.shinyapps.io/airbnb_priceR/"),
                           p("Code Link: https://github.com/gl2668/airbnb_priceR"),
                           br(),
                           br(),
                           p("The ‘Map by Price’ of our app referenced some code from RPubs-NYC Maps(Aurthor:Jake Hofman)."),
                           p("Code Link: https://rpubs.com/jhofman/nycmaps"),
                           br(),
                           br(),
                           p("The dynamic plot code used in the’ Suggestions and Neighborhood Info’ referenced some code from Github (Author:wch)."),
                           p("Code Link: https://gist.github.com/wch/5436415/"),
                           br(),
                           br(),
                           p("The datasets used in our app are from Inside Airbnb."),
                           p("Link: http://insideairbnb.com/get-the-data.html"),
                           br(),
                           br(),
                           p("We based our amenities options on our data and this Airbnb article."),
                           p("Link: https://www.airbnb.com/resources/hosting-homes/a/the-amenities-guests-want-25"))
                  
                  
      )
    )
  )
)

##### server #####
server <- function(input, output) {
  
  ##### Data Table Output #####
    data_1 <- reactive({if (input$workspace == TRUE){
      subset(AB_All, workspace == 1)} else{
        AB_All
      }
    })
    data_2 <- reactive({if (input$wifi == TRUE){
      subset(data_1(), wifi == 1)} else{
        data_1()
      }
    })
    data_3 <- reactive({if (input$kitchen == TRUE){
      subset(data_2(), kitchen == 1)} else{
        data_2()
      }
    })
    data_4 <- reactive({if (input$TV == TRUE){
      subset(data_3(), TV == 1)} else{
        data_3()
      }
    })
    data_5 <- reactive({if (input$airconditioning == TRUE){
      subset(data_4(), airconditioning == 1)} else{
        data_4()
      }
    })
    data_6 <- reactive({if (input$heating == TRUE){
      subset(data_5(), heating == 1)} else{
        data_5()
      }
    })
    data_7 <- reactive({if (input$breakfast == TRUE){
      subset(data_6(), breakfast == 1)} else{
        data_6()
      }
    })
    data_8 <- reactive({if (input$parking == TRUE){
      subset(data_7(), parking == 1)} else{
        data_7()
      }
    })
    data_9 <- reactive({if (input$essentials == TRUE){
      subset(data_8(), essentials == 1)} else{
        data_8()
      }
    })
    data_10 <- reactive({if (input$extras == TRUE){
      subset(data_9(), extras == 1)} else{
        data_9()
      }
    })
    output$prices <- DT::renderDataTable({
      listings_table <-eventReactive(input$viewlistings, ({data_10() %>% 
          dplyr:: filter(neighbourhood_group_cleansed %in% input$neighbourhood,
                        accommodates %in% input$accommodates,
                        room_type %in% input$room_types_input,
                        year_m == 2021) %>%
          select("Name" = name,
               "Price ($)" = price,
               "Neighborhood" = neighbourhood_cleansed,
               "Property Type" = property_type,
               "Accuracy Score" = review_scores_accuracy,
               "Cleanliness Score" = review_scores_cleanliness,
               "Check-in Score" = review_scores_checkin,
               "Communication Score" = review_scores_communication,
               "Location Score" = review_scores_location,
               "Value Score" = review_scores_value,
               "Link" = listing_url) %>% 
          mutate(Link = paste0("<a href='", Link,"' target='_blank' style=\"color:#FF5A5F;\">", "Link to Listing","</a>")) 
      }))
    DT::datatable(listings_table(),
                  rownames = FALSE,
                  options = list(dom = 't',
                                 pageLength = 8, scrollY = '300px', paging = FALSE, scrollX = TRUE,
                                 columnDefs = list(list(className = 'dt-center', targets = '_all', class = 'hover', stripe = FALSE)),
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'color': '#fff'});",
                                   "}")),
                  escape = FALSE)
})
  
  ##### Text Output - Count #####
  print_data <-eventReactive(input$viewlistings, ({data_10() %>% 
      dplyr:: filter(neighbourhood_group_cleansed %in% input$neighbourhood,
                     accommodates %in% input$accommodates,
                     room_type %in% input$room_types_input,
                     year_m == 2021) %>%
      nrow()}))
  output$count <- renderText({
    paste("There are", print_data(), "listings that match your filter.")
  })

 
  ##### Map #####
  
  output$map <- renderLeaflet({
    data_1 <- reactive({if (input$workspace == TRUE){
      subset(AB_All, workspace == 1)} else{
        AB_All
      }
    })
    data_2 <- reactive({if (input$wifi == TRUE){
      subset(data_1(), wifi == 1)} else{
        data_1()
      }
    })
    data_3 <- reactive({if (input$kitchen == TRUE){
      subset(data_2(), kitchen == 1)} else{
        data_2()
      }
    })
    data_4 <- reactive({if (input$TV == TRUE){
      subset(data_3(), TV == 1)} else{
        data_3()
      }
    })
    data_5 <- reactive({if (input$airconditioning == TRUE){
      subset(data_4(), airconditioning == 1)} else{
        data_4()
      }
    })
    data_6 <- reactive({if (input$heating == TRUE){
      subset(data_5(), heating == 1)} else{
        data_5()
      }
    })
    data_7 <- reactive({if (input$breakfast == TRUE){
      subset(data_6(), breakfast == 1)} else{
        data_6()
      }
    })
    data_8 <- reactive({if (input$parking == TRUE){
      subset(data_7(), parking == 1)} else{
        data_7()
      }
    })
    data_9 <- reactive({if (input$essentials == TRUE){
      subset(data_8(), essentials == 1)} else{
        data_8()
      }
    })
    data_10 <- reactive({if (input$extras == TRUE){
      subset(data_9(), extras == 1)} else{
        data_9()
      }
    })
  data_sub3 <-eventReactive(input$viewlistings, ({data_10() %>% dplyr:: filter(neighbourhood_group_cleansed %in% input$neighbourhood,
                       accommodates %in% input$accommodates,
                       room_type %in% input$room_types_input,
                       year_m == 2021) %>% 
      mutate(Link = paste0("<a href='", listing_url,"' target='_blank' style=\"color:#FF5A5F;\">", "Link to Listing","</a>")) 
}))

  leaflet(data_sub3()) %>% 
  setView(lng = -74.0060, lat = 40.7128, zoom = 10)  %>%
  addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(data = data_sub3(), 
  lat = ~ latitude, 
  lng = ~ longitude, 
  weight = 2,
  fillOpacity = 0.5,
  color = "lightcoral",
  popup = ~as.character(paste0(name, "<br>", "Price Per Night: $", price, "<br>",
  "Value Score: ", review_scores_value, "<br>",
  "Cleanliness Score: ", review_scores_cleanliness, "<br>", Link,"<br>"),
  label = ~as.character(paste0("Price: ", sep = " ", price))
  ))

  })
  ##### price vs. accuracy score scatter plot #####
  output$accuracy_vs_price <- renderPlot({
    data_sub3 <-eventReactive(input$viewlistings, ({data_10() %>% 
        dplyr:: filter(neighbourhood_group_cleansed %in% input$neighbourhood,
                       accommodates %in% input$accommodates,
                       room_type %in% input$room_types_input,
                       year_m == 2021)
    }))
    ggplot(data_sub3(), aes(x = data_sub3()$review_scores_accuracy,y = data_sub3()$price)) + 
      geom_point(size = 4, alpha = 0.5, color = "brown1") + 
      theme(panel.background = element_rect(fill="#2B3E50",colour="#2B3E50"),
            plot.background = element_rect(fill="#2B3E50",colour="#2B3E50"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "white"),
            axis.title = element_text(size=15,colour = "white"),
            axis.text.x = element_text(size = 15,colour = "white"),
            axis.text.y = element_text(size = 15,colour = "white")) +
      labs(x = "Accuracy Review Score (1-10)", y = "Price($)")
  })
  ##### price vs. cleanliness score scatter plot #####
  output$clean_vs_price <- renderPlot({
    data_sub3 <-eventReactive(input$viewlistings, ({data_10() %>% 
        dplyr:: filter(neighbourhood_group_cleansed %in% input$neighbourhood,
                       accommodates %in% input$accommodates,
                       room_type %in% input$room_types_input,
                       year_m == 2021)
    }))
    ggplot(data_sub3(), aes(x = data_sub3()$review_scores_cleanliness,y = data_sub3()$price)) + 
      geom_point(size = 4, alpha = 0.5, color = "antiquewhite4") + 
      theme(panel.background = element_rect(fill="#2B3E50",colour="#2B3E50"),
            plot.background = element_rect(fill="#2B3E50",colour="#2B3E50"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "white"),
            axis.title = element_text(size=15,colour = "white"),
            axis.text.x = element_text(size = 15,colour = "white"),
            axis.text.y = element_text(size = 15,colour = "white")) +
      labs(x = "Cleanliness Review Score (1-10)", y = "Price($)")
  })
  
  ##### price vs. checkin score scatter plot #####
  output$checkin_vs_price <- renderPlot({
    data_sub3 <-eventReactive(input$viewlistings, ({data_10() %>% 
        dplyr:: filter(neighbourhood_group_cleansed %in% input$neighbourhood,
                       accommodates %in% input$accommodates,
                       room_type %in% input$room_types_input,
                       year_m == 2021)
    }))
    ggplot(data_sub3(), aes(x = data_sub3()$review_scores_checkin,y = data_sub3()$price)) + 
      geom_point(size = 4, alpha = 0.5, color = "brown1") + 
      theme(panel.background = element_rect(fill="#2B3E50",colour="#2B3E50"),
            plot.background = element_rect(fill="#2B3E50",colour="#2B3E50"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "white"),
            axis.title = element_text(size=15,colour = "white"),
            axis.text.x = element_text(size = 15,colour = "white"),
            axis.text.y = element_text(size = 15,colour = "white")) +
      labs(x = "Check-in Review Score (1-10)", y = "Price($)")
  })
  ##### price vs. communication score scatter plot #####
  output$communication_vs_price <- renderPlot({
    data_sub3 <-eventReactive(input$viewlistings, ({data_10() %>% 
        dplyr:: filter(neighbourhood_group_cleansed %in% input$neighbourhood,
                       accommodates %in% input$accommodates,
                       room_type %in% input$room_types_input,
                       year_m == 2021)
    }))
    ggplot(data_sub3(), aes(x = data_sub3()$review_scores_communication,y = data_sub3()$price)) + 
      geom_point(size = 4, alpha = 0.5, color = "antiquewhite4") + 
      theme(panel.background = element_rect(fill="#2B3E50",colour="#2B3E50"),
            plot.background = element_rect(fill="#2B3E50",colour="#2B3E50"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "white"),
            axis.title = element_text(size=15,colour = "white"),
            axis.text.x = element_text(size = 15,colour = "white"),
            axis.text.y = element_text(size = 15,colour = "white")) +
      labs(x = "Communication Review Score (1-10)", y = "Price($)")
  })
  
  ##### price vs. location score scatter plot #####
  output$location_vs_price <- renderPlot({
    data_sub3 <-eventReactive(input$viewlistings, ({data_10() %>% 
        dplyr:: filter(neighbourhood_group_cleansed %in% input$neighbourhood,
                       accommodates %in% input$accommodates,
                       room_type %in% input$room_types_input,
                       year_m == 2021)
    }))
    ggplot(data_sub3(), aes(x = data_sub3()$review_scores_location,y = data_sub3()$price)) + 
      geom_point(size = 4, alpha = 0.5, color = "brown1") + 
      theme(panel.background = element_rect(fill="#2B3E50",colour="#2B3E50"),
            plot.background = element_rect(fill="#2B3E50",colour="#2B3E50"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "white"),
            axis.title = element_text(size=15,colour = "white"),
            axis.text.x = element_text(size = 15,colour = "white"),
            axis.text.y = element_text(size = 15,colour = "white")) +
      labs(x = "Location Review Score (1-10)", y = "Price($)")
  })
  
  ##### amenities + property_type recommendations #####
  
  #note: file must be named app.R and you must hit "Run App" not run individual lines of code
  output$amenitiesrec <- renderUI({if (input$staytype == "Business") {
    img(src = "business.png", height = "175px")} else if (input$staytype == "Alone") {
      img(src = "alone.png", height = "175px")} else if (input$staytype == "Friends") {
        img(src = "friends.png", height = "175px")} else if (input$staytype == "Family") {
          img(src = "family.png", height = "175px")}
  })
  
  ##### price vs. value score scatter plot #####
  output$value_vs_price <- renderPlot({
    data_sub3 <-eventReactive(input$viewlistings, ({data_10() %>% 
        dplyr:: filter(neighbourhood_group_cleansed %in% input$neighbourhood,
                       accommodates %in% input$accommodates,
                       room_type %in% input$room_types_input,
                       year_m == 2021)
    }))
    ggplot(data_sub3(), aes(x = data_sub3()$review_scores_value,y = data_sub3()$price)) + 
      geom_point(size = 5, alpha = 0.5, color = "antiquewhite4") + 
      theme(panel.background = element_rect(fill="#2B3E50",colour="#2B3E50"),
            plot.background = element_rect(fill="#2B3E50",colour="#2B3E50"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "white"),
            axis.title = element_text(size=15,colour = "white"),
            axis.text.x = element_text(size = 15,colour = "white"),
            axis.text.y = element_text(size = 15,colour = "white")) +
      labs(x = "Value Review Score (1-10)", y = "Price($)")
  })
  ##### Wordcloud by neighborhood ####
  
  output$wordcloudoverview <- renderUI({
    plot_output_list <- lapply(1:length(input$neighbourhood), function(i) {
      plotname1 <- paste("plot1", i, sep="")
      plotOutput(plotname1)
    })
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  list_new <-reactive({(input$neighbourhood)})
  
  for (i in 1:5) {
    local({
      my_i<-i
      plotname1 <- paste("plot1", my_i, sep="")
      output[[plotname1]]<-renderPlot({
        data_sub7 <- AB_All_new %>% dplyr::filter(neighbourhood_group_cleansed == list_new()[my_i])
        wc <- data_sub7 %>%
          group_by(word) %>%
          summarise(N = sum(n)) %>%
          arrange(desc(N))
        set.seed(1234)
        
        
        layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
        par(mar=rep(0, 4),bg="#2B3E50")
        plot.new()
        text(x=0.5, y=0.5, list_new()[my_i],cex=2.5,col = "white")
        
        suppressWarnings(
          wordcloud(words = wc$word,
                    freq = wc$N,
                    min.freq = 600,
                    max.words = 50, 
                    random.order = FALSE,
                    colors = "indianred1",
                    font = 7,
                    size = 1.6,
                    scale=c(18,0.7),
                    width=800, 
                    height=400,
                    main='Title'))
      })
    })
  }
  ##### Price by Neighborhood#####
  output$pricebyn <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(-74.00, 40.71, zoom = 12) %>%
      addProviderTiles("CartoDB.Positron")
    
    r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
    
    nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)
    
    leaflet(nyc_neighborhoods) %>%
      addTiles() %>% 
      addPolygons(popup = ~neighborhood) %>%
      addProviderTiles("CartoDB.Positron")
    
    
    points <- data.frame(lat=AB_All$latitude, lng=AB_All$longitude, 
                         price = AB_All$price, 
                         neighborhood_group = AB_All$neighbourhood_group_cleansed,
                         guest = AB_All$accommodates)
    
    points <- points %>%
      filter(is.na(price) == 0)
    
    
    points_spdf <- points
    coordinates(points_spdf) <- ~lng + lat
    proj4string(points_spdf) <- proj4string(nyc_neighborhoods)
    matches <- over(points_spdf, nyc_neighborhoods)
    points <- cbind(points, matches)
    
    price_by_neighborhood <- points %>%
      group_by(neighborhood_group,neighborhood,guest) %>%
      summarize(price = mean(price))    
    
    
    price_by_neighborhood_2 <- price_by_neighborhood %>%
      filter(neighborhood_group %in% input$neighbourhood &
               guest >= input$accommodates)
    
    
    
    map_data <- geo_join(nyc_neighborhoods, price_by_neighborhood_2, "neighborhood", "neighborhood")
    
    colorset <- c("#ffcccd","#ff8a8d","#FF5A5F","#ad3638")
    
    pal <- colorBin(palette = colorset,
                    domain = range(price_by_neighborhood_2$price),
                    bins = c(0,50,100,150,250,500,max(price_by_neighborhood_2$price)),
                    reverse = FALSE,
                    na.color = "#dbd5d5")
    
    leaflet(map_data) %>%
      addTiles() %>%       
      addPolygons(stroke = TRUE,
                  color = "#fcf2f5",
                  weight = 2,
                  fillColor = ~pal(price), 
                  popup = ~neighborhood,
                  fillOpacity = 2) %>% 
      
      addProviderTiles("CartoDB.Positron") %>%
      setView(-73.98, 40.75, zoom = 10)%>%
      addLegend('topleft', pal=pal, values=price_by_neighborhood_2$price,
                title='price', opacity=100)
  })
 
  ##### Neighborhood listing number#####
  output$plots <- renderUI({
    plot_output_list <- lapply(1:length(input$neighbourhood), function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname)
    })
    
    do.call(tagList, plot_output_list)
  })
  list_new2 <-reactive({(input$neighbourhood)})
  
  for (i in 1:5) {
    local({
      my_i<-i
      plotname <- paste("plot", my_i, sep="")
      output[[plotname]]<-renderPlot({
        data_count <-eventReactive(input$viewlistings, ({data_10() %>% 
            dplyr:: filter(neighbourhood_group_cleansed == list_new2()[my_i],
                           accommodates %in% input$accommodates,
                           room_type %in% input$room_types_input,
                           year_m == 2021) 
        }))
        if(nrow(data_count())!=0)
          ggplot(data_count(),aes(x=data_count()$neighbourhood_cleansed))+geom_bar(position = position_dodge(width = 0.9, preserve = "single"),alpha = 0.7,fill = "brown1")+coord_flip()+
          labs(x = "Neighbourhood", title = paste("Neighborhood: ",list_new2()[my_i]))+
          theme(panel.background = element_rect(fill="#2B3E50",colour="#2B3E50"),
                plot.background = element_rect(fill="#2B3E50",colour="#2B3E50"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "white"),
                title=element_text(size =15,colour = "white"),
                axis.title = element_text(size =15,colour = "white"),
                axis.text.x = element_text(size = 8,colour = "white"),
                axis.text.y = element_text(size = 8,colour = "white"))
        else
          text(x=0.5,y=0.5,paste("Find no listings in ",list_new2()[my_i],"according to the filters"),cex=1.3)
        
      })
    })
  }
  ##### Price Prediction #####
  output$priceestimate <- renderText({
    data_1_p <- reactive({if (input$workspace == TRUE){
      subset(AB_pred, workspace == 1)} else{
        AB_pred
      }
    })
    data_2_p <- reactive({if (input$wifi == TRUE){
      subset(data_1_p(), wifi == 1)} else{
        data_1_p()
      }
    })
    data_3_p <- reactive({if (input$kitchen == TRUE){
      subset(data_2_p(), kitchen == 1)} else{
        data_2_p()
      }
    })
    data_4_p <- reactive({if (input$TV == TRUE){
      subset(data_3_p(), TV == 1)} else{
        data_3_p()
      }
    })
    data_5_p <- reactive({if (input$airconditioning == TRUE){
      subset(data_4_p(), airconditioning == 1)} else{
        data_4_p()
      }
    })
    data_6_p <- reactive({if (input$heating == TRUE){
      subset(data_5_p(), heating == 1)} else{
        data_5_p()
      }
    })
    data_7_p <- reactive({if (input$breakfast == TRUE){
      subset(data_6_p(), breakfast == 1)} else{
        data_6_p()
      }
    })
    data_8_p <- reactive({if (input$parking == TRUE){
      subset(data_7_p(), parking == 1)} else{
        data_7_p()
      }
    })
    data_9_p <- reactive({if (input$essentials == TRUE){
      subset(data_8_p(), essentials == 1)} else{
        data_8_p()
      }
    })
    data_10_p <- reactive({if (input$extras == TRUE){
      subset(data_9_p(), extras == 1)} else{
        data_9_p()
      }
    })

    data_sub3 <-eventReactive(input$viewlistings, ({data_10_p() %>% 
        dplyr::filter(neighbourhood_group_cleansed %in% input$neighbourhood, room_type %in% input$room_types_input)}))
    ctrl <- trainControl("cv", number = 10)
    model <- train(price ~ accommodates, 
                          data = data_sub3(), 
                          method = "lm", 
                          trControl = ctrl,
                          preProcess = c("range"))
    
    accommodates <- input$accommodates
    test <- data.frame(accommodates)
    
    y_hat <- predict(model, newdata = test) %>% round(2)
  
    paste("Based on your selections, the listing price should be about $", y_hat,"  ", " per night.")
})
  
} 
shinyApp(ui = ui, server = server)

