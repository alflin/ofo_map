
#libraries
library(shiny)
library(shinyjs)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(RSQLite)

# library("RSQLite")
# sqlite    <- dbDriver("SQLite")
# exampledb <- dbConnect(sqlite,"/Users/alfredlin/Desktop/ofomap/database_empty.db")
# data <- data.frame(lat = c(0), lng = c(0), company = c(0), uuid = c(0))
# dbWriteTable(exampledb,"bikes",data)
#setup sqlite http://faculty.washington.edu/kenrice/sisg-adv/exampleSQLite.R

#sqlitePath <- "/Users/alfredlin/Desktop/ofomap/map.db"
sqlitePath <- "/srv/shiny-server/map/map.db"
table <- "bikes"

loadData <- function() {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the fetching query
  query <- sprintf("SELECT * FROM %s", table)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

saveData <- function(data) {
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table,
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  # Submit the update query and disconnect
  print(paste('saving----',query))
  dbGetQuery(db, query)
  dbDisconnect(db)
}

deleteData <- function(uuid){
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  # Construct the fetching query
  query <- sprintf("DELETE FROM %s WHERE uuid = %s;", table, uuid)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
}

giveuuid <- function(){
  as.character(as.numeric(Sys.time())*100)
}

makeDelButton <- function(uuid){
  paste('<button onclick="myFunction(', uuid, ')">Delete</button>',sep="")
}

# Define UI for application that draws a histogram
ui <- bootstrapPage(
  useShinyjs(),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),

  # javascript code to send data to shiny server
  tags$script('function myFunction(layerId) {
              Shiny.onInputChange("lay", layerId);
              console.log(layerId);
              }'),
  
  # Application title
  leafletOutput("mymap", width = '100%', height = '100%'),
  absolutePanel(
    bottom = 5, left = 10,
    wellPanel(radioButtons('addMarks', 'Add Inputs', choices = c('None', 'ofo', 'mobike', 'obike'), selected = 'None'))
  )
)

server <- function(input, output, session) {

  output$mymap <- renderLeaflet({

    data <- loadData()
    data$uuid <- as.character(data$uuid)
    data <- data %>% mutate(popup = makeDelButton(uuid) )
    
    ofo <- data %>% filter(company == 1)
    mobike <- data %>% filter(company == 2)
    obike <- data %>% filter(company == 3)

    #add icons
    ofoicons <- iconfunc('ofo')
    mobikeicons <- iconfunc('mobike')
    obikeicons <- iconfunc('obike')

    m <- leaflet() %>%
       addTiles(group = 'default') %>%  # Add default OpenStreetMap map tiles
       addProviderTiles('CartoDB.Positron', group = 'simple')%>%
    
      #adding the markers
       addAwesomeMarkers(data = ofo, lat = ~lat, lng = ~lng,
                         icon = ofoicons, popup = ~popup,
                         group = 'ofo', layerId = ~uuid) %>%
       addAwesomeMarkers(data = mobike, lat = ~lat, lng = ~lng,
                         icon = mobikeicons, popup = ~popup,
                         group = 'mobike', layerId = ~uuid) %>%
       addAwesomeMarkers(data = obike, lat = ~lat, lng = ~lng,
                         icon = obikeicons, popup = ~popup,
                         group = 'obike', layerId = ~uuid) %>%

       #set view and add layer control
       setView(lng = '103.8198', lat = '1.3521', 12) %>%
       addLayersControl(
         baseGroups = c("default", "simple"),
         overlayGroups = c('ofo', 'mobike', 'obike'),
         options = layersControlOptions(collapsed = FALSE)
       ) %>%
       addSearchOSM() 
      
    # %>% removeMarker(input$lay)
      
      # %>% addMarkers(lat = 1.3521, lng = 103.8197,
      #            popup = '<button onclick="myFunction(5)">delete</button>' , layerId = '5' )
    
     m
     
   })


  iconfunc <- function(company){

    if(company == 'ofo'){
      icons <- awesomeIcons(
        icon = 'ios-close',
        iconColor = 'black',
        library = 'ion',
        markerColor = 'orange'
      )
    }
    else if (company == 'mobike'){
      icons <- awesomeIcons(
        icon = 'ios-close',
        iconColor = 'black',
        library = 'ion',
        markerColor = 'red'
      )
    }
    else if (company == 'obike'){
      icons <- awesomeIcons(
        icon = 'ios-close',
        iconColor = 'black',
        library = 'ion',
        markerColor = 'purple'
      )
    }
    icons
  }

  #drawing points
  observeEvent(input$mymap_click, {
    if(input$addMarks != 'None'){

      proxy <- leafletProxy('mymap')
      icons <- iconfunc(input$addMarks)
      uuidtemp <- giveuuid()
      uuidtemp <- as.numeric(substring(uuidtemp,5,nchar(uuidtemp))) * 1000
      
      proxy %>% addAwesomeMarkers(lat=unlist(input$mymap_click)[1],
                                  lng=unlist(input$mymap_click)[2],
                                  icon = icons,
                                  group = input$addMarks,
                                  layerId = uuidtemp
                                  # ,popup = makeDelButton(uuidtemp)
                                  )
      newdata <- data.frame( lat = c(unlist(input$mymap_click)[1]),
                           lng = c(unlist(input$mymap_click)[2]),
                           company =
                             switch(input$addMarks, "ofo" = 1, "mobike" = 2, "obike" = 3),
                           uuid = c(uuidtemp)
                          )

    saveData(newdata)

    #use this for RDS
    # cache <- readRDS("/Users/alfredlin/Desktop/ofomap/data.rds")
    # cache <- rbind(cache, newdata)
    # rownames(cache) <- NULL
    # saveRDS(cache, "/Users/alfredlin/Desktop/ofomap/data.rds")
    }
    
  })

  ##authentication-------------------------------------
  # Return the UI for a modal dialog with data selection input. If 'failed' is
  # TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(
      passwordInput("login_name", "Login Name Please",
                placeholder = 'login name here'
      ),
      span('login name here'),
      br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
      size = 'l',
      if (failed)
        div(tags$b("Invalid username", style = "color: red;")),

      footer = tagList(
        actionButton("ok", "OK")
      )
    )
  }

  #blocking screen
  observeEvent(input$ok, {
    #passwords etc
    if (input$login_name == 'uberonyuan') {
      removeModal()
    }
    else if (input$login_name == 'user'){
      shinyjs::disable(id = 'addMarks', selector = "[type=radio]")
      removeModal()
    }
    else if (input$login_name == 'ofoman'){
      removeModal()
    }
    else {
      showModal(dataModal(failed = TRUE))
    }
  })
  
  
  observeEvent(input$lay,{

    if (input$login_name == 'uberonyuan') {

      print(paste('removing----',input$lay))
      
      #remove from map
      leafletProxy("mymap") %>%
        removeMarker(input$lay)
      
      #remove from database
      deleteData(input$lay)     
    }
    
  })
  
  #show blocking screen
  showModal(dataModal())

}

# Run the application
shinyApp(ui = ui, server = server)


#other notes:
# addMarkers(lat = test$lat, lng = test$lng, layerId = test$name) %>%
# addMarkers(lat = 1.35, lng = 103.8198, layerId = 'haha', group = 'no') %>%  
# need to call this to get 3rd call, 4th argument
# m$x
# $calls[[3]]$args[[4]]
# [1] "haha"

