#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
# new branch layout
#
#    http://shiny.rstudio.com/

library(shiny)
library(bslib)
library(sf)
library(leaflet)
library(DT)
library(ggplot2)
library(ggradar)
library(tidyquant)
library(tools)
library(rgdal)
library(RColorBrewer)



create_radar_plot <- function(data, title_data){
  p <- data %>% 
    ggradar(group.colours = palette_light() %>% unname(),
            group.point.size = 0,
            group.line.width = 1,
            fill = F,
            fill.alpha = 0.25,
            plot.extent.x.sf = 1.,
            plot.extent.y.sf = 1.2,
            plot.legend = TRUE,
            legend.position = "bottom",
            axis.label.size = 4,
            values.radar = c(0, 0.5, 1)
    ) 
return(p)
  
}

###############################################################################
# Data import 
###############################################################################
#set working
wd <- "."

#import nk_shape_or including the attributes PLR_ID, PLR_NAME, TOT_POP, resilience_score
nk_shape_or <- sf::st_read(paste0(wd,"/data_clean/01_nk_shape_pop_or_norm_epsg25833_v03.gpkg"))
nk_shape_soc <- sf::st_read(paste0(wd,"/data_clean/02_nk_shape_pop_soc_epsg25833_v03.gpkg"))
nk_shape_ec <- sf::st_read(paste0(wd,"/data_clean/03_nk_shape_pop_ec_epsg25833_v03.gpkg"))
nk_shape_infr <- sf::st_read(paste0(wd,"/data_clean/04_nk_shape_pop_infr_epsg25833_v03.gpkg"))
nk_shape_env <- sf::st_read(paste0(wd,"/data_clean/05_nk_shape_pop_env_epsg25833_v03.gpkg"))
nk_shape_comcap <- sf::st_read(paste0(wd,"/data_clean/06_nk_shape_pop_comcap_epsg25833_v03.gpkg"))
# current view
nk_shape_view <- nk_shape_or_reproj <- st_transform(nk_shape_or, crs = 4326)


# Load CSV data
table_data_or <- read.csv(paste0(wd,"/data_clean/01_overall_resilience_subindices_scores_v03.csv"))
table_data_soc <- read.csv(paste0(wd,"/data_clean/02_social_resilience_indicator_scores_v03.csv"))
table_data_ec <- read.csv(paste0(wd,"/data_clean/03_economic_resilience_indicator_scores_v03.csv"))
table_data_infr <- read.csv(paste0(wd,"/data_clean/04_infr_resilience_indicator_scores_v03.csv"))
table_data_env <- read.csv(paste0(wd,"/data_clean/05_env_resilience_indicator_scores_v03.csv"))
table_data_comcap <- read.csv(paste0(wd,"/data_clean/06_comcap_resilience_indicator_scores_v03.csv"))
# current view
table_data_view <- table_data_or

# global list for data 
data_list_or <- list(navPanel = "Overall Resilience", data_shape = nk_shape_or,data_table = table_data_or, res_col = "norm_resilience_score", breaks = c(0, 0.234297, 0.393456, 0.483183, 0.658092, 1))
data_list_soc <- list(navPanel = "Social", data_shape = nk_shape_soc, data_table = table_data_soc, res_col = "normalized_social_score", breaks = c(0, 0.093037, 0.208912, 0.321620, 0.422897, 1))
data_list_ec <- list(navPanel = "Economic", data_shape = nk_shape_ec, data_table = table_data_ec, res_col = "normalized_economic_score", breaks = c(0, 0.400860, 0.470117, 0.529646, 0.819804, 1))
data_list_infr <- list(navPanel = "Infrastructure & Housing", data_shape = nk_shape_infr, data_table = table_data_infr, res_col = "normalized_infr_score", breaks = c(0, 0.285654, 0.364884, 0.484086, 0.634636, 1))
data_list_env <- list(navPanel = "Environmental", data_shape = nk_shape_env, data_table = table_data_env, res_col = "normalized_environ_score", breaks = c(0, 0.395600, 0.447662, 0.569268, 0.660944, 1))
data_list_comcap <- list(navPanel = "Community Capital", data_shape = nk_shape_comcap, data_table = table_data_comcap, res_col = "normalized_com_capital_score", breaks = c(0, 0.242026, 0.361361, 0.410412, 0.564521, 1))


# global variable used to store all PLRs selected by user
col_pal <- c("#f2f1ec", "#bbbbb3", "#717a7d", "#2f455d", "#0e1e31")
breaks <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
selected_plrs <- c()
palette_view <- colorBin(col_pal, domain = nk_shape_or$norm_resilience_score, bins = breaks)
score_viewed <- nk_shape_or$resilience_score
score_viewed_norm  <- nk_shape_or$norm_resilience_score
last_button_click <- 0
last_view_sel <- "Overall Resilience"



###############################################################################
# Define UI for application 
###############################################################################
ui <- fluidPage(
      
    #--THEME--------------------------------------------------------------------
    # customize a prototype of the app with bslib::bs_theme_preview(theme)
    #theme = bslib::bs_theme(bootswatch = "flatly"),
    # or your own theme: 
    
    #lightmode theme
    theme = bslib::bs_theme(
      bg = "#FFF", #color string for the background
      fg = "#101010", # color string for the foreground
      primary = "#E69F00", # colour for hyperlinks, active selection, contrast with theme base colour
      secondary = "#0072B2", #for components and messages that don't need to stand out
      success = "#009E73", #color for messages that indicate an operation has succeeded
      heading_font = font_google("Montserrat", wght = "700"), #typeface for heading elements
      base_font = font_google("Montserrat"), #default typeface
      code_font = font_google("JetBrains Mono"), #typeface used for code
      bootswatch = "bootstrap", # to remove all previous bootswatches??
      "navbar-bg" = "#fbf9f4"
    ),
    
    # #darkmode theme
    # theme = bslib::bs_theme(
    #   bg = "#222222", #color string for the background rgb(34,34,34) #222222
    #   fg = "#ffffff", # color string for the foreground rgb(255,255,255)#ffffff
    #   primary = "#375A7F", # colour for hyperlinks, active selection, contrast with theme base colour #375A7F
    #   secondary = "#568BA0", #for components and messages that don't need to stand out #568BA0
    #   success = "#2D2D2D", #color for messages that indicate an operation has succeeded #2D2D2D
    #   heading_font = font_google("Montserrat", wght = "700"), #typeface for heading elements
    #   base_font = font_google("Montserrat"), #default typeface
    #   code_font = font_google("JetBrains Mono"), #typeface used for code
    #   bootswatch = "bootstrap", # to remove all previous bootswatches??
    #   "navbar-bg" = "#2d2d2d"
    # ),
    
    #--APPLICATION TITLE--------------------------------------------------------
    # Application title
    titlePanel("A Neighborhood Resilience Index for Public Health Emergencies 
               in Berlin Neukölln"),
    
    #--TABSETS-------------------------------------------------------------------- 
    #set panels to the navigation bar
    page_navbar(
      title = "Subindices",
      id = "navbar",
      nav_panel("Overall Resilience"),
      nav_panel("Social"),
      nav_panel("Economic"),
      nav_panel("Infrastructure & Housing"),
      nav_panel("Environmental"),
      nav_panel("Community Capital")
      #nav_spacer(),
      #nav_panel("Indicatorlist")
    ),
    
    #--MAIN PANEL-----------------------------------------------------------------
        # Show a plot of the generated distribution
        mainPanel(
          width = 12,
          (strong("Tool description")),
          p("The Neighborhood Resilience Index for Public Health Emergencies describes the health-related resilience capacities for the neighborhoods across Neukölln and
          can be used as an initial baseline to compare neighborhoods to one another and to determine specific factors of resilience."),

          p(strong("Navigator bar:"),"The index comprises six sub-indices that can be explored by clicking on the corresponding tabs in the navigator bar at the top."),

          p(strong("Map:"),"Hovering over the map reveals planning unit names, while clicking on a planning unit displays additional information in a popup.
            The selected planning unit is filtered in the table and added to the radar chart. Multiple planning units can be selected.
            Clicking again on a specific planning unit removes it from the radar chart. To remove all selected planning units, click << Reset >>"),

          p(strong("Radar Chart:"),"Displays the distribution of normalized resilience capacity values from the selected planning units on the map."),

          p(strong("Table"),"Presents the resilience capacity values for the Overall Resilience score and its corresponding sub-indices. Please note that the Overall Resilience score is absolute,
            ranging from 0 to 5, while the sub-indices are normalized values ranging from 0 to 1."),

          p(em("Please note that this is a prototype. Another tab with the indicator descriptions and references will be added in the future")),
          
           # first row showing the leaflet ProviderTiles+nk_shape_or & radarplot
           fluidRow(
             # add the map on the left side 
             column(6,
                    leafletOutput("map", height = "600px", width = "100%"),
                    style='padding:15px'
           ),
             # add the radar plot on the right side
             column(6,
                    plotOutput("radar_plot", width = "100%", height = "550px"),
                    style='padding:15px'
             ),
            ),
          
          # second row  including the Reset Button
          fluidRow(
            # 
            column(1,
                   actionButton("reset_button", "Reset", height="50px"),
                   style='padding:15px'
            )
          ),
          
          # third row showing the data table 
          fluidRow(
            column(12,
                   DT::dataTableOutput("table"),
                   style='padding:15px'
            )
          )
          
        )
      )




###############################################################################
# # Define server logic 
###############################################################################
#c("#f2f1ec, #bbbbb3, #717a7d, #2f455d, #0e1e31")

server <- function(input, output, session) {
  
  #apply theme to the plots (runs with ggplot2, lattice and base plots)
  #thematic::thematic_shiny()

  #--MAP-------------------------------------------------------   
  # add map to the board
    output$map <- renderLeaflet({
    
    # Reproject nk_shape_or to WGS84
    nk_shape_or_reproj <- st_transform(nk_shape_or, crs = 4326)
    
    #define colours and breakpoints for nk_shape_or
    col_pal <- c("#f2f1ec", "#bbbbb3", "#717a7d", "#2f455d", "#0e1e31")
    breaks <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
    
    palette <- colorBin(col_pal, domain = nk_shape_or_reproj$norm_resilience_score, bins = breaks)
    
    #providerTiles: 
    #Stadia.AlidadeSmoothDark for darkmode
    #CartoDB.Positron for lightmode
    
    leaflet() %>% 
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(minZoom=12, maxZoom=12)) %>%
      setView(lat = 52.441362, lng = 13.445232, zoom = 12) %>%
      addPolygons(data = nk_shape_or_reproj,
                  color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 1,
                  fillColor = ~palette(norm_resilience_score),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  layerId = ~PLR_ID,
                  label = ~paste("", PLR_NAME),
                  popup = ~paste("PLR Name: ", PLR_NAME, "<br>",
                                 "Total Population: ", TOT_POP,"<br>",
                                 "Resilience Score:", round(resilience_score,2))) %>%
      leaflet::addLegend(colors=col_pal,  position = "topright", opacity = 1., labels = c("Low", "", "Medium", "", "High"))
    })

    #--add reactivity----------------------------------------------------
    
    observe({

      if (input$navbar == "Social"){
        table_data_view <<- data_list_soc$data_table
        nk_shape_view <<- st_transform(data_list_soc$data_shape, crs = 4326)
        palette_view <<- colorBin(col_pal, domain = data_list_soc$data_shape$normalized_social_score, bins = data_list_soc$breaks)
        score_viewed <<- nk_shape_view$social_score
        score_viewed_norm <<- nk_shape_view$normalized_social_score
        
      } else if (input$navbar == "Economic") {
        table_data_view <<- data_list_ec$data_table
        nk_shape_view <<- st_transform(data_list_ec$data_shape, crs = 4326)
        palette_view <<- colorBin(col_pal, domain = data_list_ec$data_shape$normalized_economic_score, bins = data_list_ec$breaks)
        score_viewed <<- nk_shape_view$economic_score
        score_viewed_norm <<- nk_shape_view$normalized_economic_score
        
      } else if (input$navbar == "Infrastructure & Housing") {
        table_data_view <<- data_list_infr$data_table
        nk_shape_view <<- st_transform(data_list_infr$data_shape, crs = 4326)
        palette_view <<- colorBin(col_pal, domain = data_list_infr$data_shape$normalized_infr_score, bins = data_list_infr$breaks)
        score_viewed <<- nk_shape_view$infr_score
        score_viewed_norm <<- nk_shape_view$normalized_infr_score
        
      } else if (input$navbar == "Environmental") {
        table_data_view <<- data_list_env$data_table
        nk_shape_view <<- st_transform(data_list_env$data_shape, crs = 4326)
        palette_view <<- colorBin(col_pal, domain = data_list_env$data_shape$normalized_environ_score, bins = data_list_env$breaks)
        score_viewed <<- nk_shape_view$environ_score
        score_viewed_norm <<- nk_shape_view$normalized_environ_score
        
      } else if (input$navbar == "Community Capital") {
        table_data_view <<- data_list_comcap$data_table
        nk_shape_view <<- st_transform(data_list_comcap$data_shape, crs = 4326)
        palette_view <<- colorBin(col_pal, domain = data_list_comcap$data_shape$normalized_com_capital_score, bins = data_list_comcap$breaks)
        score_viewed <<- nk_shape_view$com_capital_score
        score_viewed_norm <<- nk_shape_view$normalized_com_capital_score
        
      } else if (input$navbar == "Overall Resilience"){
        table_data_view <<- data_list_or$data_table
        nk_shape_view <<- st_transform(data_list_or$data_shape, crs = 4326)
        palette_view <<- colorBin(col_pal, domain = data_list_or$data_shape$norm_resilience_score, bins = data_list_or$breaks)
        score_viewed <<- nk_shape_view$resilience_score
        score_viewed_norm <<- nk_shape_view$norm_resilience_score

      }
      if (input$navbar != last_view_sel) {
      leafletProxy("map", data = data_list_or$data_shape) %>%
        clearShapes() %>%
        addPolygons(data = nk_shape_view,
                    color = "#444444", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 1,
                    fillColor = ~palette_view(score_viewed_norm),
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE),
                    layerId = ~PLR_ID,
                    label = ~paste("", PLR_NAME),
                    popup = ~paste("PLR Name: ", PLR_NAME, "<br>",
                                   "Total Population: ", TOT_POP,"<br>",
                                   "Resilience Score:", round(score_viewed,2)))
      }
      # update leaflet map

    
      #capture the event when a shape on a leaflet map is clicked
      event <- input$map_shape_click
      
      #-------------------------
      #define reactive expression that filters the table data by the clicked polygone
      # id from the event
      ################################# DONT TOUCH THIS #################################
      selected_data <- reactive({
        if (input$reset_button > last_button_click) {
          # reset selected plrs
          selected_plrs <<- c()
          # update last button click
          last_button_click <<- input$reset_button
          table_data_view
        } else if (last_view_sel == input$navbar) {
          if (is.null(event$id) ) {
            # default view no plr selected, show the full table
            table_data_view
          } else {
            # get the plr id of the clicked polygone
            curr_selected_plr <- as.numeric(event$id)
            # check if current plr is already selected
            #   if (curr_selected_plr %in% selected_plrs && last_view != input$navbar) {
              if (curr_selected_plr %in% selected_plrs) {
                  # remove from selected plrs
                  selected_plrs <<- selected_plrs[!selected_plrs %in% curr_selected_plr]
              } else {
                  # add to selected plrs
                  selected_plrs <<- c(selected_plrs, curr_selected_plr)
              }
            # filter table data by all selected plrs
           }
          }
        if (length(selected_plrs) == 0) {
          table_data_view
        } else {
          table_data_view[table_data_view$PLR_ID %in% selected_plrs, ]
        }
      })


      #-------------------------
      #render table with filtered data of the clicked polygon
      output$table <- renderDT({
        datatable(selected_data())
      })

      #-------------------------
      #render radarplot with filtered data of the clicked polygon
      ra_data <- selected_data()

      last_view_sel <<- input$navbar
      # only render radar plot if any plr is selected
      if (length(selected_plrs) > 0){
        output$radar_plot <- renderPlot({
          # select sub indices and drop overall ressilence column
          selected_columns <- ra_data[ , 2:ncol(ra_data)][, -2]
          ggradar_output <- create_radar_plot(selected_columns, ra_data)
          print(ggradar_output)

        })
      } else {
        # render empty plot
        plt_data <- data.frame(group = c(""), var1 = c(0), var2 = c(0), var3 = c(0), var4 = c(0), var5 = c(0))
        output$radar_plot <- renderPlot({
          ggradar(plt_data, axis.labels = c("", "", "", "", ""), group.line.width = 0, group.point.size = 0,
                  group.colours = c("white"),legend.position = "bottom", plot.legend = TRUE)
          
        })
        }
    })
}  
  

# Run the application 
shinyApp(ui = ui, server = server)
