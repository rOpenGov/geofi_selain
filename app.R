#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(ggplot2)
library(janitor)
library(leaflet)
# devtools::install_github("ropengov/geofi")
library(geofi)
library(sf)
library(shiny)
library(shinycssloaders)
library(svglite)
library(ggrepel)
library(extrafont)
loadfonts(quiet = TRUE)

options(shiny.sanitize.errors = FALSE)

api_content <- readRDS("./data/api_content.RDS")
apis <- readRDS("./data/apis.RDS")

orglist <- apis$provider
names(orglist) <- paste0(apis$provider," [", apis$api_url,"]")

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("geofi-selain v.0.1.2"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         selectInput("value_org",
                     "Valitse datan lähde:",
                     choices = orglist),
      uiOutput("ui_layer_1"),
      uiOutput("ui_layer_2"),
      uiOutput("customize_plot"),
      downloadButton("download_data", "Lataa aineisto"),
      br(),
      radioButtons("file_type", "",
                   choiceNames = list("GeoPackage (.gpkg)", "Shapefile (.shp)",
                                       "Vektorikuva (.svg)", "Vektorikuva (.pdf)",
                                      "Bittimappikuva (.png)"),
                   # NOTE: the first value is ".zip", not ".shp", as shapefile
                   # must be zipped. Shiny can only output one file and each
                   # shapefile constitutes of several files.
                   choiceValues = list(".gpkg", ".zip", ".svg", ".pdf", ".png"),
                   inline = FALSE),
      tags$hr(),
      tags$p("Tällä sovelluksella voit selata eri ",tags$code("geofi"),"-R-paketin tarjoamia datoja ja toiminnallisuuksia. Voit tallentaa aineistoja", 
             tags$code("GeoPackage, Shapefile, .svg, .pdf tai .png"),"-muotoihin sekä aggregoida kuntatason datoja ylemmille aluejaoille."),
      tags$p("Sovellus on tehty R:n",
             tags$a(href = "https://shiny.rstudio.com/", "Shiny")), 
      tags$a(href = "https://gitlab.com/muuankarski/geofi_selain", "Lähdekoodi Gitlab:ssa"),
      tags$p("Markus Kainu & Joona Lehtomäki 2020"),
      tags$hr(),
      tags$p("Tutustu myös uuteen", tags$a(href = "https://github.com/rOpenGov/geofi", tags$code("geofi")), "-pakettiin R:lle")
      ),


      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Esikatselu",
                   withSpinner(plotOutput("plot_shape", width = "100%", height = "850px"))),
          # tabPanel("Esikatselu",
          #          withSpinner(tableOutput("temp")))
          tabPanel("R-koodi kartan piirtämiseen", verbatimTextOutput("output_code"))
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$ui_layer_1 <- renderUI({

     layers <- api_content[api_content$provider %in% input$value_org, ]$title
     # layers <- api_content[api_content$provider %in% "Turku", ]$title

     tagList(
       selectInput(inputId = "value_layer",
                   label = "Valitse pohja-aineisto:",
                   choices = layers,
                   selected = layers[16])
     )
   })
   
   output$ui_layer_2 <- renderUI({
      
      # AGGREGOINTI!
      if (grepl("paavo", input$value_layer, ignore.case = TRUE)){
         
      } else {
         layer_row <- api_content[api_content$provider %in% input$value_org & api_content$title %in% input$value_layer,]
         
         tmp <- get(paste0("municipality_key_",sub("^.+_", "", layer_row$name)))
         nms <- names(tmp)
         
         tagList(
            selectInput(inputId = "value_aggregation",
                        label = "Valitse aggregoitava alueluokitus:",
                        choices = nms,
                        selected = "mk_name")
         )
      }

   })

   data_input <- reactive({

      req(input$value_org)
      req(input$value_layer)
      
      layer_row <- api_content[api_content$provider %in% input$value_org & api_content$title %in% input$value_layer,]
      if (grepl("kunta", layer_row$name)){
         shape <- geofi::get_municipalities(year = sub("^.+_", "", layer_row$name), scale = sub("^.+kunta", "",  sub("k_.+$", "", layer_row$name)))
      } else {
         shape <- geofi::get_zipcodes(year = sub("^.+_", "", layer_row$name))
      }
      
      return(shape)

   })
   
   
   data_aggregate <- reactive({
      
      req(input$value_org)
      req(input$value_layer)
      req(input$value_aggregation)
      
      if (grepl("paavo", input$value_layer, ignore.case = TRUE)){
         shape2 <- data_input()
      } else {
      # } else if (input$value_aggregation %in% c("kunta","kunta_name","name_fi","name_sv")){
      # shape2 <- data_input()
      # } else {
         shape <- data_input()
         shape$aggvar <- shape[[input$value_aggregation]]
         shape2 <- shape %>% 
            dplyr::group_by(aggvar) %>% 
            dplyr::summarise()
      }
      return(shape2)
      
   })
   
   output$customize_plot <- renderUI({
      
      # AGGREGOINTI!
      if (grepl("paavo", input$value_layer, ignore.case = TRUE)){
         
      } else {
         tagList(
            checkboxGroupInput("value_customize", 
                               "Muokkaa karttaa", 
                               selected = NA,
                               choiceNames =
                                  list("Lisää nimet",
                                       "Lisää täyttö",
                                       "Poista tausta"),
                               choiceValues =
                                  list("value_labels",
                                       "value_fill",
                                       "value_background")
            )
         )
      }
      
   })
   
   
   
   plot_ggplot <- reactive({
      
      shape <- data_aggregate()
      
      if (grepl("paavo", input$value_layer, ignore.case = TRUE)){
         plot <- ggplot(shape) +
            geom_sf(fill = NA) +
            theme_minimal()
      } else {
         plot <- ggplot(shape) +
            theme_minimal(base_family = "PT Sans") +
            geom_sf(fill = NA)
         if ("value_fill" %in% input$value_customize){
            plot <- plot + geom_sf(aes(fill = aggvar)) + labs(fill = input$value_aggregation)
         }
         if ("value_labels" %in% input$value_customize){
            plot <- plot + geom_text_repel(data = shape %>% 
                                              sf::st_set_geometry(NULL) %>% 
                                              bind_cols(shape %>% st_centroid() %>% st_coordinates() %>% as_tibble()),
                                           aes(label = aggvar, x = X, y = Y), family = "PT Sans")   
         }
         if ("value_background" %in% input$value_customize){
            plot <- plot + theme(axis.text = element_blank(),
                                 axis.title = element_blank(),
                                 panel.grid = element_blank())
         }
      }
      plot
   })
   
   
   output$plot_shape <- renderPlot({
      plot_ggplot()
   })
   

   output$download_data <- downloadHandler(

      filename = function() {
         dataset_name <- janitor::make_clean_names(input$value_layer,
                                                   case = "snake") %>%
            paste0(input$file_type)
         return(dataset_name)
      },

      content = function(file) {
         # Reactives are cached
         dat = data_aggregate()
         # This is slightly redundant but the only way to name the shapefile
         # compoments inside the zip file
         dataset_name <- janitor::make_clean_names(input$value_layer,
                                                   case = "snake") %>%
            paste0(input$file_type)

         if (input$file_type == ".zip") {
            # Create a temp folder for shp files
            temp_shp <- tempdir()
            # Write shp files
            sf::st_write(dat, dsn = temp_shp, layer = dataset_name,
                         driver = "ESRI Shapefile",
                         delete_dsn = TRUE)
            # Zip all the shp files
            zip_file <- file.path(temp_shp, "temp_shp.zip")
            shp_files <- list.files(temp_shp,
                                    dataset_name,
                                    full.names = TRUE)
            # The following zip method works in Linux
            zip_command <- paste("zip -j",
                                 zip_file,
                                 paste(shp_files, collapse = " "))
            system(zip_command)
            # Copy the zip file to the file argument
            file.copy(zip_file, file)
            # Remove all the files created
            file.remove(zip_file, shp_files)
         } else if (input$file_type == ".gpkg") {
            # Write GeoPacakge
            sf::st_write(dat, dsn = file, layer = dataset_name,
                         delete_dsn = TRUE, driver = "GPKG")
         } else if (input$file_type == ".svg") {
            # Write SVG using ggplot2
            p1 <- plot_ggplot()
            ggsave(file, plot = p1, device = "svg")
         } else if (input$file_type == ".png") {
            # Write SVG using ggplot2
            p1 <- plot_ggplot()
            ggsave(file, plot = p1, device = "png")
         } else if (input$file_type == ".pdf") {
            # Write SVG using ggplot2
            p1 <- plot_ggplot()
            ggsave(file, plot = p1, device = cairo_pdf)
         }
      }
   )


   
   # output$temp <- renderTable({
   #    shape <- data_aggregate()
   #    st_drop_geometry(shape)
   # })
   
   
   output$output_code <- renderText({
      
      
      layer_row <- api_content[api_content$provider %in% input$value_org & api_content$title %in% input$value_layer,]
      
      
#       glue::glue('
# library(ows4R)
# wfs <- WFSClient$new("{layer_row$api_url}",
#                     serviceVersion = "{layer_row$api_ver}",
#                     logger = "INFO")
# 
# caps <- wfs$getCapabilities()
# ft <- caps$findFeatureTypeByName("{layer_row$name}", exact = TRUE)
# shape <- ft$getFeatures()')
      
      
      if (grepl("kunta", layer_row$name)){
         code <- glue::glue('
# devtools::install_github("ropengov/geofi")
library(geofi)
library(dplyr)
library(ggplot2)
shape <- geofi::get_municipalities(year = {sub("^.+_", "", layer_row$name)}, scale = {sub("^.+kunta", "",  sub("k_.+$", "", layer_row$name))})
shape2 <- shape %>% 
    group_by({input$value_aggregation}) %>% 
    summarise()
ggplot(shape2) + 
    geom_sf(fill = NA) +
    theme_minimal()
')
      } else {
         code <- glue::glue('
# devtools::install_github("ropengov/geofi")
library(geofi)
library(dplyr)
library(ggplot2)
shape <- geofi::get_zipcodes(year = {sub("^.+_", "", layer_row$name)})
ggplot(shape) + 
    geom_sf(fill = NA) +
    theme_minimal()
')
      }
   })

   
   data_input <- reactive({
      
      req(input$value_org)
      req(input$value_layer)
      
      layer_row <- api_content[api_content$provider %in% input$value_org & api_content$title %in% input$value_layer,]
      if (grepl("kunta", layer_row$name)){
         shape <- geofi::get_municipalities(year = sub("^.+_", "", layer_row$name), scale = sub("^.+kunta", "",  sub("k_.+$", "", layer_row$name)))
      } else {
         shape <- geofi::get_zipcodes(year = sub("^.+_", "", layer_row$name))
      }
      
      return(shape)
      
   })
   
   
}

# Run the application
shinyApp(ui = ui, server = server)

