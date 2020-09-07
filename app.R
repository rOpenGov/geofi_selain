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
# library(bootstraplib)
# bs_theme_new(version = "4-3", bootswatch = NULL)
# bs_theme_add_variables(
#    "body-bg" = "#ffffff",
#    "body-color" = "f1f2f3",
#    # "font-family-base" = "Pt Sans",
#    "font-size-base" = "0.9rem",
#    "nav-tabs-link-active-color" = "#3b4044",
#    "primary" = "#4d4d00",
#    "secondary" = "#999966",
#    "well-bg" = "#f2f2f2",
#    "card-border-color" = "darken($well-bg, 3%)",
#    "card-border-radius" = 0,
#    "card-border-width" = "0.5rem"
# )

options(shiny.sanitize.errors = FALSE)

# api_content <- readRDS("./data/api_content.RDS")
api_content <- structure(list(provider = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 
                                                     1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L
), .Label = "Tilastokeskus", class = "factor"), title = c("Kunnat 2013 (1:1 000 000)", 
                                                          "Kunnat 2013 (1:4 500 000)", "Kunnat 2014 (1:1 000 000)", "Kunnat 2014 (1:4 500 000)", 
                                                          "Kunnat 2015 (1:1 000 000)", "Kunnat 2015 (1:4 500 000)", "Kunnat 2016 (1:1 000 000)", 
                                                          "Kunnat 2016 (1:4 500 000)", "Kunnat 2017 (1:1 000 000)", "Kunnat 2017 (1:4 500 000)", 
                                                          "Kunnat 2018 (1:1 000 000)", "Kunnat 2018 (1:4 500 000)", "Kunnat 2019 (1:1 000 000)", 
                                                          "Kunnat 2019 (1:4 500 000)", "Kunnat 2020 (1:1 000 000)", "Kunnat 2020 (1:4 500 000)", 
                                                          "Paavo-postinumeroalueet 2015", "Paavo-postinumeroalueet 2016", 
                                                          "Paavo-postinumeroalueet 2017", "Paavo-postinumeroalueet 2018", 
                                                          "Paavo-postinumeroalueet 2019", "Paavo-postinumeroalueet 2020"
), name = c("tilastointialueet:kunta1000k_2013", "tilastointialueet:kunta4500k_2013", 
            "tilastointialueet:kunta1000k_2014", "tilastointialueet:kunta4500k_2014", 
            "tilastointialueet:kunta1000k_2015", "tilastointialueet:kunta4500k_2015", 
            "tilastointialueet:kunta1000k_2016", "tilastointialueet:kunta4500k_2016", 
            "tilastointialueet:kunta1000k_2017", "tilastointialueet:kunta4500k_2017", 
            "tilastointialueet:kunta1000k_2018", "tilastointialueet:kunta4500k_2018", 
            "tilastointialueet:kunta1000k_2019", "tilastointialueet:kunta4500k_2019", 
            "tilastointialueet:kunta1000k_2020", "tilastointialueet:kunta4500k_2020", 
            "postialue:pno_2015", "postialue:pno_2016", "postialue:pno_2017", 
            "postialue:pno_2018", "postialue:pno_2019", "postialue:pno_2020"
), api_url = c("http://geo.stat.fi/geoserver/wfs", "http://geo.stat.fi/geoserver/wfs", 
               "http://geo.stat.fi/geoserver/wfs", "http://geo.stat.fi/geoserver/wfs", 
               "http://geo.stat.fi/geoserver/wfs", "http://geo.stat.fi/geoserver/wfs", 
               "http://geo.stat.fi/geoserver/wfs", "http://geo.stat.fi/geoserver/wfs", 
               "http://geo.stat.fi/geoserver/wfs", "http://geo.stat.fi/geoserver/wfs", 
               "http://geo.stat.fi/geoserver/wfs", "http://geo.stat.fi/geoserver/wfs", 
               "http://geo.stat.fi/geoserver/wfs", "http://geo.stat.fi/geoserver/wfs", 
               "http://geo.stat.fi/geoserver/wfs", "http://geo.stat.fi/geoserver/wfs", 
               "http://geo.stat.fi/geoserver/wfs", "http://geo.stat.fi/geoserver/wfs", 
               "http://geo.stat.fi/geoserver/wfs", "http://geo.stat.fi/geoserver/wfs", 
               "http://geo.stat.fi/geoserver/wfs", "http://geo.stat.fi/geoserver/wfs"
), api_ver = c("1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", 
               "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", 
               "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", 
               "1.0.0", "1.0.0"), layer_url = structure(c("http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=getFeature&typename=tilastointialueet:kunta1000k_2013", 
                                                          "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=getFeature&typename=tilastointialueet:kunta4500k_2013", 
                                                          "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=getFeature&typename=tilastointialueet:kunta1000k_2014", 
                                                          "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=getFeature&typename=tilastointialueet:kunta4500k_2014", 
                                                          "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=getFeature&typename=tilastointialueet:kunta1000k_2015", 
                                                          "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=getFeature&typename=tilastointialueet:kunta4500k_2015", 
                                                          "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=getFeature&typename=tilastointialueet:kunta1000k_2016", 
                                                          "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=getFeature&typename=tilastointialueet:kunta4500k_2016", 
                                                          "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=getFeature&typename=tilastointialueet:kunta1000k_2017", 
                                                          "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=getFeature&typename=tilastointialueet:kunta4500k_2017", 
                                                          "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=getFeature&typename=tilastointialueet:kunta1000k_2018", 
                                                          "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=getFeature&typename=tilastointialueet:kunta4500k_2018", 
                                                          "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=getFeature&typename=tilastointialueet:kunta1000k_2019", 
                                                          "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=getFeature&typename=tilastointialueet:kunta4500k_2019", 
                                                          "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=getFeature&typename=tilastointialueet:kunta1000k_2020", 
                                                          "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=getFeature&typename=tilastointialueet:kunta4500k_2020", 
                                                          "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=getFeature&typename=postialue:pno_2015", 
                                                          "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=getFeature&typename=postialue:pno_2016", 
                                                          "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=getFeature&typename=postialue:pno_2017", 
                                                          "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=getFeature&typename=postialue:pno_2018", 
                                                          "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=getFeature&typename=postialue:pno_2019", 
                                                          "http://geo.stat.fi/geoserver/wfs?service=WFS&version=1.0.0&request=getFeature&typename=postialue:pno_2020"
               ), class = c("glue", "character"))), class = c("tbl_df", "tbl", 
                                                              "data.frame"), row.names = c(NA, -22L))
apis <- structure(list(api_url = c("http://geo.stat.fi/geoserver/wfs", 
                                   "http://avaa.tdata.fi/geoserver/paituli/wfs", "http://geoserver.ymparisto.fi/geoserver/wfs?", 
                                   "https://kartta.hsy.fi/geoserver/wfs", "https://kartta.hel.fi/ws/geoserver/avoindata/wfs", 
                                   "http://geoserver.hel.fi/geoserver/ows", "https://kartat.espoo.fi/teklaogcweb/wfs.ashx", 
                                   "http://gis.vantaa.fi/geoserver/wfs", "http://geodata.tampere.fi/geoserver/ows", 
                                   "https://opaskartta.turku.fi/TeklaOGCWeb/WFS.ashx", "https://e-kartta.ouka.fi/TeklaOGCWeb/WFS.ashx", 
                                   "http://kartta.kuopio.fi/TeklaOgcWeb/WFS.ashx", "https://kartta.rovaniemi.fi/teklaogcweb/WFS.ashx", 
                                   "https://kartta.jkl.fi/TeklaOgcWeb/WFS.ashx", "http://lipas.cc.jyu.fi/geoserver/lipas/ows", 
                                   "http://geoserver.lounaistieto.fi/geoserver/varsinais-suomi_aluesuunnittelu/ows", 
                                   "http://geoserver.lounaistieto.fi/geoserver/koostetietovaranto/ows", 
                                   "http://kartta.hyvinkaa.fi/wfs_1/ows.ashx", "https://extranet.liikennevirasto.fi/inspirepalvelu/avoin/wfs", 
                                   "https://extranet.liikennevirasto.fi/inspirepalvelu/rajoitettu/wfs", 
                                   "https://extranet.liikennevirasto.fi/inspirepalvelu/TransportNetworks/wfs", 
                                   "http://gis2.luke.fi:8080/geoserver/kala/wfs", "http://gis2.luke.fi:8080/geoserver/riista/wfs", 
                                   "http://maps.luomus.fi/geoserver/wfs"), ver = c("1.0.0", "2.0.0", 
                                                                                   "2.0.0", "2.0.0", "1.0.0", "2.0.0", "1.0.0", "2.0.0", "2.0.0", 
                                                                                   "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", 
                                                                                   "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", "1.0.0", 
                                                                                   "1.0.0"), provider = c("Tilastokeskus", "Paituli", "Syke", "HSY", 
                                                                                                          "Helsinki", "Helsinki geoserver", "Espoo", "Vantaa", "Tampere", 
                                                                                                          "Turku", "Oulu", "Kuopio", "Rovaniemi", "Jyväskylä", "LIPAS", 
                                                                                                          "Lounaistieto: varsinais-suomi_aluesuunnittelu", "Lounaistieto: koostetietovaranto", 
                                                                                                          "Hyvinkää", "Liikennevirasto avoin", "Liikennevirasto rajoitettu", 
                                                                                                          "Liikennevirasto Transport network", "LUKE kala", "LUKE riista", 
                                                                                                          "Luomus")), row.names = c(NA, -24L), class = c("tbl_df", "tbl", 
                                                                                                                                                         "data.frame"))
apis <- apis[apis$provider == "Tilastokeskus",]
orglist <- apis$provider
names(orglist) <- paste0(apis$provider," [", apis$api_url,"]")

# Define UI for application that draws a histogram
ui <- fluidPage(
   # bootstrap(),

   # Application title
   titlePanel("geofi-selain v.0.1.4"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         tags$p("Tällä sovelluksella voit selata eri ",tags$a(href = "https://github.com/rOpenGov/geofi", tags$code("geofi")),"-R-paketin tarjoamia datoja ja toiminnallisuuksia. Voit tallentaa aineistoja", 
                tags$code("GeoPackage, Shapefile, .svg, .pdf tai .png"),"-muotoihin sekä aggregoida kuntatason datoja ylemmille aluejaoille."),
         tags$h4("1. Luo kartta"),
         selectInput("value_org",
                     "Valitse datan lähde:",
                     choices = orglist),
      uiOutput("ui_layer_1"),
      uiOutput("ui_layer_2"),
      uiOutput("customize_plot"),
      bookmarkButton(id = "bookmark1", 
                     label = "Jaa valinnat"),
      tags$br(),
      tags$hr(),
      tags$h4("2. Lataa kartta-aineisto"),
      radioButtons("file_type", "Valitse tiedostomuoto",
                   choiceNames = list("GeoPackage (.gpkg)", "Shapefile (.shp)",
                                       "Vektorikuva (.svg)", "Vektorikuva (.pdf)",
                                      "Bittimappikuva (.png)"),
                   # NOTE: the first value is ".zip", not ".shp", as shapefile
                   # must be zipped. Shiny can only output one file and each
                   # shapefile constitutes of several files.
                   choiceValues = list(".gpkg", ".zip", ".svg", ".pdf", ".png"),
                   inline = FALSE),
      downloadButton("download_data", "Lataa aineisto"),
      tags$hr(),
      tags$p("Sovellus on tehty R:n",
             tags$a(href = "https://shiny.rstudio.com/", "Shiny"), ":lla"), 
      tags$a(href = "https://gitlab.com/muuankarski/geofi_selain", "Lähdekoodi Gitlab:ssa"),
      tags$p("Markus Kainu & Joona Lehtomäki 2020")
      ),


      # Show a plot of the generated distribution
      mainPanel(
         shiny::fluidRow(column(7,
            tags$h4("Kartan esikatselu"),
            withSpinner(plotOutput("plot_shape", width = "100%", height = "850px"))),
            column(5,
            tags$h4("R-koodi kartan piirtämiseen"),
            verbatimTextOutput("output_code"))
         )

        # tabsetPanel(
        #   tabPanel("Esikatselu",
        #            withSpinner(plotOutput("plot_shape", width = "5", height = "850px")),
        #                        tags$h4("R-koodi kartan piirtämiseen"),
        #                        verbatimTextOutput("output_code"))#,
        #   # tabPanel("Esikatselu",
        #   #          withSpinner(tableOutput("temp")))
        #   # tabPanel("R-koodi kartan piirtämiseen", verbatimTextOutput("output_code"))
        # )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
   observeEvent(input$bookmark1, {
      session$doBookmark()
   })

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
         
         dataname <- paste0("municipality_key_",sub("^.+_", "", layer_row$name))
         tmp <- get(dataname)
         nms <- names(tmp)
         
         tagList(
            selectInput(inputId = "value_aggregation",
                        label = "Valitse aggregoitava alueluokitus:",
                        choices = nms,
                        selected = "mk_name"),
               tags$strong(
               tags$a(href = glue::glue("https://ropengov.github.io/geofi/reference/{dataname}.html"), 
                   "Katso lisätietoja saatavilla olevista alueluokituksista!"))
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
            theme_minimal() +
            labs(title = input$value_layer)
      } else {
         plot <- ggplot(shape) +
            theme_minimal(base_family = "PT Sans") +
            geom_sf(fill = NA) +
            labs(title = paste0("Pohjadata: ", input$value_layer),
                 subtitle = paste0("Aggregoitu muuttujan: ", input$value_aggregation, " tasolle"))
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
      
      req(input$value_org)
      req(input$value_layer)
      req(input$value_aggregation)

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
            ggsave(file, plot = p1, device = "svg", width = 8.3, height = 11.7)
         } else if (input$file_type == ".png") {
            # Write SVG using ggplot2
            p1 <- plot_ggplot()
            ggsave(file, plot = p1, device = "png", width = 8.3, height = 11.7)
         } else if (input$file_type == ".pdf") {
            # Write SVG using ggplot2
            p1 <- plot_ggplot()
            ggsave(file, plot = p1, device = cairo_pdf, width = 8.3, height = 11.7)
         }
      }
   )


   
   # output$temp <- renderTable({
   #    shape <- data_aggregate()
   #    st_drop_geometry(shape)
   # })
   
   
   output$output_code <- renderText({
      
      req(input$value_org)
      req(input$value_layer)
      req(input$value_aggregation)

      layer_row <- api_content[api_content$provider %in% input$value_org & api_content$title %in% input$value_layer,]

      if (grepl("kunta", layer_row$name)){
         code <- glue::glue('
# devtools::install_github("ropengov/geofi")
library(geofi)
library(dplyr)
library(ggplot2)
shape <- geofi::get_municipalities(year = {sub("^.+_", "", layer_row$name)}, scale = {sub("^.+kunta", "",  sub("k_.+$", "", layer_row$name))})
shape <- shape %>% 
    group_by({input$value_aggregation}) %>% 
    summarise()
ggplot(shape) + 
    geom_sf(fill = NA) +
    theme_minimal() +
    labs(title = "Pohjadata: {input$value_layer}",
         subtitle = "{paste0("Aggregoitu muuttujan: ", input$value_aggregation, " tasolle")}") 
')
# code <- code_base #paste0(code_base, " + fill()", collapse = "\n")
         if ("value_fill" %in% input$value_customize){
            # code <- paste0(code_base, " + fill()", collapse = "\n")
            code <- paste0(code,  
                   glue::glue(' + 
                                geom_sf(aes(fill = {input$value_aggregation})) +
                                labs(fill = "{input$value_aggregation}")'))
         } else {
            code <- code
         }
         if ("value_labels" %in% input$value_customize){
            code <- paste0(code,
                      glue::glue(' + 
                      ggrepel::geom_text_repel(data = shape %>%
                                              sf::st_set_geometry(NULL) %>%
                                              bind_cols(shape %>% sf::st_centroid() %>% sf::st_coordinates() %>% as_tibble()),
                                           aes(label = {input$value_aggregation}, x = X, y = Y))'))

         } else {
            code <- code
         }
         if ("value_background" %in% input$value_customize){
            code <- paste0(code,
                      glue::glue(' +
                      theme(axis.text = element_blank(),
                                 axis.title = element_blank(),
                                 panel.grid = element_blank())'))
         } else {
            code <- code
         }
         
      } else {
         code <- glue::glue('
# devtools::install_github("ropengov/geofi")
library(geofi)
library(dplyr)
library(ggplot2)
shape <- geofi::get_zipcodes(year = {sub("^.+_", "", layer_row$name)})
ggplot(shape) + 
    geom_sf(fill = NA) +
    theme_minimal() +
    labs(title = {input$value_layer})
')
         return(code)
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
enableBookmarking(store = "url")
# Run the application
shinyApp(ui = ui, server = server)

