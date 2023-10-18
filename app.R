library(dplyr)
library(ggplot2)
library(janitor)
library(leaflet)
library(glue)
library(geofi)
library(sf)
library(shiny)
library(shinycssloaders)
library(svglite)
library(ggrepel)
library(extrafont)
library(hrbrthemes)
library(bslib)
library(metathis)

# options(shiny.sanitize.errors = FALSE)
options(shiny.useragg = TRUE)

# eri valikkojen vaihtoehdoista
year_current <- as.integer(substr(Sys.Date(), start = 1, stop = 4))
tk_geo_menu_list <- list("municipality" = list("year_min" = 2013,
                                               "year_max" = year_current,
                                               "resolution" = c(1000,4500)),
                         "zipcodes" = list("year_min" = 2015,
                                           "year_max" = year_current),
                         "statistical_grid" = list("resolution" = c(1,5),
                                                   "auxillary_data" = c(FALSE,TRUE)),
                         "population_grid" = list("year_min" = 2010,
                                                  "year_max" = 2020,
                                                  "resolution" = c(1,5))
                         )

ui <- fluidPage(lang = "fi",
                title = "geofi-selain",
                tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
                meta() %>% 
                  meta_description(description = "geofi-selain") %>% 
                  meta_social(
                    title = "geofi-selain",
                    description = "geofi-selain: tarkastele geofi R-paketilla saatavia datoja selaimessa",
                    url = "",
                    image = "geofi_selain,png",
                    image_alt = "An image for social media cards",
                    twitter_creator = "@muuankarski",
                    twitter_card_type = "summary_large_image",
                    twitter_site = "@muuankarski"
                  ),
                theme = bslib::bs_theme(bootswatch = "cosmo",
                                        # bg = "#0b3d91", fg = "white", primary = "#FCC780",
                                        base_font = font_google("PT Sans"),
                                        code_font = font_google("Space Mono")),
                tags$html(HTML('<a class="sr-only sr-only-focusable" href="#maincontent">Skip to main</a>')),
                tags$style(HTML("
      .navbar-xyz {
        background-color: rgb(255, 255, 255, .9);
        border-bottom: 1px solid rgb(55, 55, 55, .4);
      }
      #map {
    margin: auto;
  }")),
                tags$html(HTML('
    <nav class="navbar navbar-light sticky-top navbar-xyz">
      <a class="navbar-brand" role="brand" href = "https://ropengov.github.io/geofi/"><img src = "https://ropengov.github.io/geofi/reference/figures/logo.png" style = "height: 35px; padding-right: 0px;" alt = "Kompassi"></a>
      <div class = "lead">geofi-selain</div>
      <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarResponsive" aria-controls="navbarResponsive" aria-expanded="false" aria-label="Avaa valikko">
        <span class="navbar-toggler-icon"></span>
      </button>
      <div role = "navigation" class="collapse navbar-collapse justify-content-between" id="navbarResponsive">
        <ul class="navbar-nav ml-auto">
          <li class="nav-item">
            <a class="nav-link text-dark" href="https://github.com/rOpenGov/geofi_selain"><code>&lt;sovelluksen lahdekoodi/&gt;&gt;</code></a>
          </li>
          <li class="nav-item">
            <a class="nav-link text-dark" href="https://ropengov.github.io/geofi/">geofi-paketti</a>
          </li>
                    <li class="nav-item">
            <a class="nav-link text-dark" href="http://ropengov.org/">ropengov.org</a>
          </li>

        </ul>
      </div>
  </nav>')),
                tags$html(HTML('<main id="maincontent">')),
                tags$h2("", id = "alku"),
   sidebarLayout(
      sidebarPanel(
         tags$p("Tällä sovelluksella voit selata eri ",tags$a(href = "https://github.com/rOpenGov/geofi", tags$code("geofi")),"-R-paketin tarjoamia datoja ja toiminnallisuuksia. Voit tallentaa aineistoja", 
                tags$code("GeoPackage, Shapefile, .svg, .pdf tai .png"),"-muotoihin sekä aggregoida kuntatason datoja ylemmille aluejaoille."),
         tags$h4("1. Luo kartta"),
      uiOutput("ui_data_spatial"),
      uiOutput("ui_arguments"),
      uiOutput("ui_aggregate"),
      uiOutput("customize_plot"),
      # bookmarkButton(id = "bookmark1",
      #                label = "Jaa valinnat"),
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
      tags$p("Markus Kainu & Joona Lehtomäki 2020-2023")
      ),

      mainPanel(
         shiny::fluidRow(column(7,
            tags$h4("Kartan esikatselu"),
            plotOutput("plot_shape", width = "100%", height = "850px")),
            column(5,
            tags$h4("R-koodi kartan piirtämiseen")
            ,verbatimTextOutput("output_code")
            )
         )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
   # observeEvent(input$bookmark1, {
   #    session$doBookmark()
   # })

   output$ui_data_spatial <- renderUI({

     layers <- names(tk_geo_menu_list)

     tagList(
       selectInput(inputId = "value_data_spatial",
                   label = "Valitse pohja-aineisto:",
                   choices = layers,
                   selected = layers[1])
     )
   })
   
   # Menu for extra arguments
   output$ui_arguments <- renderUI({
      
     req(input$value_data_spatial)
     # Year
     if (input$value_data_spatial %in% c("municipality","zipcodes","population_grid")){
       menu_01 <- selectInput("value_year", 
                              label = "Vuosi", 
                              choices = rev(tk_geo_menu_list[[input$value_data_spatial]]$year_min:tk_geo_menu_list[[input$value_data_spatial]]$year_max))
     } else {
       menu_01 <- list()
     }
     # Resolution
     if (input$value_data_spatial %in% c("municipality","statistical_grid","population_grid")){
       menu_02 <- selectInput("value_resolution", 
                              label = "Resoluutio", 
                              choices = rev(tk_geo_menu_list[[input$value_data_spatial]]$resolution))
     } else {
       menu_02 <- list()
     }
     if (input$value_data_spatial %in% c("statistical_grid")){
       menu_03 <- selectInput("value_aux", 
                              label = "Include auxillary data", 
                              choices = rev(tk_geo_menu_list[["statistical_grid"]]$auxillary_data))
     }else {
       menu_03 <- list()
     }
         tagList(
           list(menu_01,menu_02,menu_03)
         )
   })
   
   # Menu for aggregation
   output$ui_aggregate <- renderUI({
     
     req(input$value_data_spatial)
     if (input$value_data_spatial %in% c("municipality","zipcodes","population_grid")) req(input$value_year)
     if (input$value_data_spatial %in% c("municipality","statistical_grid","population_grid")) req(input$value_resolution)
     
     if (input$value_data_spatial %in% c("municipality")){
       key_data <- get(paste0("municipality_key_",input$value_year))
       nms <- names(key_data)
       nms_fi <- grep("_fi$", nms, value = TRUE)

       menu_01 <- selectInput("value_aggregate", 
                              label = "Valitse aggregoitava aluetaso", 
                              choices = nms_fi)
     } else {
       menu_01 <- list()
     }
     tagList(
       list(menu_01)
     )
   })

   data_input <- reactive({

     req(input$value_data_spatial)
     if (input$value_data_spatial %in% c("municipality","zipcodes","population_grid")) req(input$value_year)
     if (input$value_data_spatial %in% c("municipality","statistical_grid","population_grid")) req(input$value_resolution)

     if (input$value_data_spatial == "municipality"){
       shape <- geofi::get_municipalities(year = input$value_year, scale = input$value_resolution)
     }
     if (input$value_data_spatial == "zipcodes"){
       shape <- geofi::get_zipcodes(year = input$value_year)
     }
     if (input$value_data_spatial == "statistical_grid"){
       shape <- geofi::get_statistical_grid(resolution = input$value_resolution
                                            # ,auxiliary_data = input$value_aux
                                            ,auxiliary_data = TRUE # lets incorporate aux data in every case as needed to subset Helsinki in plots
                                            )
     }
     if (input$value_data_spatial == "population_grid"){
       shape <- geofi::get_population_grid(year = input$value_year, resolution = input$value_resolution)
     }
     return(shape)
 })


   data_aggregate <- reactive({

     req(input$value_data_spatial)
     if (input$value_data_spatial %in% c("municipality","zipcodes","population_grid")) req(input$value_year)
     if (input$value_data_spatial %in% c("municipality","statistical_grid","population_grid")) req(input$value_resolution)
     req(input$value_aggregate)

      if (input$value_data_spatial != "municipality"){
         shape2 <- data_input()
      } else if (input$value_aggregate %in% c("municipality_name_fi","name_fi")){
      shape2 <- data_input()
      shape2$aggvar <- shape2[[input$value_aggregate]]
      } else {
         shape <- data_input()
         shape$aggvar <- shape[[input$value_aggregate]]
         shape2 <- shape %>%
            dplyr::group_by(aggvar) %>%
            dplyr::summarise()
      }
      return(shape2)
   })

   output$customize_plot <- renderUI({

      if (input$value_data_spatial != "municipality"){
      # 
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
      if (input$value_data_spatial %in% c("statistical_grid")){
        shape <- shape[shape$kunnro2021 == 91,]
      }
      if (input$value_data_spatial %in% c("population_grid")){
        shape <- shape[shape$kunta == "091",]
      }
      if (input$value_data_spatial %in% c("zipcodes")){
        # shape$aggvar <- shape$nimi
      }

      if (input$value_data_spatial != "municipality"){
         plot <- ggplot(shape) +
            geom_sf(fill = NA) +
            theme_minimal() +
            labs(title = input$value_data_spatial)
      } else {
        plot <- ggplot(shape) +
          geom_sf(fill = NA) +
          theme_minimal() +
          labs(title = input$value_data_spatial)
         plot <- ggplot(shape) +
            theme_minimal(base_family = "Space Mono") +
            geom_sf(fill = NA) +
            labs(title = paste0("Pohjadata: ", input$value_data_spatial),
                 subtitle = paste0("Aggregoitu muuttujan: ", input$value_aggregate, " tasolle"))
         if ("value_fill" %in% input$value_customize){
            plot <- plot + geom_sf(aes(fill = aggvar)) + labs(fill = input$value_aggregate)
         }
         if ("value_labels" %in% input$value_customize){
            plot <- plot + geom_text_repel(data = shape %>%
                                              sf::st_set_geometry(NULL) %>%
                                              bind_cols(shape %>% st_centroid() %>% st_coordinates() %>% as_tibble()),
                                           aes(label = aggvar, x = X, y = Y), family = "Space Mono")
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

     req(input$value_data_spatial)
     if (input$value_data_spatial %in% c("municipality","zipcodes","population_grid")) req(input$value_year)
     if (input$value_data_spatial %in% c("municipality","statistical_grid","population_grid")) req(input$value_resolution)

     plot_ggplot()
   })


   output$download_data <- downloadHandler(

      filename = function() {
         dataset_name <- janitor::make_clean_names(input$value_data_spatial,
                                                   case = "snake") %>%
            paste0(input$file_type)
         return(dataset_name)
      },

      content = function(file) {
         # Reactives are cached
         dat <- data_aggregate()
         # This is slightly redundant but the only way to name the shapefile
         # compoments inside the zip file
         dataset_name <- janitor::make_clean_names(input$value_data_spatial,
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

     req(input$value_data_spatial)
     if (input$value_data_spatial %in% c("municipality","zipcodes","population_grid")) req(input$value_year)
     if (input$value_data_spatial %in% c("municipality","statistical_grid","population_grid")) req(input$value_resolution)
     req(input$value_aggregate)

     if (input$value_data_spatial == "municipality"){
       code_shape <- glue("shape <- geofi::get_municipalities(year = {input$value_year}, scale = {input$value_resolution})")
     }
     if (input$value_data_spatial == "zipcodes"){
       code_shape <- glue("shape <- geofi::get_zipcodes(year = {input$value_year})")
     }
     if (input$value_data_spatial == "statistical_grid"){
       code_shape <- glue("shape <- geofi::get_statistical_grid(resolution = {input$value_resolution}, auxiliary_data = TRUE)")
     }
     if (input$value_data_spatial == "population_grid"){
       code_shape <- glue("shape <- geofi::get_population_grid(year = {input$value_year}, resolution = {input$value_resolution})")
     }
     
     
     if (input$value_data_spatial == "municipality"){
         code <- glue::glue('
# install.packages("geofi")
library(geofi)
library(dplyr)
library(ggplot2)
{code_shape}
shape <- shape %>%
    group_by({input$value_aggregate}) %>%
    summarise()
ggplot(shape) +
    geom_sf(fill = NA) +
    theme_minimal() +
    labs(title = "Pohjadata: {input$value_data_spatial}",
         subtitle = "{paste0("Aggregoitu muuttujan: ", input$value_aggregate, " tasolle")}")
')
         
         if ("value_fill" %in% input$value_customize){
            # code <- paste0(code_base, " + fill()", collapse = "\n")
            code <- paste0(code,
                   glue::glue(' +
                                geom_sf(aes(fill = {input$value_aggregate})) +
                                labs(fill = "{input$value_aggregate}")'))
         } else {
            code <- code
         }
         
         if ("value_labels" %in% input$value_customize){
            code <- paste0(code,
                      glue::glue(' +
                      ggrepel::geom_text_repel(data = shape %>%
                                              sf::st_set_geometry(NULL) %>%
                                              bind_cols(shape %>% sf::st_centroid() %>% sf::st_coordinates() %>% as_tibble()),
                                           aes(label = {input$value_aggregate}, x = X, y = Y))'))

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
# install.packages("geofi")
library(geofi)
library(ggplot2)
{code_shape}
ggplot(shape) +
    geom_sf(fill = NA) +
    theme_minimal() +
    labs(title = "{input$value_data_spatial}")
')
      }
     return(code)
   })

   
}
# enableBookmarking(store = "url")
# Run the application
shinyApp(ui = ui, server = server)

