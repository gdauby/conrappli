
#' @importFrom shiny NS fluidRow column conditionalPanel radioButtons sliderInput
#' @importFrom shinyWidgets panel virtualSelectInput dropMenu
data_country_ui <- function(id) {
  ns <- NS(id)
  template_ui(
    title = i18n("Gabon's threatened tree species"),
    fluidRow(

      column(
        width = 3,
        shinyWidgets::panel(
          status = "primary",
          shinyWidgets::virtualSelectInput(
            inputId = ns("country"),
            label = i18n("Country to explore:"),
            choices = c("Gabon"),
            selected = character(0),
            search = TRUE,
            width = "100%"
          ),
          shinyWidgets::dropMenu(
            actionButton(
              inputId = ns("see_var_sel"),
              class = "btn-outline-primary w-100 mb-2",
              label = tagList(
                i18n("See variable selection"),
                phosphoricons::ph("caret-down", title = i18n("See variable selection"))
              )
            ),
            tags$div(
              style = "width: 700px; height: 500px; overflow: auto;",
              data_variable_ui(ns("variable"))
            )
          ),
          shinyWidgets::dropMenu(
            actionButton(
              inputId = ns("see_data_valid"),
              class = "btn-outline-primary w-100 mb-2",
              label = tagList(
                i18n("See data validation"),
                phosphoricons::ph("caret-down", title = i18n("See data validation"))
              )
            ),
            data_validation_ui(ns("validation"))
          ),
          radioButtons(
            inputId = ns("type_map"),
            label = tagList(
              i18n("Which kind of map:"),
              btn_help(
                i18n("'Grid' map show the number of threatened species in grid of given resolution, while 'Occurrences' show all occurrences")
              )
            ),
            choices = c("Occurences" = "occ", "Grid" = "grid"),
            selected = "grid"
          ),
          sliderInput(
            inputId = ns("resolution"),
            label = tagList(
              i18n("Grid size (km):"),
              btn_help(
                i18n("Define the resolution of the grid to show the density of threatened species")
              )
            ),
            min = 2,
            max = 40,
            value = 5,
            round = TRUE,
            step = 1,
            width = "100%"
          ),
          shinyWidgets::pickerInput(
            inputId = ns("cat_chosen"),
            label = i18n("Which IUCN category ?"), 
            choices = c("CR", "EN", "VU"),
            multiple = TRUE,
            selected = c("CR", "EN", "VU"), 
            # choicesOpt = list(
            #   content = sprintf("<span class='badge text-bg-%s'>%s</span>", 
            #                     c("info", "success", "danger", "primary", "warning"), 
            #                     c("CR", "EN", "VU"))),
            options = shinyWidgets::pickerOptions(container = "body")
          )
        )
      ),

      column(
        width = 9,
        bslib::card(
          bslib::card_header("Map"),
          leaflet::leafletOutput(outputId = ns("map"), height = "600px"),
          downloadButton(
            outputId = ns("download_report"),
            label = i18n("Download the report"),
            class = "disabled",
            style = "width: 100%;"
          )
        )
      )

    )
  )
}

data_country_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      data_r <- reactive({
        req(input$country)
        shinybusy::show_modal_spinner(
          spin = "half-circle",
          color = "#088A08",
          text = i18n("Extracting data, please wait. It may takes several minutes.")
        )
        mydb_extract <- conn_mydb_rb(
          pass = "Anyuser2022",
          user = "common"
        )
        if (input$country == "Gabon")
          full_table <- func_try_fetch(con = mydb_extract, sql = "SELECT * FROM table_threat_taxa_gab")
        threat_taxa <-  full_table %>%
          filter(redlistcategory %in% c("CR", "EN", "VU"))
        
        
        # threat_taxa <- threat_taxa %>% dplyr::slice(1:20)

        extract_sp <- search_species_info(
          species_name = unique(threat_taxa$accepetedtaxonname)
        )
        keys <- extract_sp$specieskey
        data <- retrieve_occ_data(keys)
        
        
        cat_keys <- threat_taxa %>% 
          dplyr::left_join(extract_sp %>% dplyr::select(provided_sciname, specieskey), 
                                              by = c("accepetedtaxonname" = "provided_sciname")) %>% 
          dplyr::select(specieskey, redlistcategory, accepetedtaxonname)
        cat_keys <- 
          cat_keys %>% 
          dplyr::group_by(specieskey) %>% 
          dplyr::summarise(redlistcategory = dplyr::first(redlistcategory))
        
        data <- data %>%
          dplyr::left_join(cat_keys,
                    by = c("speciesKey" = "specieskey"))
        
        print(data %>% dplyr::select(redlistcategory))

        shinybusy::remove_modal_spinner()
        return(data)
      })

      variable_r <- data_variable_server(
        id = "variable",
        data_r = data_r
      )

      data_validated_r <- data_validation_server(
        id = "validation",
        data_r = reactive({
          req(variable_r())
          variable_r()$data
        })
      )


      output$map <- leaflet::renderLeaflet({
        shiny::validate(
          shiny::need(input$cat_chosen, i18n("Please select at least one category"))
        )
        shiny::validate(
          shiny::need(input$country, i18n("Please select a country"))
        )
        if (identical(input$type_map, "grid")) {
          req(data_validated_r()) %>%
            draw_map_grid(resolution = input$resolution, categories = input$cat_chosen)
        } else if (identical(input$type_map, "occ")) {
          req(data_validated_r()) %>%
            draw_map_occ(categories = input$cat_chosen)
        }
      })


      observe({
        if (isTruthy(input$country)) {
          shinyjs::removeCssClass(id = "download_report", class = "disabled")
        } else {
          shinyjs::removeCssClass(id = "download_report", class = "disabled")
        }
      })


      output$download_report <- downloadHandler(
        filename = function() {
          paste0("ConR-report-country", "-", Sys.Date(), ".html")
        },
        content = function(file) {

          req(data_validated_r())
          
          data = data_validated_r()
          
          print(data)
          
          print(names(data))

          tmp <- tempfile(fileext = ".html")

          shinyWidgets::execute_safely({
            rmarkdown::render(
              input =  system.file(package = "conrappli", "reports/country_threat_report.Rmd"),
              output_format = rmarkdown::html_document(
                theme = bs_theme_conr(),
                number_sections = TRUE,
                toc = TRUE,
                toc_float = TRUE,
                toc_depth = 5,
                self_contained = TRUE
              ),
              params = list(
                data = data_validated_r(),
                resolution = input$resolution,
                type_map = input$type_map,
                country = input$country,
                categories = input$cat_chosen
              ),
              output_file = tmp
            )
          })

          file.copy(from = tmp, to = file)
        }
      )

    }
  )
}
