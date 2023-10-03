

data_country_ui <- function(id) {
  ns <- NS(id)
  template_ui(
    title = i18n("Gabon's threatened plant species"),

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
            label = "Which kind of map:",
            choices = c("Occurences" = "occ", "Grid" = "grid"),
            selected = "grid"
          ),
          conditionalPanel(
            condition = "input.type_map == 'grid'",
            ns = ns,
            sliderInput(
              inputId = ns("resolution"),
              label = tagList(
                "Grid size (km):",
                btn_help(
                  "Grid size"
                )
              ),
              min = 5,
              max = 40,
              value = 20,
              round = TRUE,
              step = 1,
              width = "100%"
            )
          )
        )
      ),

      column(
        width = 9,
        bslib::card(
          bslib::card_header("Map"),
          leaflet::leafletOutput(outputId = ns("map"), height = "600px")
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

        extract_sp <- search_species_info(
          species_name = unique(threat_taxa$accepetedtaxonname)
        )
        keys <- extract_sp$specieskey
        data <- retrieve_occ_data(keys)
        # TEMP
        # Sys.sleep(2)
        # dataset_rv$value <- readRDS("D:\\work\\ConRapp\\conrappli\\dev\\data_country.rds")
        # TEMP

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
          shiny::need(input$country, i18n("Please select a country"))
        )
        CHECK_DATA <<- req(data_validated_r())
        if (identical(input$type_map, "grid")) {
          req(data_validated_r()) %>%
            draw_map_grid(resolution = input$resolution)
        } else if (identical(input$type_map, "occ")) {
          req(data_validated_r()) %>%
            draw_map_occ()
        }
      })

    }
  )
}
