dashboardPage(
  dashboardHeader(title = "Preliminary Conservation Assessments following IUCN criteria",
                  titleWidth=845,
                  tags$li(class = "dropdown",
                          tags$a(href="https://en.ird.fr/", target="_blank",
                                 tags$img(height = "20px",
                                          src="https://en.ird.fr/themes/custom/ird_theme/logo.png")
                          ),
                          tags$a(href="http://www.missouribotanicalgarden.org/", target="_blank",
                                 tags$img(height = "20px",
                                          src="http://www.missouribotanicalgarden.org/Portals/0/Images/logo.gif?ver=2011-10-24-173805-843")
                          ),
                          tags$a(href="https://www.plantentuinmeise.be/", target="_blank",
                                 tags$img(height = "20px",
                                          src="https://www.plantentuinmeise.be/images/cc/logo_en.svg")
                          )
                          )),
  dashboardSidebar(
    sidebarMenu(
      id = "menu1",
      menuItem("Load dataset", tabName = "tab_LOAD")
      ,
      menuItem("Mapping", tabName = "tab_MAP")
      ,
      menuItem("Evaluation - Criterion B", tabName = "tab_EVAL")
      ,
      menuItem("Habitat quality/population decline", tabName = "tab_EVAL2")
      ,
      menuItem("Summary report", tabName = "tab_SUMMARY")
      # ,
      # menuItem("Height-diameter model", tabName = "tab_HEIGHT"),
      # menuItem("AGB calculation", tabName = "tab_AGB")
    )
  ),
  dashboardBody(
    shinyalert::useShinyalert(),
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
    tabItems(

      # load dataset ------------------------------------------------------------

      tabItem(
        "tab_LOAD",
        fluidRow(
          box( # box with the file input
            title = "Species dataset", width = 6,
            fileInput("file_DATASET", "Select data file", accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            ))
            # ,
            # numericInput("num_skip_line", "Skip lines", value = 0, min = 0),
            # radioButtons("rad_decimal", "Decimal:", choices = c(Dot = ".",
            #                                                     Comma = ","))
          ),

          hidden(boxWithId( # box for the input
            id = "box_FIELDS", title = "Column selection", width = 6,

            # obligatory argument
            strong("Taxa column selection - select either taxa OR others columns"),
            selectInput("sel_taxa", "Taxa", choices = NULL),
            selectInput("sel_genus", "Genus", choices = NULL),
            selectInput("sel_species_epiteth", "Species epiteth", choices = NULL),
            selectInput("sel_authors", "Authors", choices = NULL),
            selectInput("sel_rank", "Rank infra-specific level", choices = NULL),
            selectInput("sel_lower_infra", "Name infra-specific level", choices = NULL),
            selectInput("sel_lower_infra_authors", "Authors infra-specific level", choices = NULL),
            hr(),

            hr(),
            strong("Coordinates and altitude column selection"),
            selectInput("sel_LONG", "Longitude (decimal degrees)", choices = NULL),
            selectInput("sel_LAT", "Latitude (decimal degrees)", choices = NULL),
            selectInput("sel_ALT", "Altitude (m)", choices = NULL),
            hr(),
            selectInput("sel_YEAR", "Collection year", choices = NULL),
            # action button to continue
            hr(),
            actionButton("btn_DATASET_LOADED", "Continue", color = "#0040FF")
          )),

          hidden(boxWithId(
            id = "box_DATASET", title = "Dataset file preview content", width = 12,
            DT::dataTableOutput("table_DATASET"),
            uiOutput("show_col")
          ))
        ),
        textOutput("summary")
      )
       ,

      # Mapping ----------------------------------------------------------------
      tabItem(
        "tab_MAP",
        box(
          title = "Select species to check", width = 12,
          uiOutput("list_taxa"),
          actionButton("check_species", "Map and show summary range statistics")
        ),
        hidden(boxWithId(
          id = "check_species_stat", title = "Taxa summary", width = 12,
          verbatimTextOutput("nbe_occ"),
          hr(),
          verbatimTextOutput("nbe_missing_coord")
        )),
        textOutput("summary2"),

        boxWithId(id = "box_SELECT_TAX", shinyWidgets::actionBttn(
          inputId = "btn_SELECT_TAX",
          label = "EVALUATION",
          color = "royal",
          style = "jelly"
        )),

        mapview::mapviewOutput(outputId="map"),

        hr(),

        hr(),
        hidden(plotOutput("plot_alt", width = "50%")),
        hr(),
        hidden(boxWithId(
          id = "box_DATASET_SPECIES", title = "Species dataset file preview content", width = 12,
          DT::dataTableOutput("table_DATASET_SPECIES"),
          uiOutput("show_col_species")
        ))


        # ,
        # hidden(boxWithId(
        #   id = "box_RESULT_TAXO", title = "Result", width = 12,
        #   verbatimTextOutput("out_taxo_error"),
        #   hr(),
        #   verbatimTextOutput("out_wd_error")
        # )),
        # hidden(boxWithId(id = "box_TAXO_DONE", actionButton("btn_TAXO_DONE", "continue")))
      ),

      # Evaluation Criterion B ----------------------------------------------------------------
      tabItem(
        "tab_EVAL",
        box(
          title = "Evaluate preliminary status", width = 8,
          sliderInput(inputId = "aoo_km_res", label = "AOO resolution", min=0.1, max=50, value = 2, round=TRUE, step=1),
          sliderInput(inputId = "locations_km_res", label = "Locations resolution", min=0.1, max=50, value = 10, round=TRUE, step=1),
          numericInput("repeat_pos_aoo", "Number of random different position for overlaying grids", 50),
          sliderInput(inputId = "sub_pop_resol", label = "Resolution of sub-population (circular buffer method)", min=1, max=200, value = 10, round=TRUE, step=1),
          sliderInput(inputId = "threshold_mayaux", label = "Threshold of the proportion of human-impacted land cover", min=0, max=1, value = 0.5, round=FALSE, step=0.1),
          shinyWidgets::actionBttn(
            inputId = "info_mayaux",
            label = "Info on this threshold?",
            color = "primary",
            style = "bordered"
          ),

          sliderInput(inputId = "deforest",
                      label = "Threshold of the proportion of forest cover loss between 2000 and 2018",
                      min=0, max=1, value = 0.5, round=FALSE, step=0.1),

          shinyWidgets::actionBttn(
            inputId = "info_deforest",
            label = "Info on this threshold?",
            color = "primary",
            style = "bordered"
          ),

          shinyWidgets::actionBttn(
            inputId = "eval_species",
            label = "Compute evaluation and map",
            color = "royal",
            style = "simple"
          )
          # actionButton("eval_species", "Compute and map evaluation")
        )
        ,
        hidden(boxWithId(
          id = "eval_species_res", title = "Parameters values", width = 12,
          htmlOutput("title_eval"),
          verbatimTextOutput("results_conr"),
          DT::dataTableOutput("table_occupied_pa")
        )),
        textOutput("summary3"),

        mapview::mapviewOutput(outputId="map2"),

        uiOutput("evaluation_CA")

      ),


      # Evaluation Criterion A ----------------------------------------------------------------
      tabItem(
        "tab_EVAL2",
        box(
          title = "Infer habitat and population decline", width = 8,
          sliderInput(inputId = "threshold_mayaux_CA", label = "Threshold of the proportion of human-impacted land cover", min=0, max=1, value = 0.5, round=FALSE, step=0.1),
          shinyWidgets::actionBttn(
            inputId = "info_mayaux",
            label = "Info on this threshold?",
            color = "primary",
            style = "bordered"
          ),

          sliderInput(inputId = "deforest_CA",
                      label = "Threshold of the proportion of forest cover loss between 2000 and 2018",
                      min=0, max=1, value = 0.5, round=FALSE, step=0.1),

          shinyWidgets::actionBttn(
            inputId = "info_deforest",
            label = "Info on this threshold?",
            color = "primary",
            style = "bordered"
          ),

          shinyWidgets::actionBttn(
            inputId = "eval_species_CA",
            label = "Compute evaluation and map",
            color = "royal",
            style = "simple"
          )
          # actionButton("eval_species", "Compute and map evaluation")
        )
        ,
        hidden(boxWithId(
          id = "eval_species_res_CA", title = "Parameters values", width = 12,
          htmlOutput("title_eval_CA"),
          verbatimTextOutput("results_CA")
        )),
        textOutput("summary3_CA"),

        mapview::mapviewOutput(outputId="map_CA"),
        textOutput("summary_CA"),

        uiOutput("see_report")

      ),

      # Summary report ----------------------------------------------------------------
      tabItem(
        "tab_SUMMARY",

        textOutput("summary_rep"),
        boxWithId(
          id = "box_Report", downloadButton("species_report", label = "Report")
        )
      )
    )
  )
)
