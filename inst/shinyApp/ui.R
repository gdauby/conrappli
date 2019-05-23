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
      menuItem("Evaluation - Criterion A", tabName = "tab_EVAL2")
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
            dataTableOutput("table_DATASET"),
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

        boxWithId(id = "box_SELECT_TAX", actionButton("btn_SELECT_TAX", "EVALUATION")),

        mapview::mapviewOutput(outputId="map"),

        hr(),

        hr(),
        hidden(plotOutput("plot_alt", width = "50%")),
        hr(),
        hidden(boxWithId(
          id = "box_DATASET_SPECIES", title = "Species dataset file preview content", width = 12,
          dataTableOutput("table_DATASET_SPECIES"),
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
          numericInput("repeat_pos_aoo", "Number of random different position for overlaying grids", 10),
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
          dataTableOutput("table_occupied_pa")
        )),
        textOutput("summary3"),

        mapview::mapviewOutput(outputId="map2"),

        uiOutput("evaluation_CA")

      ),


      # Evaluation Criterion A ----------------------------------------------------------------
      tabItem(
        "tab_EVAL2",
        box(
          title = "Evaluate preliminary status following Criterion A", width = 8,
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
        boxWithId(
          id = "box_Report", downloadButton("species_report", label = "Report")
        )
      )


      # tabItem(
      #   "tab_TAXO",
      #   box(
      #     title = "Wood density (WD) extraction", width = 12,
      #     radioButtons(
      #       "rad_WD", "Correct the taxonomy (mispelling) before wood density extraction?",
      #       c(
      #         "Correct taxonomy and extract WD" = "corr",
      #         "Extract WD without taxonomic correction" = "WD"
      #       )
      #     ),
      #     actionButton("btn_TAXO_RESULT", "Go on")
      #   ),
      #   hidden(boxWithId(
      #     id = "box_RESULT_TAXO", title = "Result", width = 12,
      #     verbatimTextOutput("out_taxo_error"),
      #     hr(),
      #     verbatimTextOutput("out_wd_error")
      #   )),
      #   hidden(boxWithId(id = "box_TAXO_DONE", actionButton("btn_TAXO_DONE", "continue")))
      # )
      # ,


      # heigth ------------------------------------------------------------------


      # tabItem(
      #   "tab_HEIGHT",
      #   box(
      #     title = "HD model", width = 12,
      #     checkboxGroupInput(
      #       "chkgrp_HEIGHT", "Choose the HD model:",
      #       inline = T,
      #       c(
      #         "Local HD model" = "HDloc",
      #         "Feldpausch" = "feld",
      #         "Chave" = "chave"
      #       )
      #     )
      #   ),
      #   column(
      #     6,
      #     ## HD model
      #     hidden(boxWithId(
      #       id = "box_RESULT_HDMOD", title = "Local HD model (model accuracy is estimated on all HD data)", width = 12,
      #       tableOutput("out_tab_HD"),
      #       radioButtons("rad_HDMOD", "Choose your HD model:", choices = "NULL")
      #     )),
      #
      #     ## Map
      #     hidden(boxWithId(
      #       id = "box_MAP", title = "Map", width = 12,
      #       numericInput("num_LONG", "longitude", 3.8614, min = -180, max = 180, step = 0.01),
      #       numericInput("num_LAT", "latitude", 43.652, min = -90, max = 90, step = 0.01),
      #       plotOutput("plot_MAP")
      #     ))
      #   ),
      #
      #   column(
      #     6,
      #     ## Feldpauch
      #     hidden(boxWithId(
      #       id = "box_RESULT_FELD", title = "Feldpausch et al. (2012)", width = 12,
      #       textOutput("txt_feld")
      #     )),
      #
      #     ## chave
      #     hidden(boxWithId(
      #       id = "box_result_chave", title = "Chave et al. (2014)", width = 12,
      #       textOutput("txt_chave")
      #     )),
      #
      #     ## comparison of the methods
      #     hidden(boxWithId(
      #       id = "box_plot_comparison", title = "Model comparison", width = 12,
      #       plotOutput("out_plot_comp")
      #     )),
      #
      #     hidden(boxWithId(
      #       id = "box_RESULT_HDEND", title = NULL, width = 12,
      #       actionButton("btn_HD_DONE", "continue")
      #     ))
      #   )
      # ),


      # AGB -----------------------------------------------------------------
      # tabItem(
      #   "tab_AGB",
      #   fluidRow(
      #     box(
      #       title = "AGB estimation",
      #       radioButtons("rad_AGB_MOD", NULL, choices = c("AGB" = "agb", "AGB + error" = "agbe"), inline = T),
      #       actionButton("btn_AGB_DONE", "Go on")
      #     ),
      #     hidden(boxWithId(
      #       id = "box_AGB_res", title = "AGB result", width = 12,
      #       plotOutput("out_plot_AGB")
      #     )),
      #     hidden(boxWithId(
      #       id = "box_AGB_Report", downloadButton("dwl_report", label = "Report"),
      #       downloadButton("dwl_file", label = "file FOS")
      #     ))
      #   )
      # )
    )
  )
)
