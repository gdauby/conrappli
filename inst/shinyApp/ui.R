dashboardPage(
  dashboardHeader(title = "Preliminary IUCN evaluation and mapping"),
  dashboardSidebar(
    sidebarMenu(
      id = "menu1",
      menuItem("Load dataset", tabName = "tab_LOAD")
      ,
      menuItem("Mapping", tabName = "tab_MAP")
      ,
      menuItem("Evaluation", tabName = "tab_EVAL")
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
            column(9, selectInput("sel_taxa", "Taxa", choices = NULL)),
            column(9, selectInput("sel_genus", "Genus", choices = NULL)),
            column(9, selectInput("sel_species_epiteth", "Species epiteth", choices = NULL)),
            column(9, selectInput("sel_rank", "Rank infra-specific level ", choices = NULL)),
            column(9, selectInput("sel_lower_infra", "Name infra-specific level", choices = NULL)),

            # column(3, radioButtons("rad_units_diameter", "Unit:", choices = c("mm", "cm", "m"), selected = "cm")),

            # wood density argument
            hr(),
            # h4("Provide either wood density values or the taxonomy"),
            # selectInput("sel_WD", "Wood density", choices = NULL),
            # selectInput("sel_GENUS", "Genus (e.g. Terminalia) or scientific name (e.g. Terminalia superba or Terminalia superba Engl. & Diels)", choices = NULL),
            # selectInput("sel_SPECIES", "Species (e.g. superba)", choices = NULL),
            # hidden(div("Impossible combination", id = "msg_wd", style = "color:red;")),
            #
            # # Height argument
            # hr(),
            # h4("Optional"),
            # column(9, selectInput("sel_H", "Height", choices = NULL)),
            # column(3, radioButtons("rad_units_height", "Unit:", choices = c("cm", "m"), selected = "m")),
            selectInput("sel_LONG", "Coordinate longitude", choices = NULL),
            selectInput("sel_LAT", "Coordinate latitude", choices = NULL),
            # hidden(div("Impossible combination", id = "msg_h", style = "color:red;")),

            # plot id
            # hr(),
            # selectInput("sel_PLOT", "Plot name", choices = NULL),

            # action button to continue
            hr(),
            actionButton("btn_DATASET_LOADED", "Continue", color = "#0040FF")
          )),

          hidden(boxWithId(
            id = "box_DATASET", title = "Dataset file preview content", width = 12,
            dataTableOutput("table_DATASET")
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

        mapview::mapviewOutput(outputId="map"),

        boxWithId(id = "box_SELECT_TAX", actionButton("btn_SELECT_TAX", "evaluation"))
        # ,
        # hidden(boxWithId(
        #   id = "box_RESULT_TAXO", title = "Result", width = 12,
        #   verbatimTextOutput("out_taxo_error"),
        #   hr(),
        #   verbatimTextOutput("out_wd_error")
        # )),
        # hidden(boxWithId(id = "box_TAXO_DONE", actionButton("btn_TAXO_DONE", "continue")))
      )
       ,

      # Evaluation ----------------------------------------------------------------
      tabItem(
        "tab_EVAL",
        box(
          title = "Evaluate preliminary status", width = 12,
          sliderInput(inputId = "aoo_km_res", label = "AOO resolution", min=0.1, max=50, value = 4, round=TRUE, step=1),
          numericInput("repeat_pos_aoo", "Number of random different position for overlaying the grid", 10),
          sliderInput(inputId = "locations_km_res", label = "Locations resolution", min=0.1, max=50, value = 10, round=TRUE, step=1),
          actionButton("eval_species", "Compute and map evaluation")
        )
        ,
        hidden(boxWithId(
          id = "eval_species_res", title = "Parameters values", width = 12,
          verbatimTextOutput("results_conr")
        )),
        textOutput("summary3"),

        mapview::mapviewOutput(outputId="map2")
        # ,
        # hidden(boxWithId(
        #   id = "box_RESULT_TAXO", title = "Result", width = 12,
        #   verbatimTextOutput("out_taxo_error"),
        #   hr(),
        #   verbatimTextOutput("out_wd_error")
        # )),
        # hidden(boxWithId(id = "box_TAXO_DONE", actionButton("btn_TAXO_DONE", "continue")))
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
