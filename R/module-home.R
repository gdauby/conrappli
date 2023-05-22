
#' @importFrom shiny NS fluidRow column actionButton actionLink
#' @importFrom datamods i18n
home_ui <- function(id) {
  ns <- NS(id)
  template_ui(
    title = i18n("Welcome to ConApp !"),
    fluidRow(
      column(
        width = 8, offset = 2,
        tags$p(
          class = "fs-5 mt-5",
          i18n("This app is designed to help you:"),
          tags$ol(
            tags$ol(
              i18n("Identify the presence of threatened (extinction risk status published on the IUCN Red List; see "),
              tags$a(
                "<http://www.iucnredlist.org>",
                href = "http://www.iucnredlist.org"
              ),
              i18n(") or potentially threatened (preliminary extinction risk status) plant species at a site of your choice."),
              i18n("To do this, you have the choice of importing a shapefile of your study area or drawing a polygon on a map.")
            )
          ),

          actionButton(
            inputId = ns("start_shp"),
            label = tagList(
              "Identify potentially or likely threatened plant species at a site (Tropical Africa only)",
              ph("arrow-circle-right")
            ),
            class = "btn-outline-primary text-center fs-5 mb-3 d-block",
            width = "100%"
          )
        ),
        tags$p(
          class = "fs-5 mt-5",
          tags$ol(
            tags$ol(
              "Obtain information on the distribution of Gabon's tree species and endangered species in particular."
            )
          )
        ),
        actionButton(
          inputId = ns("start_mod2"),
          label = tagList(
            "Get information on (threatened) tree species in Gabon (not yet available)",
            ph("arrow-circle-right")
          ),
          class = "btn-outline-primary text-center fs-5 mb-3 d-block",
          width = "100%"
        ),
        tags$p(
          class = "fs-5 mt-5",
          tags$ol(
            tags$ol(
              "Obtain the published extinction risk status on the IUCN Red List (if it exists) of one or more species and define",
              "a preliminary extinction risk status according to criterion B (geographic range) from the IUCN Red List based on",
              "the most recent distribution data. This can be done from your own distribution data or by getting distribution data",
              "from different databases (GBIF for all types of taxa or Rainbio and a database of Central African forest plots and transects for plants of tropical Africa."
            )
          )
        ),
        actionButton(
          inputId = ns("start_data"),
          label = tagList(
            "Conduct a preliminary multi-species assessment of extinction risk under Criterion B",
            ph("arrow-circle-right")
          ),
          class = "btn-outline-primary text-center fs-5 mb-3 d-block",
          width = "100%"
        ),

        tags$br(),

        tags$br(),

        tags$p(
          "The development of this application has been funded by the",
          tags$a(
            class = "text-reset",
            "Fondation Franklinia:",
            href = "https://fondationfranklinia.org/",
            target = "_blank"
          )
        ),

        fluidRow(
          column(
            width = 4,
            class = "text-center",
            tags$a(
              href = "https://fondationfranklinia.org/",
              target = "_blank",
              tags$img(
                style = "height: 200px;",
                src = "conrappli/medias/logo_franklinia.jpg"
              )
            )
          )
        ),

        tags$br(),
        tags$br(),

        tags$p("This is a collaborative work between several institutions:"),
        tags$br(),

        fluidRow(
          column(
            width = 4,
            class = "text-center",
            tags$a(
              href="https://en.ird.fr/",
              target="_blank",
              tags$img(
                style = "height: 200px;",
                src = "conrappli/medias/logo_IRD_2016_BLOC_UK_COUL.png"
              )
            )
          ),
          column(
            width = 4,
            class = "text-center",
            tags$a(
              href="http://www.missouribotanicalgarden.org/",
              target="_blank",
              tags$img(
                style = "height: 200px;",
                src = "conrappli/medias/mobot-header-logo.svg"
              )
            )
          ),
          column(
            width = 4,
            class = "text-center",
            tags$a(
              href="https://www.ulb.be/",
              target="_blank",
              tags$img(
                style = "height: 180px;",
                src = "conrappli/medias/logo_ULB.jpg"
              )
            )
          )
        )

      )
    )
  )
}

home_server <- function(id, main_session) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      observeEvent(input$start_shp, {
        bslib::nav_select(
          id = "navbar",
          selected = "data_from_shp", 
          session = main_session
        )
      })

      observeEvent(input$start_data, {
        bslib::nav_select(
          id = "navbar", 
          selected = "data_other_options", 
          session = main_session
        )
      })

    }
  )
}
