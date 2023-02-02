
#' @importFrom shiny NS fluidRow column actionButton actionLink
home_ui <- function(id) {
  ns <- NS(id)
  template_ui(
    title = "Welcome to ConR application",
    fluidRow(
      column(
        width = 8, offset = 2,
        tags$p(
          class = "text-center fs-5 text mt-5",
          "This application is designed to perform multi-species estimation of geographical",
          "range parameters for preliminary assessment of conservation status following Criterion A and B of the",
          "International Union for Conservation of Nature (IUCN, see <http://www.iucnredlist.org>)",
          "It is based on the ",
          tags$a(
            "R package ConR",
            href = "https://cran.r-project.org/web/packages/ConR/index.html"
          )
        ),

        tags$br(),

        actionButton(
          inputId = ns("start_shp"),
          label = tagList(
            "Import a shapefile and launch criterion B evaluation",
            ph("arrow-circle-right")
          ),
          class = "btn-outline-primary text-center fs-4 mb-3 d-block",
          width = "100%"
        ),
        actionLink(
          inputId = ns("start_data"),
          label = tagList(
            "Other data import options",
            ph("arrow-circle-right")
          ),
          class = "text-center fs-6 mb-3 d-block",
          width = "100%"
        ),
        
        tags$br(),
        
        tags$p("The development of this application has been funded by the franklinia fondation :"),
        
        fluidRow(
          column(
            width = 4,
            class = "text-center",
            tags$a(
              href="https://fondationfranklinia.org/",
              target="_blank",
              tags$img(
                style = "height: 200px;",
                src = "conrappli/medias/logo_franklinia.jpg"
              )
            )
          )
        ),
        
        tags$br(),
        tags$br(),

        tags$p("This is a collaborative work between several institutions :"),
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
        bslib::nav_select(id = "navbar", selected = "data_from_shp", session = main_session)
      })

      observeEvent(input$start_data, {
        bslib::nav_select(id = "navbar", selected = "data_other_options", session = main_session)
      })

    }
  )
}
