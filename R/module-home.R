

home_ui <- function(id) {
  ns <- NS(id)
  template_ui(
    title = "Welcome to ConR application",
    fluidRow(
      column(
        width = 8, offset = 2,
        tags$p(
          class = "text-center fs-5 text mt-5",
          "This explication is designed to perform multi-species estimation of geographical",
          "range parameters for preliminary assessment of conservation status following Criterion B of the", 
          "International Union for Conservation of Nature (IUCN, see <http://www.iucnredlist.org>)",
          "through", 
          tags$a(
            "R package ConR",
            href = "https://cran.r-project.org/web/packages/ConR/index.html"
          )
        ),
        
        tags$br(),
        
        actionButton(
          inputId = ns("start"),
          label = tagList(
            "Get started",
            ph("arrow-circle-right")
          ),
          class = "btn-outline-primary text-center fs-4 my-5 d-block",
          width = "100%",
          onclick = "$('.navbar').find('a[data-value=\"data\"]').click();"
        ),
        
        tags$br(),
        
        tags$p("A collaborative work between:"),
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
              href="https://www.plantentuinmeise.be/",
              target="_blank",
              tags$img(
                style = "height: 200px;",
                src = "conrappli/medias/th.webp"
              )
            )
          )
        )
        
      )
    )
  )
}

home_server <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
    }
  )
}
