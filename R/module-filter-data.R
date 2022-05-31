
filter_ui <- function(id,
                      show_nrow = TRUE,
                      max_height = NULL) {
  ns <- NS(id)
  
  datamods::filter_data_ui(id, show_nrow, max_height)
  
  }



filter_server <- function(id,
                          data = reactive(NULL),
                          vars = reactive(NULL),
                          name = reactive("data"),
                          drop_ids = TRUE,
                          widget_char = c("select", "picker"),
                          widget_num = c("slider", "range"),
                          widget_date = c("slider", "range"),
                          label_na = "NA",
                          value_na = TRUE) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      datamods::filter_data_server(id,
                         data,
                         vars,
                         name,
                         drop_ids,
                         widget_char,
                         widget_num,
                         widget_date,
                         label_na,
                         value_na)
        
      }
    )
  }
