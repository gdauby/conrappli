function(input, output, session) {

  # stop the serveur in the end of the session
  session$onSessionEnded(function() {
    stopApp()
  })

  observe({
    # hide few menu at the begining
    hideMenuItem("tab_TAXO")
    hideMenuItem("tab_MAP")
    hideMenuItem("tab_EVAL")
  })

  original.dataset <- reactiveValues(df = NULL)


  # load dataset ------------------------------------------------------------

  # inv <- reactiveVal(label = "data frame")
  observeEvent(ignoreInit = T, {
    input$file_DATASET
    # input$num_skip_line
    # input$rad_decimal
  }, {
    # file importation
    if (!is.null(input$file_DATASET)) {

      dataset <-
        readr::read_csv(input$file_DATASET$datapath,
                      locale = readr::locale(encoding = stringi::stri_enc_get()))
      # inv(fread(
      #   file = input$file_DATASET$datapath,
      #   skip = ifelse(is.na(input$num_skip_line) || input$num_skip_line == 0, "__auto__", input$num_skip_line),
      #   data.table = F,
      #   dec = input$rad_decimal
      # ))

      # show the box
      showElement("box_DATASET")
      showElement("box_FIELDS")

      # show the content
      # output$table_DATASET <- renderDataTable(inv())

      int_num_col <- names(dataset)[ sapply(dataset, class) %in% c("integer", "numeric", "double") ]

      # fill the selector with the column name
      for (id in c("sel_LONG", "sel_LAT")) {
        updateSelectInput(session, id, choices = c("<unselected>", int_num_col))
      }
      #
      char_col <- names(dataset)[ sapply(dataset, class) %in% "character" ]
      #
      for (id in c("sel_taxa", "sel_species_epiteth", "sel_genus")) {
        updateSelectInput(session, id, choices = c("<unselected>", char_col))
      }

      original.dataset$df <- dataset
      #
      # name <- names(inv())
      # updateSelectInput(session, "sel_PLOT", choices = c("<unselected>", name))
    }
  })


  # error when the user click on the button on the first page
  observeEvent(input$btn_DATASET_LOADED, {
    error <- F

    if (input$sel_taxa == "<unselected>" & input$sel_species_epiteth == "<unselected>" & input$sel_genus == "<unselected>") { # if no taxa
      error <- T
      shinyalert("Oops!", "Select taxa or both species epithet and genus", type = "error")
    }else{
      if(input$sel_taxa == "<unselected>" & (any(c(input$sel_species_epiteth, input$sel_genus)== "<unselected>"))) {
        error <- T
        shinyalert("Oops!", "Select both genus and species epithet", type = "error")
      }
    }

    if((any(c(input$sel_LONG, input$sel_LAT)== "<unselected>"))) {
      error <- T
      shinyalert("Oops!", "Select both longitude and latitude", type = "error")
    }

    if (!error) {
      showMenuItem("tab_MAP")
      updateTabItems(session, "tab_MAP")

      newData <- original.dataset$df

      newData <-
        newData %>%
        mutate(taxa = paste0(!!rlang::sym(input$sel_genus)," " , !!rlang::sym(input$sel_species_epiteth)))

      original.dataset$df <- newData

    }
  })



  }
