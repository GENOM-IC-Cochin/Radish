# Module for input

# All the objects expected in the input
expected_data <- c(
    "dataMerged",
    "all_results",
    "configuration",
    "contrasteList",
    "rld",
    "txi.rsem"
)

# UI ---------------------------------------------------------------------------
InputUI <- function(id) {
  ns <- NS(id)
  box(
    title = "Result input",
    width = 12,
    fileInput(ns("res_data"),
              "Results"
    ),
    actionButton(ns("demo"), "Load demo data"),
    htmlOutput(ns("check_data"))
  )
}


# Server -----------------------------------------------------------------------
InputServer <- function(id, contrast_act) {
  stopifnot(is.reactive(contrast_act))
  moduleServer(id, function(input, output, session) {
    # rds-sourced list, inpendent of demo or user-loaded data
    data <- reactiveVal()
    
    demo_data <- eventReactive(input$demo, {
      tmp <- readRDS("./data/demo_data.rds")
      validate(need(all(expected_data == names(tmp)), "Missing objects in loaded file"))
      tmp
    })
   
    
    data_loaded <- eventReactive({
      input$res_data
    }, {
      req(input$res_data)
      extension <- tools::file_ext(input$res_data$name)
      tmp <- switch(extension,
                    rds = readRDS(input$res_data$datapath),
                    RDS = readRDS(input$res_data$datapath),
                    validate("Invalid file : Need a .rds file")
      )
      validate(need(all(expected_data == names(tmp)), "Missing objects in loaded file"))
      tmp
    })
    
    #switch data to the correct (last modified) data source
    observeEvent(demo_data(), data(demo_data()))
    observeEvent(data_loaded(), data(data_loaded()))
    
    
    counts <- eventReactive(data(),{
      req(data())
      # Permet de remplacer un nom variable (hgcn_symbol, mgi_symbol) par un nom fixe
      tmp <- data()[["dataMerged"]] %>%
        dplyr::rename("symbol" = dplyr::contains("symbol"))
      # Noms de gènes "" -> NA
      tmp %<>% mutate(across(everything(), na_if, "")) 
      # Avoids duplicated symbol names
      tmp$symbol %<>% tidy_symbols()
      tmp
    })
    
    all_results <- eventReactive(data(),{
      req(data())
      tmp <- vector(mode = "list", length = length(data()[["all_results"]]))
      names(tmp) <- names(data()[["all_results"]]) 
      for (contraste in names(data()[["all_results"]])) {
        tmp[[contraste]] <- data()[["all_results"]][[contraste]] %>% 
          dplyr::rename("symbol" = dplyr::contains("symbol"))
        tmp[[contraste]] %<>%
        # Noms de gènes "" -> NA
          mutate(across(everything(), na_if, "")) 
        tmp[[contraste]]$symbol %<>% 
          tidy_symbols()
      }
      tmp
    })
    
    rld <- eventReactive(data(),{
      req(data())
      data()[["rld"]]
    })
    
    config <- eventReactive(data(), {
      req(data())
      data()[["configuration"]]
    })
    
    contrastes <- eventReactive(data(), {
      req(data())
      data()[["contrasteList"]]
    })
    
    txi.rsem <- eventReactive(data(), {
      req(data())
      data()[["txi.rsem"]]
    })
    
    
    res <- eventReactive({
      contrast_act()
      all_results()
    }, {
      all_results()[[contrast_act()]]
    })
    
    output$check_data <- renderUI({
      req(all_results(),
          counts(),
          config()
      )
      HTML(paste("<br> Read <b>",
                 all_results() %>% length(),
                 "</b> contrasts, and <b>",
                 config() %>% nrow(),
                 "</b> samples"))
      
    })
    list(
      res = res,
      counts = counts,
      contrastes = contrastes,
      rld = rld,
      config = config,
      txi.rsem = txi.rsem,
      all_results_names = reactive(names(all_results()))
    )
  })
}


# Test App ---------------------------------------------------------------------
InputApp <- function() {
  ui <- fluidPage(InputUI("test"))
  
  server <- function(input, output, session) {
    InputServer("test", reactive("Cond1_vs_Control"))
  }
  shinyApp(ui, server)
}