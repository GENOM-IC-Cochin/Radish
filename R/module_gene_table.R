# Gene table module

# UI ---------------------------------------------------------------------------
GeneTableUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    bs4Dash::column(3,
           bs4Dash::box(title = "Genes Information",
               status = "secondary",
               width = 12,
               htmlOutput(ns("outlier"))
               ),
           bs4Dash::box(title = "Row selection",
               status = "secondary",
               width = 12,
               selectInput(ns("input_type"),
                           "Selection based on genes' :",
                           c("IDs", "Names")),
               uiOutput(ns("given_genes")),
               htmlOutput(ns("read_items")),
               bs4Dash::actionButton(ns("select_genes"),
                            "Select Genes"),
               br(),
               bs4Dash::actionButton(ns("clear"),
                            "Clear selection"),
               br(),
               bs4Dash::actionButton(ns("clear_input"),
                            "Clear Input")
           ),
           bs4Dash::box(title = "Filtered Download",
               status = "secondary",
               width = 12,
               FilterUI(ns("fil"), list("pval" = 0.05, "lfc" = 1)),
               htmlOutput(ns("sig_genes")),
               downloadButton(ns("down_fil"), "Download the filtered genes")
           ),
    ),
           
    bs4Dash::column(9,
           tabBox(title = "Genes Tables",
                  width = 12,
                  status = "primary",
                  tabPanel(
                    "All genes",
                    htmlOutput(ns("n_selected")),
                    DT::DTOutput(outputId = ns("genes"))
                    ),
                  tabPanel(
                    "Selected genes",
                    DT::DTOutput(ns("genes_selected")),
                    downloadButton(
                      outputId = ns("download_sel_genes"),
                      label = "Download selected genes"
                    ),
                    downloadButton(
                      outputId = ns("download_sel_ids"),
                      label = "Download selected genes' Gene IDs"
                    )
                  )
           )
    )
  )
}


# Server -----------------------------------------------------------------------


GeneTableServer <- function(id,
                            res,
                            config,
                            contrast_act) {
  stopifnot(is.reactive(res))
  stopifnot(is.reactive(config))
  stopifnot(is.reactive(contrast_act))
  moduleServer(id, function(input, output, session){
    
    my_values <- reactiveValues(
      given_genes_rows = NULL,
      items_length = 0 # Number of read IDs/names
    )
    
    
    # Genes Information --------------------------------------------------------
    output$outlier <- renderUI({
      req(res())
      nb_na <- res() %>% 
        filter(if_any(padj, ~ is.na(.x))) %>%
        nrow()
      HTML(paste("<p> <b>", 
                 nb_na,
                 "</b>",
                 "genes have their <i> p-values </i> set to NA, as one sample has an extreme count outlier, or the gene has not passed independent filtering.",
                 "</p>"))
    })
    
    # Row Selection ------------------------------------------------------------
    
    output$given_genes <- renderUI({
      req(input$input_type)
      if(input$input_type == "IDs") {
        fileInput(session$ns("identifiers"), "Gene IDs for selection", accept = "text/plain")
      } else {
        fileInput(session$ns("identifiers"), "Gene names for selection", accept = "text/plain")
      }
    })
    
    
    # Matches the genes given and the rows in the table
    observeEvent({
      input$identifiers
      genes_table()
    },
    {
      req(input$input_type,
          input$identifiers)
      if(input$input_type == "IDs") {
        extension <- tools::file_ext(input$identifiers$name)
        validate(need(extension == "txt", "Please upload a plain text (txt) file"))
        
        gene_ids <- scan(file = input$identifiers$datapath,
                         what = character())
        my_values$given_genes_rows <- which(genes_table()$Row.names %in% gene_ids)
        
      } else {
        extension <- tools::file_ext(input$identifiers$name)
        validate(need(extension == "txt", "Please upload a plain text (txt) file"))
        
        gene_names <- scan(file = input$identifiers$datapath,
                           what = character())
        gg_reg <- paste(gene_names, collapse = "|")
        my_values$given_genes_rows <- grep(gg_reg, 
                                           genes_table()$symbol,
                                           ignore.case = TRUE)
      }
    }) 
    
    
    observeEvent(input$clear_input,{
      my_values$given_genes_rows <- NULL
    })
    
    
    # Set length of read_items
    observeEvent({
      input$identifiers
    }, {
      my_values$items_length <-scan(input$identifiers$datapath,
           what = character()) %>% length
    })
    
    # Reset length of read_items
    observeEvent({
      input$clear_input
      input$input_type
    },{
      my_values$items_length <- 0
    })
    
    output$read_items <- renderUI({
      HTML("<p> <b>",
           my_values$items_length,
           "</b>",
           "items were read.",
           "</p>")
    })
    
    
    # Selective Download -------------------------------------------------------
    
    filter_res <- FilterServer("fil",
                               res,
                               list("pval" = 0.05, "lfc" = 1),
                               reactive(0))# no reset button here
    
    
    output$sig_genes <- renderUI({
      req(res(),
          filter_res$pval(),
          filter_res$lfc())
      n_sig <- res() %>%
        dplyr::filter(padj < filter_res$pval(), abs(log2FoldChange) > filter_res$lfc()) %>%
        nrow()
      HTML(paste("<p> <b>", n_sig, "</b>",
                 "genes are significantly differentially expressed",
                 "at an adjusted <i> pvalue </i> of",
                 filter_res$pval(), "and a minimum log2(Foldchange) of",
                 filter_res$lfc(),
                 "</p>"))
    })
    
    output$down_fil <- downloadHandler(
      filename = function() {
        paste0(contrast_act(),
               "_",
               filter_res$pval(),
               "_",
               filter_res$lfc(),
               ".csv"
               )
      },
      content = function(file) {
        filter_res$res_filtered() %>%
          filter(sig_expr != "ns") %>%
          select(-sig_expr) %>%
          write.csv(., file)
      }
    ) 
    
    
    # Tables -------------------------------------------------------------------
    
    genes_table <- eventReactive({
      res()
    },{
      res() %>%
        # Significant digits (DT function somewhere?)
        mutate(dplyr::across(where(is.numeric), signif, 3))
    })
    
    
    sel_genes_table <- eventReactive({
      genes_table()
      # Fourni par DT
      input$genes_rows_selected
    },{
      # res, pour avoir tous les chiffres significatifs?
        genes_rows <- input$genes_rows_selected
        res()[genes_rows, ] %>%
          select(Row.names) %>%
          inner_join(res(), by = "Row.names")
    },
    ignoreNULL = FALSE, # in order not to prevent sel_genes_table to return to NULL if the contrast changes
    label = "SEL_GENES"
    )
    
    
    cols_to_hide <- eventReactive(res(),{
      # - 1 because JS indices start at 0
      which(!(colnames(res()) %in% base_table_columns)) - 1
    })
    
    
    # Row selection in the DT table
    proxy <- DT::dataTableProxy("genes")
    
    
    observeEvent(input$select_genes, {
      proxy %>% DT::selectRows(my_values$given_genes_rows)
    })
    
    
    observeEvent(input$clear, {
      proxy %>% DT::selectRows(NULL)
    })
    
    
    # To reset selection if contrast_act() changes
    observeEvent(contrast_act(), {
      proxy %>% DT::selectRows(NULL)
    })
    
    
    output$genes <- DT::renderDT(
      expr = {
        genes_table()
      },
      rownames = FALSE,
      # filter = "top", # ne permet pas de sélectionner abs(x) > 1
      class = "cell-border stripe hover order-colum",
      colnames = c("Gene ID" = "Row.names",
                   "Adjusted p-value" = "padj",
                   "Mean of normalised counts, all samples" = "baseMean",
                   "log2(FoldChange)" = "log2FoldChange",
                   "Gene name" = "symbol",
                   "Gene description" = "description"),
      extensions = "Buttons",
      options = list(scrollX = TRUE,
                     dom = "Bfrtip",
                     columnDefs = list(
                       list(targets = c(0, 1, 2, 6, 8), className = "noVis"),
                       list(targets = cols_to_hide(), visible = FALSE)
                     ),
                     buttons = list(
                       list(extend = 'colvis', columns = I(':not(.noVis)'))
                     )),
      selection = list(target = "row")
    )
    
    
    output$n_selected <- renderUI({
      HTML(paste("<p> <b>", length(input$genes_rows_selected), "</b>",
                 "rows are currently selected. </p>"))
    })
    
    
    output$genes_selected <- DT::renderDT(
      expr = {
        req(sel_genes_table())
        sel_genes_table() %>%
          mutate(dplyr::across(where(is.numeric), signif, 3))
      },
      rownames = FALSE,
      class = "cell-border stripe hover order-colum",
      colnames = c("Gene ID" = "Row.names",
                   "Adjusted p-value" = "padj",
                   "Mean of normalised counts, all samples" = "baseMean",
                   "log2(FoldChange)" = "log2FoldChange",
                   "Gene name" = "symbol",
                   "Gene description" = "description"),
      extensions = "Buttons",
      options = list(scrollX = TRUE,
                     dom = "Bfrtip",
                     columnDefs = list(
                       list(targets = c(0, 1, 2, 6, 8), className = "noVis"),
                       list(targets = cols_to_hide(), visible = FALSE)
                     ),
                     buttons = list(
                       list(extend = 'colvis', columns = I(':not(.noVis)'))
                     )),
      selection = "none"
    )
    
    
    output$download_sel_genes <- downloadHandler(
      filename = function() {
        paste("selected_genes", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(sel_genes_table(), file)
      }
    )
    
    
    output$download_sel_ids <- downloadHandler(
      filename = function() {
        paste("selected_genes_ids", ".txt", sep = "")
      },
      content = function(file) {
        write(sel_genes_table() %>% pull(Row.names), file)
      }
    )
    
    
    # Module output
    sel_genes_table
    
  })
}


# Test App ---------------------------------------------------------------------
GeneTableApp <- function() {
  ui <- fluidPage(
    bs4Dash::tabsetPanel(type = "tabs",
                tabPanel("Input", InputUI("inp")),
                tabPanel("Table", GeneTableUI("tab"))
    )
  )
  
  server <- function(input, output, session) {
    list_loaded <- InputServer("inp", reactive("1"))
    GeneTableServer(
      id = "tab",
      res = list_loaded$res,
      config = list_loaded$config,
      contrast_act = reactive("1")
    )
  }
  shinyApp(ui, server)
}
