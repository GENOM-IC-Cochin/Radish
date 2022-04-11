# Gene table module

# UI ---------------------------------------------------------------------------
GeneTableUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(3,
           box(title = "Genes Information",
               status = "warning",
               width = 12,
               htmlOutput(ns("outlier")),
               htmlOutput(ns("sig_genes"))),
           box(title = "Row selection",
               status = "warning",
               width = 12,
               column(6,
                      fileInput(ns("given_genes_names"),
                                "Genes names for selection",
                                accept = "text/plain")
               ),
               column(6,
                      fileInput(ns("given_genes_ids"),
                                "Genes IDs for selection",
                                accept = "text/plain")
               ),
               htmlOutput(ns("read_items")),
               actionButton(ns("select_genes"),
                            "Select Genes"),
               actionButton(ns("clear"),
                            "Clear selection")
           ),
           box(title = "Selective Download",
               status = "warning",
               width = 12,
               FilterUI(ns("fil"), list("pval" = 0.05, "lfc" = 1)),
               downloadButton(ns("down_fil"), "Download the filtered genes")
           ),
    ),
           
    column(9,
           tabBox(title = "Genes Tables",
                  width = 12,
                  tabPanel(
                    "All genes",
                    htmlOutput(ns("n_selected")),
                    DTOutput(outputId = ns("genes"))
                    ),
                  tabPanel(
                    "Selected genes",
                    DTOutput(ns("genes_selected")),
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
                            counts,
                            res,
                            config,
                            contrastes,
                            contrast_act) {
  stopifnot(is.reactive(counts))
  stopifnot(is.reactive(res))
  stopifnot(is.reactive(config))
  stopifnot(is.reactive(contrastes))
  stopifnot(is.reactive(contrast_act))
  moduleServer(id, function(input, output, session){
    
    my_values <- reactiveValues(
      given_genes_rows = NULL
    )
    output$outlier <- renderUI({
      req(res())
      nb_na <- res() %>% 
        filter(if_any(padj, ~ is.na(.x))) %>%
        nrow()
      HTML(paste("<b>", 
                 nb_na,
                 "</b>",
                 "genes had their <i> p-values </i> set to NA, as they are deemed outliers.",
                 "<br>",
                 "<br>"))
    })
    
    output$sig_genes <- renderUI({
      req(res())
      n_sig <- res() %>%
        filter(padj < 0.05, abs(log2FoldChange) > 1) %>%
        nrow()
      HTML(paste("<b>", n_sig, "</b>",
                 "genes are significantly differentially expressed",
                 "at an adjusted <i> pvalue </i> of 0.05 and a minimum log2(Foldchange) of 1.",
                 "<br>"))
    })
    
    genes_table <- eventReactive({
      res()
    },{
      res() %>%
        # Significant digits
        mutate(dplyr::across(where(is.numeric), signif, 3))
    })
    
    filter_res <- FilterServer("fil",
                               res,
                               list("pval" = 0.05, "lfc" = 1),
                               reactive(0))# no reset button here
    
    
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
    
    
    sel_genes_table <- eventReactive({
      genes_table()
      # Fourni par DT
      input$genes_rows_selected
    },{
      # res, pour avoir tous les chiffres significatifs
        genes_rows <- input$genes_rows_selected
        genes_table()[genes_rows, ] %>%
          select(Row.names) %>%
          inner_join(res(), by = "Row.names")
    },
    ignoreNULL = FALSE # in order not to prevent sel_genes_table to return to NULL if the contrast changes
    )
    
    
    output$read_items <- renderUI({
      names <- ids <- NULL
      if(!is.null(input$given_genes_names)) {
        names <- scan(input$given_genes_names$datapath,
                      what = character())
      }
      if(!is.null(input$given_genes_ids)) {
        ids <- scan(input$given_genes_ids$datapath,
                    what = character())
      }
      HTML("<b>",
           length(ids) + length(names),
           "</b>",
           "items were read.",
           "<br>",
           "<br>")
    })
    
    
    # Matches the genes given and the rows in the table
    observeEvent({
      # either one or the other
      # both NULL stops observeEvent
      c(input$given_genes_names, input$given_genes_ids)
      genes_table()
    },
    {
      # give priority to ids
      if(!is.null(input$given_genes_ids)) {
        extension <- tools::file_ext(input$given_genes_ids$name)
        validate(need(extension == "txt", "Please upload a plain text (txt) file"))
        
        gene_ids <- scan(file = input$given_genes_ids$datapath,
                         what = character())
        my_values$given_genes_rows <- which(genes_table()$Row.names %in% gene_ids)
        
      }
      if(!is.null(input$given_genes_names)) {
        extension <- tools::file_ext(input$given_genes_names$name)
        validate(need(extension == "txt", "Please upload a plain text (txt) file"))
        
        gene_names <- scan(file = input$given_genes_names$datapath,
                           what = character())
        gg_reg <- paste(gene_names, collapse = "|")
        my_values$given_genes_rows <- c(my_values$given_genes_rows,
                                        grep(gg_reg, 
                                             genes_table()$symbol,
                                             ignore.case = TRUE)) %>% unique()
      }
    }
    )
    
    cols_to_hide <- eventReactive(res(),{
      # - 1 because JS indices start at 0
      which(!(colnames(res()) %in% base_table_columns)) - 1
    })
    
    # Row selection in the DT table
    proxy <- dataTableProxy("genes")
    
    observeEvent(input$select_genes, {
      proxy %>% selectRows(my_values$given_genes_rows)
    })
    
    observeEvent(input$clear, {
      proxy %>% selectRows(NULL)
    })
    
    
    # To reset selection if res() changes
    observeEvent(res(), {
      proxy %>% selectRows(NULL)
    })
    
    output$genes <- renderDT(
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
      HTML(paste("<b>", length(input$genes_rows_selected), "</b>",
                 "rows are currently selected. <br> <br>"))
    })
    
    output$genes_selected <- renderDT(
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
    
    sel_genes_table
    
  })
}


# Test App ---------------------------------------------------------------------
GeneTableApp <- function() {
  ui <- fluidPage(
    tabsetPanel(type = "tabs",
                tabPanel("Input", InputUI("inp")),
                tabPanel("Table", GeneTableUI("tab"))
    )
  )
  
  server <- function(input, output, session) {
    list_loaded <- InputServer("inp", reactive("Cond1_vs_Control"))
    GeneTableServer(
      id = "tab",
      counts = list_loaded$counts,
      res = list_loaded$res,
      config = list_loaded$config,
      contrastes = list_loaded$contrastes,
      contrast_act = reactive("Cond1_vs_Control")
    )
  }
  shinyApp(ui, server)
}