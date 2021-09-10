server <- function(input, output, session) {

  res <- eventReactive(c(
    input$inp_res_table,
    input$auto_inp),
    {
      if(input$auto_inp) {
        tmp <- vroom::vroom("./NGS21-023_Resultats/Analyses_supervisees/NGS21-023_ensemblGCRm38r101_deseq2_results_contrast_High_vs_Controle.tsv",
                            delim = "\t")
        # Find the symbol column
        nom_col <- colnames(tmp)
        col_symb_test <- str_detect(nom_col, "symbol")
        shinyFeedback::feedbackDanger("inp_res_table",
                                      !any(col_symb_test),
                                      "The table does not contain a symbol (gene name) column")
        req(any(col_symb_test))
        col_symbol <- which(col_symb_test)
        colnames(tmp)[col_symbol] <- "symbol"
        return(tmp)
      }
      req(input$inp_res_table)

      extension <- tools::file_ext(input$inp_res_table$name)
      tmp <- switch(extension,
                    csv = vroom::vroom(input$inp_res_table$datapath, delim = ","),
                    tsv = vroom::vroom(input$inp_res_table$datapath, delim = "\t"),
                    validate("Invalid file : Need a .csv or .tsv file")
      )

      # Vérifier l'intégrité des données
      val_cols <- c("baseMean", "log2FoldChange", "lfcSE", "stat", "pvalue", "padj") %in% colnames(tmp)
      shinyFeedback::feedbackDanger("inp_res_table",
                                    !val_cols,
                                    "The table does not contain minimum column names")
      req(all(val_cols))
      
      # Find the symbol column
      nom_col <- colnames(tmp)
      col_symb_test <- str_detect(nom_col, "symbol")
      shinyFeedback::feedbackDanger("inp_res_table",
                                    !any(col_symb_test),
                                    "The table does not contain a symbol (gene name) column")
      req(any(col_symb_test))
      col_symbol <- which(col_symb_test)
      colnames(tmp)[col_symbol] <- "symbol"
      
      tmp
    })

  counts <- eventReactive(c(
    input$inp_compt_table,
    input$auto_inp),
    {
      if(input$auto_inp) {
        tmp <- vroom::vroom("./NGS21-023_Resultats/Donnees_Normalisees/NGS21-023_ensemblGCRm38r101_deseq2_NormalizedMatrix.tsv",
                            delim = "\t")
      } else {
        req(input$inp_compt_table)

        extension <- tools::file_ext(input$inp_compt_table$name)
        tmp <- switch(extension,
                      csv = vroom::vroom(input$inp_compt_table$datapath, delim = ","),
                      tsv = vroom::vroom(input$inp_compt_table$datapath, delim = "\t"),
                      validate("Invalid file : Need a .csv or .tsv file")
        )

        # Vérifier la vraisemblance des données
        shinyFeedback::feedbackWarning("inp_comp_table",
                                       ncol(tmp) <= 5,
                                       "The counts table has less than 5 samples")
        shinyFeedback::feedbackWarning("inp_compt_table",
                                       nrow(tmp) <= 10000,
                                       "The counts table has less than 10000 annotations")

      }
      tmp
    })


  output$size_res <- renderUI({
    req(res())
    HTML(paste0("<p> Number of columns read : ",
                ncol(res()),
                "</p>",
                "<br>",
                "Number of rows read :",
                nrow(res()),
                "</p>"))
  })

  output$size_count <- renderUI({
    req(counts())
    HTML(paste0("<p> Number of columns read : ",
                ncol(counts()),
                "</p>",
                "<br>",
                "Number of rows read :",
                nrow(counts()),
                "</p>"))
  })
  
  x_max_abs <- eventReactive(res(),{
     min_x <- res() %>%
      as.data.frame() %>%
      pull(log2FoldChange) %>%
      min() %>%
      floor()
    max_x <- res() %>%
      as.data.frame() %>%
      pull(log2FoldChange) %>%
      max() %>%
      ceiling()
    # maximum value of the x axis
    max(c(abs(min_x), abs(max_x)))
  })
  
  y_max_abs <-eventReactive(res(), {
    res() %>%
      as.data.frame() %>%
      na.omit() %>%
      transmute(log_padj = -log10(padj)) %>%
      max() %>%
      ceiling()
  }) 
  
  observe({
  updateSliderInput(inputId = "x_max",
                    max = x_max_abs(),
                    value = x_max_abs())
  })
  
  observe({
  updateSliderInput(inputId = "y_max",
                    max = y_max_abs(),
                    value = y_max_abs())
  })
  
  
  observeEvent(res(), {
    updateSelectizeInput(
      inputId = "sel_gene", 
      choices = as.vector(res()$symbol),
      server = TRUE,
      selected = NULL
    )
  })
  
  res_to_plot <- eventReactive(res(), {
    res() %>%
      as.data.frame() %>%
      mutate(sig_expr = factor(case_when(log2FoldChange >= 1 & padj <= 0.05 ~ "up",
                                         log2FoldChange <= -1 & padj <= 0.05 ~ "down",
                                         TRUE ~ "ns"))) %>%
      mutate(sig_expr = relevel(sig_expr, "up"))
  })
  
  volcano_plot <- reactive({
    # Choice of colors/transparency for up/down
    cols <- c("up" = input$up_col, "down" = input$down_col, "ns" = "black")
    alphas <- c("up" = 1, "down" = 1, "ns" = 0.3)

    tmp <-  res_to_plot() %>%
      ggplot(aes(x = log2FoldChange,
                 y = -log10(padj),
                 alpha = sig_expr,
                 fill = sig_expr)) +
      geom_point(color = "black",
                 na.rm = TRUE,
                 shape = 21,
                 stroke = 0.1) +
      geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
      geom_vline(xintercept = c(-1, 1), linetype = "dashed") +
      scale_fill_manual(values = cols) +
      scale_alpha_manual(values = alphas, guide = "none") +
      labs(title = input$plot_title,
           x = "Log2 Fold Change",
           y = "-Log10(Adjusted p-value)",
           fill = "Expression\nChange") +
      theme_bw() +
      theme(plot.title = element_text(face = "bold", size = 20, hjust = 0.5))
    if (input$y_max != y_max_abs()) {
      tmp <- tmp + scale_y_continuous(limits = c(NA, input$y_max), oob = scales::squish) +
        geom_hline(yintercept = input$y_max, linetype = "dotted")
    }
    if(input$x_max != x_max_abs()) {
      tmp <- tmp + scale_x_continuous(limits = c(-input$x_max, input$x_max),
                                      oob = scales::squish) +
        geom_vline(xintercept = c(-input$x_max, input$x_max),
                   linetype = "dotted")
    }
    if(!is.null(input$sel_gene)) {
      # Choice of genes, do not show labels of non significant genes
      genes_to_highlight <- which(res_to_plot()$symbol %in% input$sel_gene &
                                    res_to_plot()$sig_expr != "ns")
      tmp <- tmp + geom_label_repel(
        data = res_to_plot()[genes_to_highlight, ],
        aes(label = symbol),
        color = "black",
        fill = "white",
        min.segment.length = 0,
        show.legend = FALSE
      )
    }
    tmp
  })

  output$volcano_plot <- renderPlot({
    req(res())
    volcano_plot()
  })

  output$down_volc <- downloadHandler(
    filename = function() {paste0("volcano_plot.", req(input$volcano_format))},
    content = function(file) {
      ggsave(file, plot = req(volcano_plot()), device = req(input$volcano_format))
    }
  )
}