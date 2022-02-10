# Ma plot server

observeEvent({
  genes_table()
  ma_data()
  sel_genes_names()
}, {
  updateSelectizeInput(
    inputId = "sel_gene_ma_nm",
    choices = ma_data() %>%
      pull(symbol),
    server = TRUE,
    selected = sel_genes_names()
  )
})

observeEvent({
  genes_table()
  ma_data()
  sel_genes_table()
  }, {
    updateSelectizeInput(
        inputId = "sel_gene_ma_id",
        choices = ma_data() %>%
            pull(Row.names),
        server = TRUE,
        selected = sel_genes_table() %>%
          filter(is.na(symbol)) %>%
          pull(Row.names)
    )
})

observeEvent(res(), {
    contr <- strsplit(input$contrast_act, "_") %>% unlist()
    updateTextInput(
        inputId = "plot_title_ma",
        value = paste(c("Gene expression change in", contr),
                      collapse = " ")
    )
})

ma_data <- reactive({
    # collé ici pour ne pas le recalculer,
    #à chaque modif des paramètres de ma_plot
    req(input$pval_cut_ma)
    res_ma(req(res()),
             pval_cutoff = input$pval_cut_ma)
})

ma_plot <- eventReactive(input$draw_ma, {
    req(ma_data())
    my_maplot(
        plot_data = ma_data(),
        title = input$plot_title_ma,
        colors = c("up" = input$up_col_ma, "down" = input$down_col_ma),
        legends = c("up" = input$up_leg_ma, "down" = input$down_leg_ma, "ns" = input$ns_leg_ma),
        ratio = input$ratio_ma,
        selected_genes = c(input$sel_gene_ma_nm, input$sel_gene_ma_id),
        theme = input$theme_ma,
        label_size = input$lab_size_ma,
        pval_cutoff = req(input$pval_cut_ma) 
    )
})

output$ma_plot <- renderPlot({
    ma_plot()
})

output$down_ma <- downloadHandler(
    filename = function() {
        paste0("ma_plot.", req(input$format_ma))
    },
    content = function(file) {
        ggsave(file, plot = req(ma_plot()),
               device = req(input$format_ma),
               height = (3.5 + 3.5 * input$ratio_ma),
               width = (3.5 + 3.5 / input$ratio_ma),
               dpi = 600)
    }
)
