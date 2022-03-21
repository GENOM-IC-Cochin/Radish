observeEvent(res(),{
    updateSliderInput(
        inputId = "x_max",
        max = x_max_abs(donnees = res()),
        value = x_max_abs(donnees = res())
    )
})

observeEvent(res(), {
    updateSliderInput(
        inputId = "y_max",
        max = y_max(donnees = res()),
        value = y_max(donnees = res())
    )
})


observeEvent({
  genes_table()
  volc_data()
  sel_genes_names()
}, {
  updateSelectizeInput(
    inputId = "sel_gene_vp_nm",
    choices = volc_data() %>%
      pull(symbol),
    server = TRUE,
    selected = sel_genes_names()
  )
})

observeEvent({
  genes_table()
  volc_data()
  sel_genes_table()
  }, {
    updateSelectizeInput(
        inputId = "sel_gene_vp_id",
        choices = volc_data() %>%
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
        inputId = "plot_title",
        value = paste(c("Gene expression change in", contr),
                      collapse = " ")
    )
})

volc_data <- reactive({
    # collé ici pour ne pas le recalculer,
    #à chaque modif des paramètres de volcano_plot
    req(input$lfc_cut, input$pval_cut)
    res_volc(req(res()),
             lfc_cutoff = input$lfc_cut,
             pval_cutoff = input$pval_cut)
})

volc_plot <- eventReactive(input$draw_vp, {
    req(volc_data())
    my_volcanoplot(
        plot_data = volc_data(),
        titre = input$plot_title,
        colors = c("up" = input$up_col, "down" = input$down_col),
        legends = c("up" = input$up_leg, "down" = input$down_leg, "ns" = input$ns_leg),
        axis_max = c(input$x_max, input$y_max),
        ratio = input$volc_ratio,
        theme = input$theme,
        selected_genes = c(input$sel_gene_vp_nm, input$sel_gene_vp_id),
        label_size = input$vp_lab_size,
        lfc_cutoff = req(input$lfc_cut),
        pval_cutoff =req(input$pval_cut) 
    )
})

output$volcano_plot <- renderPlot({
    volc_plot()
})

output$down_volc <- downloadHandler(
    filename = function() {
        paste0("volcano_plot.", req(input$volcano_format))
    },
    content = function(file) {
        ggsave(file, plot = req(volc_plot()),
               device = req(input$volcano_format),
               height = (3.5 + 3.5 * input$volc_ratio),
               width = (3.5 + 3.5 / input$volc_ratio),
               dpi = 600)
    }
)
