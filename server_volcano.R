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


observeEvent(volc_data(), {
    updateSelectizeInput(
        inputId = "sel_gene_vp_nm",
        choices = volc_data() %>%
            filter(sig_expr != "ns") %>%
            pull(symbol),
        server = TRUE,
        selected = NULL
    )
})

observeEvent(volc_data(), {
    updateSelectizeInput(
        inputId = "sel_gene_vp_id",
        choices = volc_data() %>%
            filter(sig_expr != "ns") %>%
            pull(Row.names),
        server = TRUE,
        selected = NULL
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
    volcano_plot(
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
