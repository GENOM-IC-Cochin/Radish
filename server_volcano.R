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


observeEvent(res(), {
    updateSelectizeInput(
        inputId = "sel_gene",
        choices = as.vector(res()$symbol),
        server = TRUE,
        selected = NULL
    )
})

volc_data <- reactive({
    # collé ici pour ne pas le recalculer,
    #à chaque modif des paramètres de volcano_plot
    res_volc(req(res()))
})

volc_plot <- reactive({
    req(volc_data())
    volcano_plot(
        plot_data = volc_data(),
        titre = input$plot_title,
        colors = c("up" = input$up_col, "down" = input$down_col),
        legends = c("up" = input$up_leg, "down" = input$down_leg, "ns" = input$ns_leg),
        axis_max = c(input$x_max, input$y_max),
        ratio = input$volc_ratio,
        theme = input$theme,
        selected_genes = input$sel_gene
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
