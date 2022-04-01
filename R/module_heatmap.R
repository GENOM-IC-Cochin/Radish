# Heatmap module

HeatmapUI <- function(id) {
  ns <- NS(id)
  
  # Conditional UI
  parameters_tab <- tabsetPanel(
    id = ns("settings"),
    type = "hidden",
    tabPanel(
      "diff",
      value="diff",
      FilterUI(ns("fil"), default = list("pval" = 0.05, "lfc" = 1)),
      numericInput(
        ns("nb_top_gene"),
        "Select the number of top differentially expressed genes (max 2000)",
        value = 100,
        min = 0,
        max = 2000
      ),
    ),
    tabPanel(
      "all",
      value="all",
      GeneSelectUI(ns("gnsel"))
    )
  )
  
  tagList(
    fluidRow(
      column(width = 3,
             box(title = "Settings",
                 status = "orange",
                 width = 12,
                 selectInput(
                   inputId = ns("top_gene"),
                   label = "Choose the Heatmap",
                   choices = c("Top genes from current contrast" = "diff",
                               "Selected genes" = "all")
                 ),
                 checkboxGroupInput(
                   inputId = ns("sel_cond"),
                   label = "Choose the conditions",
                 ),
                 selectInput(
                   inputId = ns("palette"),
                   label = "Choose the color palette of the heatmap",
                   choices = brewer.pal.info %>%
                     filter(category == "div" & colorblind == TRUE) %>%
                     rownames_to_column() %>%
                     pull(rowname),
                   selected = "RdYlBu"
                 ),
                 checkboxInput(
                   ns("show_names"),
                   "Show gene names",
                   value = FALSE
                 ),
                 sliderInput(
                   ns("fontsize"),
                   "Choose the row names fontsize",
                   min = 3,
                   max = 12,
                   value = 10,
                   step = .5
                 ),
                 sliderInput(
                   inputId = ns("ratio"),
                   label = "Choose the plot aspect ratio",
                   value = 1,
                   min = 0.5,
                   max = 2
                 ),
                 
                 parameters_tab,
                 htmlOutput(ns("gene_number")),
                 actionButton(ns("draw"),
                              "Draw heatmap",
                              class = "btn-warning"),
                 actionButton(ns("reset"),
                              "Reset defaults",
                              class = "btn-warning") 
             ),
             box(title = "Information",
                 status = "orange",
                 width = 12,
                 HTML(paste("<p> <strong> Why only 2000 rows maximum? </strong> </p>",
                            "<p> If one goes over 1080, (or 2096 in the case of a 4k screen)",
                            "genes, then there are less pixels than rows in the matrix",
                            "and the results displayed is a sort of mean of multiple rows.",
                            "It is well explained <a href='https://gdevailly.netlify.app/post/plotting-big-matrices-in-r/'>here</a>.",
                            "If you are still willing to obtain such a matrix, please contact the developpers </p>")))
      ),
      column(width = 9,
             box(title = "Heatmap",
                 status = "primary",
                 width = 12,
                 plotOutput(ns("heatmap"),
                            height = "600px"),
                 selectInput(inputId = ns("format"),
                             label = "Format of the downloaded plot :",
                             choices = c("png", "pdf", "svg"),
                             selected = "pdf"),
                 downloadButton(
                   outputId = ns("down"),
                   label = "Download plot"
                 )
             )
      )
    )
  )
}


# Server -----------------------------------------------------------------------

HeatmapServer <- function(
  id,
  counts,
  res,
  config,
  contrast_act,
  sel_genes_table
) {
  stopifnot(is.reactive(counts))
  stopifnot(is.reactive(res))
  stopifnot(is.reactive(config))
  stopifnot(is.reactive(contrast_act))
  stopifnot(is.reactive(sel_genes_table))
  
  moduleServer(id, function(input, output, session) {
    iv <- InputValidator$new()
    iv$add_rule("nb_top_gene", function(value) {
      if (value > 2000) {
        "Maximum : 2000"
      }
    }
    )
    iv$enable()
    
    condition_possibles <- reactive({
      # Conditions from which choice is possible
      req(
        config(),
      )
      condition <- config()$Condition %>% unique()
      if(input$top_gene == "diff") {
        cond_act <- contrast_act() %>%
          strsplit(., split = "_vs_") %>%
          unlist()
        condition[condition %in% cond_act]
      } else {
        condition
      }
    })
    
    
    observeEvent(input$reset, {
      updateNumericInput(inputId = "nb_top_gene", value = 100)
      updateSelectInput(inputId = "top_gene", selected = "diff")
      updateSelectInput(inputId = "palette", selected = "RdYlBu")
      updateCheckboxInput(inputId = "show_names", value = FALSE)
      updateSliderInput(inputId = "fontsize", value = 10)
      updateSliderInput(inputId = "ratio", value = 1)
      updateCheckboxGroupInput(inputId = "sel_cond", selected = req(condition_possibles()))
    })
    
    
    observeEvent(input$top_gene,{
      # Switch between top gene heatmap UI and selected genes heatmap UI
      updateTabsetPanel(
        session = session,
        inputId = "settings",
        selected = input$top_gene
      )}
    )
    
    observeEvent(condition_possibles(),
                 updateCheckboxGroupInput(
                   inputId = "sel_cond",
                   choices = condition_possibles(),
                   # Selected, tout, car si ce Checkbox... est pas affiché (top genes)
                   # il doit alors contenir les deux conditions en contraste
                   selected = condition_possibles(),
                 )
    )
    
    
    output$gene_number <- renderUI({
      req(data())
      HTML(paste("<p>", nrow(data()), "genes are currently displayed on the heatmap </p>"))
    })
    
    
    genes_selected <- GeneSelectServer(
      id = "gnsel",
      src_table = counts,
      sel_genes_table = sel_genes_table
    )
    
    res_filtered <- FilterServer("fil",
                                 res,
                                 list("pval" = 0.05, "lfc" = 1),
                                 reactive(input$reset))$res_filtered
    
    data <- eventReactive(input$draw, {
      req(input$sel_cond,
          config(),
          counts(),
          res_filtered())
      req(iv$is_valid())
      # sélection du nom des échantillons
      # Basé sur les conditions selectionnees
      # Ou par defaut dans le cas top gene
      echantillons <- config() %>%
        filter(Condition %in% input$sel_cond) %>%
        pull(Name)
      
      if(input$top_gene == "diff") {
        req(input$nb_top_gene)
        # Si l'on veut que les plus différentiellement exprimés
        res_filtered() %>%
          filter(sig_expr != "ns") %>%
          slice_min(order_by = padj, n = input$nb_top_gene) %>%
          mutate(name = coalesce(symbol, Row.names)) %>%
          remove_rownames() %>%
          column_to_rownames(var = "name") %>%
          select(all_of(echantillons)) %>%
          as.matrix(., rownames = TRUE)
      } else {
        # Si l'on veut sélectionner à la main
        counts() %>%
          filter(symbol %in% genes_selected$sel_genes_names() |
                   Row.names %in% genes_selected$sel_genes_ids()) %>%
          mutate(name = coalesce(symbol, Row.names)) %>%
          column_to_rownames(var = "name") %>%
          select(all_of(echantillons)) %>%
          as.matrix(., rownames = TRUE)
      }
    })
    
    
    # Met en correspondance les conditions choisies et les échantillons
    # (Pour les couleurs de la heatmap)
    # retour : un df avec la correspondance désirée
    annotation_col <- eventReactive({
      config()
      input$draw
      input$sel_cond
    },{
      config() %>%
        filter(Condition %in% input$sel_cond) %>%
        select(-File) %>%
        column_to_rownames(var = "Name")
    })
    
    
    # A partir des annotations, leur affecte une couleur
    # retour : une liste avec la correspondance selon le format de pheatmap
    # pour le paramètre annotation_colors
    annotation_colors <- eventReactive({
      config()
      annotation_col()
    }, {
      # Au cas où il y ait un jour plusieurs catégories (malade, traitement ...)
      # Aujourd'hui de longueur 1
      ret <- vector(mode = "list", length = ncol(annotation_col()))
      # on y met le nom des échantillons
      names(ret) <- colnames(annotation_col())
      for (name in names(ret)) {
        # quelle condition correspond à ces ech
        quels_cond <- unique(annotation_col()[, name]) 
        # condition_colors : variable globale
        ret[[name]] <- setNames(condition_colors[1:length(quels_cond)], quels_cond)
      }
      ret
    })
    
    cur_plot <- eventReactive(input$draw, {
      req(data(),
          annotation_col(),
          annotation_colors())
      
      pheatmap( # C'est le traducteur de ComplexHeatmap
        name = "z-score",
        mat = data(),
        color = rev(brewer.pal(9, input$palette)),
        cluster_rows = TRUE,
        # No row names if top genes
        show_rownames = input$show_names,
        annotation_col = annotation_col(),
        annotation_colors = annotation_colors(),
        border_color = NA,
        fontsize = 10,
        fontsize_row = input$fontsize,
        scale = "row",
        heatmap_height = unit(5*(input$ratio^0.5), "in"),
        heatmap_width = unit(5/(input$ratio^0.5), "in")
      )
    })
    
    output$heatmap <- renderPlot({
      cur_plot()
    })
    
    
    output$down <- downloadHandler(
      filename = function() {
        paste0("heatmap.", input$format)
      },
      content = function(file) {
        if(input$format == "png") {
          agg_png(file,
                  width = 8/(input$ratio)^0.5,
                  height = 8*(input$ratio)^0.5,
                  units = "in",
                  res = 600)
        } else if (input$format == "pdf") {
          pdf(file,
              width = 7/(input$ratio)^0.5,
              height = 7*(input$ratio)^0.5)
        } else if (input$format == "svg") {
          svglite(file,
                  width = 8/(input$ratio)^0.5,
                  height = 8*(input$ratio)^0.5)
        }
        draw(cur_plot())
        dev.off()
      }
    )
    
  })
}


# Test App ---------------------------------------------------------------------

HeatmapApp <- function() {
  ui <- fluidPage(
    tabsetPanel(type = "tabs",
                tabPanel("Input", InputUI("inp")),
                tabPanel("Heatmap", HeatmapUI("hm"))
    )
  )
  server <- function(input, output, session) {
    list_loaded <- InputServer("inp", reactive("Cond1_vs_Control"))
    HeatmapServer(id = "hm",
                  counts = list_loaded$counts,
                  res = list_loaded$res,
                  config = list_loaded$config,
                  contrast_act = reactive("Cond1_vs_Control"),
                  sel_genes_table = reactive(head(list_loaded$res())))
    
  }
  shinyApp(ui, server)
}