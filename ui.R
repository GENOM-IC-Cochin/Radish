ui <- dashboardPage(
    dashboardHeader(title = "Plotting tool"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Inputs", tabName = "inp"),
            menuItem("Volcano plot", tabName = "volcano"),
            menuItem("Interactive table", tabName = "tabl_gene")
        )
    ),
    dashboardBody(
        shinyFeedback::useShinyFeedback(),
        tabItems(
            tabItem(
                tabName = "inp",
                checkboxInput(
                    inputId = "auto_inp",
                    label = "Auto-detection of input files (debug feature)",
                    value = TRUE
                ),
                conditionalPanel(
                    condition = "input.auto_inp == false",
                    fluidRow(
                        column(
                            width = 6,
                            p("Please enter the result table (csv or tsv format: NO .xls)"),
                            br(),
                            fileInput("inp_res_table",
                                NULL,
                                accept = c(".csv", ".tsv")
                            ),
                            hr(),
                            htmlOutput("size_res")
                        ),
                        column(
                            width = 6,
                            p("Please enter the counts table (csv or tsv format: NO .xls)"),
                            br(),
                            fileInput("inp_compt_table",
                                NULL,
                                accept = c(".csv", ".tsv")
                            ),
                            hr(),
                            htmlOutput("size_count")
                        )
                    )
                )
            ),
            tabItem(
                tabName = "volcano",
                sidebarLayout(
                    sidebarPanel(
                        h3("Settings"),
                        sliderInput(
                            inputId = "x_max",
                            label = "Maximum value of the x axis",
                            min = 0,
                            max = 100,
                            value = 10
                        ),
                        sliderInput(
                            inputId = "y_max",
                            label = "Maximum value of the y axis",
                            min = 0,
                            max = 100,
                            value = 10
                        ),
                        textInput(
                            inputId = "plot_title",
                            label = "Title of the plot",
                            value = "Gene expression change"
                        ),
                        colourInput(
                            inputId = "up_col",
                            label = "Choose the color of the upregulated genes",
                            value = "#fe7f00"
                        ),
                        colourInput(
                            inputId = "down_col",
                            label = "Choose the color of the downregulated genes",
                            value = "#007ffe"
                        ),
                        selectizeInput(
                            inputId = "sel_gene",
                            label = "Select which genes to highlight :",
                            choices = NULL,
                            multiple = TRUE
                        ),
                        selectInput(
                            inputId = "volcano_format",
                            label = "Format of the dowloaded plot",
                            choices = c("svg", "jpg", "png"),
                            selected = "png"
                        ),
                        downloadButton(
                            outputId = "down_volc",
                            label = "Download plot"
                        )
                    ),
                    mainPanel(
                        plotOutput(outputId = "volcano_plot")
                    )
                )
            ),
            tabItem(tabName = "tabl_gene")
        )
    )
)
