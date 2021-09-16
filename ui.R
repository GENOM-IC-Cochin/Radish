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
                        textInput(
                            inputId = "up_leg",
                            label = "Choose the upregulated legend name",
                            value = "up"
                        ),
                        textInput(
                            inputId = "down_leg",
                            label = "Choose the downregulated legend name",
                            value = "down"
                        ),
                        textInput(
                            inputId = "ns_leg",
                            label = "Choose the nonsignificant legend name",
                            value = "ns"
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
                            label = "Select which significant genes to highlight :",
                            choices = NULL,
                            multiple = TRUE
                        ),
                        selectInput(
                            inputId = "theme",
                            label = "Choose the theme for the plot",
                            choices = themes_gg,
                            selected = "Classic"
                        ),
                        sliderInput(
                            inputId = "volc_ratio",
                            label = "Choose the plot aspect ratio",
                            value = 1,
                            min = 0.5,
                            max = 2
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
            tabItem(tabName = "tabl_gene",
                    sidebarLayout(
                        sidebarPanel(
                            numericInput(
                                inputId = "pval_cutoff",
                                label = "Enter the maximum p-value :",
                                value = 0.05,
                                min = 0,
                                max = 1,
                                step = .05
                            ),
                            numericInput(
                                inputId = "lfc_cutoff",
                                label = "Enter the minimum (absolute) logFold2Change :",
                                value = 1,
                                min = 0
                            ),
                            checkboxGroupInput(
                                inputId = "genes_columns",
                                label = "Choose the genes column to display :"
                            )
                        ),
                        mainPanel(
                            DT::dataTableOutput(outputId = "genes")
                        )
                    )
            )
        )
    )
)
