library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(statease)

#UI
ui <- dashboardPage(
  skin = "blue",

  # Header
  dashboardHeader(
    title = tags$span(
      tags$img(src = "logo.png", height = "40px"),
      "statease"
    ),
    titleWidth = 250
  ),

  # Sidebar
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "sidebar",
      menuItem("Home",
               tabName = "home",
               icon = icon("house")),
      menuItem("Descriptive Statistics",
               tabName = "describe",
               icon = icon("table")),
      menuItem("T-Tests",
               tabName = "ttest",
               icon = icon("not-equal")),
      menuItem("ANOVA",
               tabName = "anova",
               icon = icon("chart-bar")),
      menuItem("Two-Way ANOVA",
               tabName = "anova2",
               icon = icon("chart-bar")),
      menuItem("MANOVA",
               tabName = "manova",
               icon = icon("layer-group")),
      menuItem("Chi-Square",
               tabName = "chisq",
               icon = icon("table-cells")),
      menuItem("Correlation",
               tabName = "correlation",
               icon = icon("circle-nodes")),
      menuItem("Regression",
               tabName = "regression",
               icon = icon("chart-line")),
      menuItem("Non-Parametric",
               tabName = "nonparam",
               icon = icon("arrows-up-down")),
      menuItem("P-Value Interpreter",
               tabName = "pvalue",
               icon = icon("magnifying-glass")),
      menuItem("Auto Analyze",
               tabName = "analyze",
               icon = icon("wand-magic-sparkles")),
      hr(),
      menuItem("About",
               tabName = "about",
               icon = icon("circle-info"))
    )
  ),

  # Body
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .skin-blue .main-header .logo {
          background-color: #2C3E7A;
        }
        .skin-blue .main-header .navbar {
          background-color: #2C3E7A;
        }
        .skin-blue .main-sidebar {
          background-color: #1a2550;
        }
        .result-box {
          background-color: #f8f9fa;
          border-left: 4px solid #2C3E7A;
          padding: 15px;
          border-radius: 5px;
          font-family: monospace;
          white-space: pre-wrap;
          font-size: 13px;
        }
        .btn-primary {
          background-color: #2C3E7A;
          border-color: #2C3E7A;
          color: #FFFFFF !important;
        }
        .btn-primary:hover {
          background-color: #4A90D9;
          border-color: #4A90D9;
          color: #FFFFFF !important;
        }
        .btn-primary:focus,
        .btn-primary:active {
          background-color: #2C3E7A;
          border-color: #2C3E7A;
          color: #FFFFFF !important;
        }
      "))
    ),

    tabItems(

      #HOME
      tabItem(
        tabName = "home",
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "Welcome to statease",
            h3("Statistical Analysis Made Simple"),
            p("statease is a free, open-source R package that runs
              statistical tests and automatically interprets results
              in plain English, no coding required"),
            hr(),
            h4("What can statease do?"),
            fluidRow(
              valueBox(
                value = "10+",
                subtitle = "Statistical Tests",
                icon = icon("chart-bar"),
                color = "blue",
                width = 3
              ),
              valueBox(
                value = "100%",
                subtitle = "Free & Open Source",
                icon = icon("unlock"),
                color = "green",
                width = 3
              ),
              valueBox(
                value = "Plain",
                subtitle = "English Interpretation",
                icon = icon("comment"),
                color = "purple",
                width = 3
              ),
              valueBox(
                value = "Auto",
                subtitle = "Test Detection",
                icon = icon("wand-magic-sparkles"),
                color = "yellow",
                width = 3
              )
            ),
            hr(),
            h4("How to use this app:"),
            tags$ol(
              tags$li("Upload your dataset (CSV file) in any analysis tab"),
              tags$li("Select your variables from the dropdown menus"),
              tags$li("Click 'Run Analysis' to get your results"),
              tags$li("Read the English interpretation"),
              tags$li("Download your results report")
            ),
            hr(),
            h4("Quick Links:"),
            tags$a(href = "https://cran.r-project.org/package=statease",
                   target = "_blank",
                   class = "btn btn-primary",
                   "CRAN Package"),
            tags$a(href = "https://devwebwacky.github.io/statease/",
                   target = "_blank",
                   class = "btn btn-primary",
                   style = "margin-left: 10px;",
                   "Documentation"),
            tags$a(href = "https://github.com/DevWebWacky/statease",
                   target = "_blank",
                   class = "btn btn-primary",
                   style = "margin-left: 10px;",
                   "GitHub")
          )
        )
      ),

      # Descriptive Statistics
      tabItem(
        tabName = "describe",
        fluidRow(
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            title = "Input",
            fileInput("desc_file", "Upload CSV File",
                      accept = ".csv"),
            uiOutput("desc_var_ui"),
            textInput("desc_varname", "Variable Label",
                      value = "Variable"),
            actionButton("desc_run", "Run Analysis",
                         class = "btn btn-primary btn-block",
                         icon = icon("play"))
          ),
          box(
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            title = "Results",
            verbatimTextOutput("desc_result"),
            hr(),
            downloadButton("desc_download", "Download Report")
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "info",
            solidHeader = TRUE,
            title = "Data Preview",
            DTOutput("desc_preview")
          )
        )
      ),

      #Ttest
      tabItem(
        tabName = "ttest",
        fluidRow(
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            title = "Input",
            fileInput("ttest_file", "Upload CSV File",
                      accept = ".csv"),
            selectInput("ttest_type", "Test Type",
                        choices = c(
                          "Independent Samples" = "independent",
                          "One Sample" = "onesample",
                          "Paired Samples" = "paired"
                        )),
            uiOutput("ttest_x_ui"),
            uiOutput("ttest_y_ui"),
            uiOutput("ttest_mu_ui"),
            sliderInput("ttest_conf", "Confidence Level",
                        min = 0.90, max = 0.99,
                        value = 0.95, step = 0.01),
            textInput("ttest_varname", "Variable Label",
                      value = "Variable"),
            actionButton("ttest_run", "Run Analysis",
                         class = "btn btn-primary btn-block",
                         icon = icon("play"))
          ),
          box(
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            title = "Results",
            verbatimTextOutput("ttest_result"),
            hr(),
            downloadButton("ttest_download", "Download Report")
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "info",
            solidHeader = TRUE,
            title = "Data Preview",
            DTOutput("ttest_preview")
          )
        )
      ),

      #One Way ANOVA
      tabItem(
        tabName = "anova",
        fluidRow(
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            title = "Input",
            fileInput("anova_file", "Upload CSV File",
                      accept = ".csv"),
            uiOutput("anova_outcome_ui"),
            uiOutput("anova_group_ui"),
            sliderInput("anova_conf", "Confidence Level",
                        min = 0.90, max = 0.99,
                        value = 0.95, step = 0.01),
            actionButton("anova_run", "Run Analysis",
                         class = "btn btn-primary btn-block",
                         icon = icon("play"))
          ),
          box(
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            title = "Results",
            verbatimTextOutput("anova_result"),
            hr(),
            downloadButton("anova_download", "Download Report")
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "info",
            solidHeader = TRUE,
            title = "Data Preview",
            DTOutput("anova_preview")
          )
        )
      ),

      #TWO WAY ANOVA
      tabItem(
        tabName = "anova2",
        fluidRow(
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            title = "Input",
            fileInput("anova2_file", "Upload CSV File",
                      accept = ".csv"),
            uiOutput("anova2_outcome_ui"),
            uiOutput("anova2_group1_ui"),
            uiOutput("anova2_group2_ui"),
            selectInput("anova2_type", "SS Type",
                        choices = c("Type II" = "II",
                                    "Type III" = "III")),
            sliderInput("anova2_conf", "Confidence Level",
                        min = 0.90, max = 0.99,
                        value = 0.95, step = 0.01),
            actionButton("anova2_run", "Run Analysis",
                         class = "btn btn-primary btn-block",
                         icon = icon("play"))
          ),
          box(
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            title = "Results",
            verbatimTextOutput("anova2_result"),
            hr(),
            downloadButton("anova2_download", "Download Report")
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "info",
            solidHeader = TRUE,
            title = "Data Preview",
            DTOutput("anova2_preview")
          )
        )
      ),

      #MANOVA
      tabItem(
        tabName = "manova",
        fluidRow(
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            title = "Input",
            fileInput("manova_file", "Upload CSV File",
                      accept = ".csv"),
            uiOutput("manova_outcomes_ui"),
            uiOutput("manova_group_ui"),
            sliderInput("manova_conf", "Confidence Level",
                        min = 0.90, max = 0.99,
                        value = 0.95, step = 0.01),
            actionButton("manova_run", "Run Analysis",
                         class = "btn btn-primary btn-block",
                         icon = icon("play"))
          ),
          box(
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            title = "Results",
            verbatimTextOutput("manova_result"),
            hr(),
            downloadButton("manova_download", "Download Report")
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "info",
            solidHeader = TRUE,
            title = "Data Preview",
            DTOutput("manova_preview")
          )
        )
      ),

      #Chi-square
      tabItem(
        tabName = "chisq",
        fluidRow(
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            title = "Input",
            fileInput("chisq_file", "Upload CSV File",
                      accept = ".csv"),
            uiOutput("chisq_x_ui"),
            uiOutput("chisq_y_ui"),
            sliderInput("chisq_conf", "Confidence Level",
                        min = 0.90, max = 0.99,
                        value = 0.95, step = 0.01),
            actionButton("chisq_run", "Run Analysis",
                         class = "btn btn-primary btn-block",
                         icon = icon("play"))
          ),
          box(
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            title = "Results",
            verbatimTextOutput("chisq_result"),
            hr(),
            downloadButton("chisq_download", "Download Report")
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "info",
            solidHeader = TRUE,
            title = "Data Preview",
            DTOutput("chisq_preview")
          )
        )
      ),

      #Correlation
      tabItem(
        tabName = "correlation",
        fluidRow(
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            title = "Input",
            fileInput("cor_file", "Upload CSV File",
                      accept = ".csv"),
            uiOutput("cor_x_ui"),
            uiOutput("cor_y_ui"),
            selectInput("cor_method", "Correlation Method",
                        choices = c("Pearson"  = "pearson",
                                    "Spearman" = "spearman",
                                    "Kendall"  = "kendall")),
            sliderInput("cor_conf", "Confidence Level",
                        min = 0.90, max = 0.99,
                        value = 0.95, step = 0.01),
            textInput("cor_var1", "Variable 1 Label",
                      value = "Variable 1"),
            textInput("cor_var2", "Variable 2 Label",
                      value = "Variable 2"),
            actionButton("cor_run", "Run Analysis",
                         class = "btn btn-primary btn-block",
                         icon = icon("play"))
          ),
          box(
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            title = "Results",
            verbatimTextOutput("cor_result"),
            hr(),
            downloadButton("cor_download", "Download Report")
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "info",
            solidHeader = TRUE,
            title = "Data Preview",
            DTOutput("cor_preview")
          )
        )
      ),

      #Regression
      tabItem(
        tabName = "regression",
        fluidRow(
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            title = "Input",
            fileInput("reg_file", "Upload CSV File",
                      accept = ".csv"),
            selectInput("reg_type", "Regression Type",
                        choices = c(
                          "Simple Linear"   = "simple",
                          "Multiple Linear" = "multiple",
                          "Logistic"        = "logistic"
                        )),
            uiOutput("reg_outcome_ui"),
            uiOutput("reg_predictors_ui"),
            sliderInput("reg_conf", "Confidence Level",
                        min = 0.90, max = 0.99,
                        value = 0.95, step = 0.01),
            actionButton("reg_run", "Run Analysis",
                         class = "btn btn-primary btn-block",
                         icon = icon("play"))
          ),
          box(
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            title = "Results",
            verbatimTextOutput("reg_result"),
            hr(),
            downloadButton("reg_download", "Download Report")
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "info",
            solidHeader = TRUE,
            title = "Data Preview",
            DTOutput("reg_preview")
          )
        )
      ),

      #Non-parametric tests
      tabItem(
        tabName = "nonparam",
        fluidRow(
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            title = "Input",
            fileInput("np_file", "Upload CSV File",
                      accept = ".csv"),
            selectInput("np_type", "Test Type",
                        choices = c(
                          "Mann-Whitney U"      = "mannwhitney",
                          "Wilcoxon Signed Rank"= "wilcoxon",
                          "Kruskal-Wallis"      = "kruskal"
                        )),
            uiOutput("np_x_ui"),
            uiOutput("np_y_ui"),
            sliderInput("np_conf", "Confidence Level",
                        min = 0.90, max = 0.99,
                        value = 0.95, step = 0.01),
            textInput("np_varname", "Variable Label",
                      value = "Variable"),
            actionButton("np_run", "Run Analysis",
                         class = "btn btn-primary btn-block",
                         icon = icon("play"))
          ),
          box(
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            title = "Results",
            verbatimTextOutput("np_result"),
            hr(),
            downloadButton("np_download", "Download Report")
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "info",
            solidHeader = TRUE,
            title = "Data Preview",
            DTOutput("np_preview")
          )
        )
      ),

      #Pvalue Interpreter
      tabItem(
        tabName = "pvalue",
        fluidRow(
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            title = "Input",
            numericInput("pval_p", "P-Value",
                         value = 0.05,
                         min = 0, max = 1,
                         step = 0.001),
            numericInput("pval_alpha", "Alpha Level",
                         value = 0.05,
                         min = 0.01, max = 0.10,
                         step = 0.01),
            textInput("pval_context", "Context (optional)",
                      placeholder = "e.g. treatment vs control"),
            actionButton("pval_run", "Interpret P-Value",
                         class = "btn btn-primary btn-block",
                         icon = icon("play"))
          ),
          box(
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            title = "Results",
            verbatimTextOutput("pval_result"),
            hr(),
            downloadButton("pval_download", "Download Report")
          )
        )
      ),

      #Auto analyze
      tabItem(
        tabName = "analyze",
        fluidRow(
          box(
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            title = "Auto Analyze — Let statease decide the right test!!",
            p("Upload your data and select your variables.
              statease will automatically detect the right
              statistical test based on your data structure!!")
          )
        ),
        fluidRow(
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            title = "Input",
            fileInput("auto_file", "Upload CSV File",
                      accept = ".csv"),
            uiOutput("auto_x_ui"),
            uiOutput("auto_y_ui"),
            checkboxInput("auto_nonparam",
                          "Use non-parametric test",
                          value = FALSE),
            sliderInput("auto_conf", "Confidence Level",
                        min = 0.90, max = 0.99,
                        value = 0.95, step = 0.01),
            textInput("auto_varname", "Variable Label",
                      value = "Variable"),
            actionButton("auto_run", "Auto Analyze",
                         class = "btn btn-primary btn-block",
                         icon = icon("wand-magic-sparkles"))
          ),
          box(
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            title = "Results",
            verbatimTextOutput("auto_result"),
            hr(),
            downloadButton("auto_download", "Download Report")
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "info",
            solidHeader = TRUE,
            title = "Data Preview",
            DTOutput("auto_preview")
          )
        )
      ),

      #About
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            title = "About statease",
            h3("statease — Statistical Analysis Made Simple"),
            p("statease is a free, open-source R package published
              on CRAN that makes statistical analysis accessible to
              everyone by running statistical tests and
              automatically interpreting results in English."),
            hr(),
            h4("Author"),
            p("Uwakmfon Usen Paul"),
            hr(),
            h4("Version"),
            p("1.2.1"),
            hr(),
            h4("Links"),
            tags$a(href = "https://cran.r-project.org/package=statease",
                   target = "_blank", "CRAN"),
            tags$span(" | "),
            tags$a(href = "https://devwebwacky.github.io/statease/",
                   target = "_blank", "Website"),
            tags$span(" | "),
            tags$a(href = "https://github.com/DevWebWacky/statease",
                   target = "_blank", "GitHub"),
            hr(),
            h4("Citation"),
            p("If you use statease in your research please cite:"),
            verbatimTextOutput("citation_text")
          )
        )
      )
    )
  )
)

#Server
server <- function(input, output, session) {

  # Helper: capture print output as text
  capture_output <- function(expr) {
    paste(capture.output(expr), collapse = "\n")
  }

  # Helper: read uploaded CSV
  read_csv_file <- function(file_input) {
    req(file_input)
    read.csv(file_input$datapath, stringsAsFactors = FALSE)
  }

  #Citation
  output$citation_text <- renderText({
    'Usen Paul, U. (2025). statease: Simplified Statistical
Analysis with Plain-English Interpretation.
R package version 1.2.1.
https://cran.r-project.org/package=statease'
  })

  #Descriptive Statistics
  desc_data <- reactive({ read_csv_file(input$desc_file) })

  output$desc_var_ui <- renderUI({
    req(desc_data())
    nums <- names(desc_data())[sapply(desc_data(), is.numeric)]
    selectInput("desc_var", "Select Variable", choices = nums)
  })

  output$desc_preview <- renderDT({
    req(desc_data())
    datatable(desc_data(), options = list(pageLength = 5))
  })

  desc_result_text <- eventReactive(input$desc_run, {
    req(desc_data(), input$desc_var)
    result <- describe(desc_data()[[input$desc_var]],
                       var_name = input$desc_varname)
    capture_output(print(result))
  })

  output$desc_result <- renderText({ desc_result_text() })

  output$desc_download <- downloadHandler(
    filename = "statease_descriptive_report.txt",
    content  = function(file) {
      writeLines(desc_result_text(), file)
    }
  )

  #TTest
  ttest_data <- reactive({ read_csv_file(input$ttest_file) })

  output$ttest_x_ui <- renderUI({
    req(ttest_data())
    nums <- names(ttest_data())[sapply(ttest_data(), is.numeric)]
    selectInput("ttest_x", "Select Variable (x)", choices = nums)
  })

  output$ttest_y_ui <- renderUI({
    req(ttest_data(), input$ttest_type)
    if (input$ttest_type == "onesample") return(NULL)
    nums <- names(ttest_data())[sapply(ttest_data(), is.numeric)]
    selectInput("ttest_y", "Select Variable (y)", choices = nums)
  })

  output$ttest_mu_ui <- renderUI({
    req(input$ttest_type)
    if (input$ttest_type != "onesample") return(NULL)
    numericInput("ttest_mu", "Hypothesised Mean (mu)",
                 value = 0)
  })

  output$ttest_preview <- renderDT({
    req(ttest_data())
    datatable(ttest_data(), options = list(pageLength = 5))
  })

  ttest_result_text <- eventReactive(input$ttest_run, {
    req(ttest_data(), input$ttest_x)
    x <- ttest_data()[[input$ttest_x]]

    result <- if (input$ttest_type == "onesample") {
      ttest_interpret(x, mu = input$ttest_mu,
                      conf.level = input$ttest_conf,
                      var_name = input$ttest_varname)
    } else if (input$ttest_type == "paired") {
      req(input$ttest_y)
      y <- ttest_data()[[input$ttest_y]]
      ttest_interpret(x, y, paired = TRUE,
                      conf.level = input$ttest_conf,
                      var_name = input$ttest_varname)
    } else {
      req(input$ttest_y)
      y <- ttest_data()[[input$ttest_y]]
      ttest_interpret(x, y, paired = FALSE,
                      conf.level = input$ttest_conf,
                      var_name = input$ttest_varname)
    }
    capture_output(print(result))
  })

  output$ttest_result <- renderText({ ttest_result_text() })

  output$ttest_download <- downloadHandler(
    filename = "statease_ttest_report.txt",
    content  = function(file) {
      writeLines(ttest_result_text(), file)
    }
  )

  #OneWayAnova
  anova_data <- reactive({ read_csv_file(input$anova_file) })

  output$anova_outcome_ui <- renderUI({
    req(anova_data())
    nums <- names(anova_data())[sapply(anova_data(), is.numeric)]
    selectInput("anova_outcome", "Select Outcome Variable",
                choices = nums)
  })

  output$anova_group_ui <- renderUI({
    req(anova_data())
    selectInput("anova_group", "Select Group Variable",
                choices = names(anova_data()))
  })

  output$anova_preview <- renderDT({
    req(anova_data())
    datatable(anova_data(), options = list(pageLength = 5))
  })

  anova_result_text <- eventReactive(input$anova_run, {
    req(anova_data(), input$anova_outcome, input$anova_group)
    formula <- as.formula(
      paste(input$anova_outcome, "~", input$anova_group)
    )
    result <- anova_interpret(formula,
                              data = anova_data(),
                              conf.level = input$anova_conf)
    capture_output(print(result))
  })

  output$anova_result <- renderText({ anova_result_text() })

  output$anova_download <- downloadHandler(
    filename = "statease_anova_report.txt",
    content  = function(file) {
      writeLines(anova_result_text(), file)
    }
  )

  #2WayAnova
  anova2_data <- reactive({ read_csv_file(input$anova2_file) })

  output$anova2_outcome_ui <- renderUI({
    req(anova2_data())
    nums <- names(anova2_data())[sapply(anova2_data(), is.numeric)]
    selectInput("anova2_outcome", "Select Outcome Variable",
                choices = nums)
  })

  output$anova2_group1_ui <- renderUI({
    req(anova2_data())
    selectInput("anova2_group1", "Select Factor 1",
                choices = names(anova2_data()))
  })

  output$anova2_group2_ui <- renderUI({
    req(anova2_data())
    selectInput("anova2_group2", "Select Factor 2",
                choices = names(anova2_data()))
  })

  output$anova2_preview <- renderDT({
    req(anova2_data())
    datatable(anova2_data(), options = list(pageLength = 5))
  })

  anova2_result_text <- eventReactive(input$anova2_run, {
    req(anova2_data(), input$anova2_outcome,
        input$anova2_group1, input$anova2_group2)
    formula <- as.formula(paste(
      input$anova2_outcome, "~",
      input$anova2_group1, "*",
      input$anova2_group2
    ))
    result <- anova2_interpret(formula,
                               data = anova2_data(),
                               conf.level = input$anova2_conf,
                               type = input$anova2_type)
    capture_output(print(result))
  })

  output$anova2_result <- renderText({ anova2_result_text() })

  output$anova2_download <- downloadHandler(
    filename = "statease_anova2_report.txt",
    content  = function(file) {
      writeLines(anova2_result_text(), file)
    }
  )

  #Manova
  manova_data <- reactive({ read_csv_file(input$manova_file) })

  output$manova_outcomes_ui <- renderUI({
    req(manova_data())
    nums <- names(manova_data())[sapply(manova_data(), is.numeric)]
    selectInput("manova_outcomes", "Select Outcome Variables
                (select 2 or more)",
                choices = nums, multiple = TRUE)
  })

  output$manova_group_ui <- renderUI({
    req(manova_data())
    selectInput("manova_group", "Select Group Variable",
                choices = names(manova_data()))
  })

  output$manova_preview <- renderDT({
    req(manova_data())
    datatable(manova_data(), options = list(pageLength = 5))
  })

  manova_result_text <- eventReactive(input$manova_run, {
    req(manova_data(), input$manova_outcomes,
        input$manova_group)
    outcomes <- paste(input$manova_outcomes, collapse = ", ")
    formula  <- as.formula(paste(
      "cbind(", outcomes, ") ~", input$manova_group
    ))
    result <- manova_interpret(formula,
                               data = manova_data(),
                               conf.level = input$manova_conf)
    capture_output(print(result))
  })

  output$manova_result <- renderText({ manova_result_text() })

  output$manova_download <- downloadHandler(
    filename = "statease_manova_report.txt",
    content  = function(file) {
      writeLines(manova_result_text(), file)
    }
  )

  #Chi-square
  chisq_data <- reactive({ read_csv_file(input$chisq_file) })

  output$chisq_x_ui <- renderUI({
    req(chisq_data())
    selectInput("chisq_x", "Select Variable 1 (x)",
                choices = names(chisq_data()))
  })

  output$chisq_y_ui <- renderUI({
    req(chisq_data())
    selectInput("chisq_y", "Select Variable 2 (y)",
                choices = names(chisq_data()))
  })

  output$chisq_preview <- renderDT({
    req(chisq_data())
    datatable(chisq_data(), options = list(pageLength = 5))
  })

  chisq_result_text <- eventReactive(input$chisq_run, {
    req(chisq_data(), input$chisq_x, input$chisq_y)
    result <- chisq_interpret(
      chisq_data()[[input$chisq_x]],
      chisq_data()[[input$chisq_y]],
      conf.level = input$chisq_conf
    )
    capture_output(print(result))
  })

  output$chisq_result <- renderText({ chisq_result_text() })

  output$chisq_download <- downloadHandler(
    filename = "statease_chisq_report.txt",
    content  = function(file) {
      writeLines(chisq_result_text(), file)
    }
  )

  #Correlation
  cor_data <- reactive({ read_csv_file(input$cor_file) })

  output$cor_x_ui <- renderUI({
    req(cor_data())
    nums <- names(cor_data())[sapply(cor_data(), is.numeric)]
    selectInput("cor_x", "Select Variable 1 (x)",
                choices = nums)
  })

  output$cor_y_ui <- renderUI({
    req(cor_data())
    nums <- names(cor_data())[sapply(cor_data(), is.numeric)]
    selectInput("cor_y", "Select Variable 2 (y)",
                choices = nums)
  })

  output$cor_preview <- renderDT({
    req(cor_data())
    datatable(cor_data(), options = list(pageLength = 5))
  })

  cor_result_text <- eventReactive(input$cor_run, {
    req(cor_data(), input$cor_x, input$cor_y)
    result <- cor_interpret(
      cor_data()[[input$cor_x]],
      cor_data()[[input$cor_y]],
      method    = input$cor_method,
      conf.level = input$cor_conf,
      var1_name = input$cor_var1,
      var2_name = input$cor_var2
    )
    capture_output(print(result))
  })

  output$cor_result <- renderText({ cor_result_text() })

  output$cor_download <- downloadHandler(
    filename = "statease_correlation_report.txt",
    content  = function(file) {
      writeLines(cor_result_text(), file)
    }
  )

  #Regression
  reg_data <- reactive({ read_csv_file(input$reg_file) })

  output$reg_outcome_ui <- renderUI({
    req(reg_data())
    nums <- names(reg_data())[sapply(reg_data(), is.numeric)]
    selectInput("reg_outcome", "Select Outcome Variable",
                choices = nums)
  })

  output$reg_predictors_ui <- renderUI({
    req(reg_data())
    nums <- names(reg_data())[sapply(reg_data(), is.numeric)]
    if (input$reg_type == "simple") {
      selectInput("reg_predictors", "Select Predictor",
                  choices = nums)
    } else {
      selectInput("reg_predictors", "Select Predictors
                  (select 2 or more)",
                  choices = nums, multiple = TRUE)
    }
  })

  output$reg_preview <- renderDT({
    req(reg_data())
    datatable(reg_data(), options = list(pageLength = 5))
  })

  reg_result_text <- eventReactive(input$reg_run, {
    req(reg_data(), input$reg_outcome, input$reg_predictors)
    predictors <- paste(input$reg_predictors, collapse = " + ")
    formula    <- as.formula(
      paste(input$reg_outcome, "~", predictors)
    )
    result <- if (input$reg_type == "logistic") {
      logistic_interpret(formula,
                         data = reg_data(),
                         conf.level = input$reg_conf)
    } else if (input$reg_type == "multiple") {
      mlr_interpret(formula,
                    data = reg_data(),
                    conf.level = input$reg_conf)
    } else {
      reg_interpret(formula,
                    data = reg_data(),
                    conf.level = input$reg_conf)
    }
    capture_output(print(result))
  })

  output$reg_result <- renderText({ reg_result_text() })

  output$reg_download <- downloadHandler(
    filename = "statease_regression_report.txt",
    content  = function(file) {
      writeLines(reg_result_text(), file)
    }
  )

  #Non-parametric
  np_data <- reactive({ read_csv_file(input$np_file) })

  output$np_x_ui <- renderUI({
    req(np_data())
    nums <- names(np_data())[sapply(np_data(), is.numeric)]
    selectInput("np_x", "Select Variable 1 (x)",
                choices = nums)
  })

  output$np_y_ui <- renderUI({
    req(np_data(), input$np_type)
    if (input$np_type == "kruskal") {
      selectInput("np_y", "Select Group Variable",
                  choices = names(np_data()))
    } else {
      nums <- names(np_data())[sapply(np_data(), is.numeric)]
      selectInput("np_y", "Select Variable 2 (y)",
                  choices = nums)
    }
  })

  output$np_preview <- renderDT({
    req(np_data())
    datatable(np_data(), options = list(pageLength = 5))
  })

  np_result_text <- eventReactive(input$np_run, {
    req(np_data(), input$np_x, input$np_y)

    result <- if (input$np_type == "mannwhitney") {
      mannwhitney_interpret(
        np_data()[[input$np_x]],
        np_data()[[input$np_y]],
        conf.level = input$np_conf,
        var_name   = input$np_varname
      )
    } else if (input$np_type == "wilcoxon") {
      wilcoxon_interpret(
        np_data()[[input$np_x]],
        np_data()[[input$np_y]],
        conf.level = input$np_conf,
        var_name   = input$np_varname
      )
    } else {
      formula <- as.formula(
        paste(input$np_x, "~", input$np_y)
      )
      kruskal_interpret(formula,
                        data = np_data(),
                        conf.level = input$np_conf)
    }
    capture_output(print(result))
  })

  output$np_result <- renderText({ np_result_text() })

  output$np_download <- downloadHandler(
    filename = "statease_nonparam_report.txt",
    content  = function(file) {
      writeLines(np_result_text(), file)
    }
  )

  #PvalueInterpreter
  pval_result_text <- eventReactive(input$pval_run, {
    context <- if (nchar(input$pval_context) > 0) {
      input$pval_context
    } else {
      NULL
    }
    result <- interpret_p(
      p       = input$pval_p,
      alpha   = input$pval_alpha,
      context = context
    )
    capture_output(print(result))
  })

  output$pval_result <- renderText({ pval_result_text() })

  output$pval_download <- downloadHandler(
    filename = "statease_pvalue_report.txt",
    content  = function(file) {
      writeLines(pval_result_text(), file)
    }
  )

  #auto-analyze
  auto_data <- reactive({ read_csv_file(input$auto_file) })

  output$auto_x_ui <- renderUI({
    req(auto_data())
    selectInput("auto_x", "Select Variable (x)",
                choices = names(auto_data()))
  })

  output$auto_y_ui <- renderUI({
    req(auto_data())
    selectInput("auto_y", "Select Variable (y) — optional",
                choices = c("None", names(auto_data())))
  })

  output$auto_preview <- renderDT({
    req(auto_data())
    datatable(auto_data(), options = list(pageLength = 5))
  })

  auto_result_text <- eventReactive(input$auto_run, {
    req(auto_data(), input$auto_x)
    x <- auto_data()[[input$auto_x]]
    y <- if (input$auto_y != "None") {
      auto_data()[[input$auto_y]]
    } else {
      NULL
    }

    result_text <- capture.output({
      if (is.null(y)) {
        analyze(x        = x,
                nonparam = input$auto_nonparam,
                conf.level = input$auto_conf,
                var_name = input$auto_varname)
      } else {
        analyze(x        = x,
                y        = y,
                nonparam = input$auto_nonparam,
                conf.level = input$auto_conf,
                var_name = input$auto_varname)
      }
    })
    paste(result_text, collapse = "\n")
  })

  output$auto_result <- renderText({ auto_result_text() })

  output$auto_download <- downloadHandler(
    filename = "statease_auto_report.txt",
    content  = function(file) {
      writeLines(auto_result_text(), file)
    }
  )
}

#runApp
shinyApp(ui = ui, server = server)
