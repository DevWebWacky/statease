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
      menuItem("Friedman Test",
               tabName = "friedman",
               icon = icon("repeat")),
      menuItem("Fisher's Exact Test",
               tabName = "fisher",
               icon = icon("table-cells")),
      menuItem("McNemar's Test",
               tabName = "mcnemar",
               icon = icon("arrows-left-right")),
      menuItem("Check Assumptions",
               tabName = "checkassumptions",
               icon = icon("clipboard-check")),
      menuItem("Power Analysis",
               tabName = "power",
               icon = icon("gauge-high")),
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

      # FRIEDMAN
      tabItem(
        tabName = "friedman",
        fluidRow(
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            title = "Input",
            fileInput("friedman_file", "Upload CSV File",
                      accept = ".csv"),
            uiOutput("friedman_outcome_ui"),
            uiOutput("friedman_time_ui"),
            uiOutput("friedman_subject_ui"),
            sliderInput("friedman_conf", "Confidence Level",
                        min = 0.90, max = 0.99,
                        value = 0.95, step = 0.01),
            actionButton("friedman_run", "Run Analysis",
                         class = "btn btn-primary btn-block",
                         icon = icon("play"))
          ),
          box(
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            title = "Results",
            verbatimTextOutput("friedman_result"),
            hr(),
            downloadButton("friedman_download", "Download Report")
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "info",
            solidHeader = TRUE,
            title = "Data Preview",
            DTOutput("friedman_preview")
          )
        )
      ),

      #FISHER'S EXACT TEST
      tabItem(
        tabName = "fisher",
        fluidRow(
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            title = "Input",
            fileInput("fisher_file", "Upload CSV File",
                      accept = ".csv"),
            uiOutput("fisher_x_ui"),
            uiOutput("fisher_y_ui"),
            checkboxInput("fisher_simulate",
                          "Use simulation for larger tables",
                          value = FALSE),
            sliderInput("fisher_conf", "Confidence Level",
                        min = 0.90, max = 0.99,
                        value = 0.95, step = 0.01),
            actionButton("fisher_run", "Run Analysis",
                         class = "btn btn-primary btn-block",
                         icon = icon("play"))
          ),
          box(
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            title = "Results",
            verbatimTextOutput("fisher_result"),
            hr(),
            downloadButton("fisher_download", "Download Report")
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "info",
            solidHeader = TRUE,
            title = "Data Preview",
            DTOutput("fisher_preview")
          )
        )
      ),

      #MCNEMAR'S Test
      tabItem(
        tabName = "mcnemar",
        fluidRow(
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            title = "Input",
            fileInput("mcnemar_file", "Upload CSV File",
                      accept = ".csv"),
            uiOutput("mcnemar_x_ui"),
            uiOutput("mcnemar_y_ui"),
            sliderInput("mcnemar_conf", "Confidence Level",
                        min = 0.90, max = 0.99,
                        value = 0.95, step = 0.01),
            actionButton("mcnemar_run", "Run Analysis",
                         class = "btn btn-primary btn-block",
                         icon = icon("play"))
          ),
          box(
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            title = "Results",
            verbatimTextOutput("mcnemar_result"),
            hr(),
            downloadButton("mcnemar_download", "Download Report")
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "info",
            solidHeader = TRUE,
            title = "Data Preview",
            DTOutput("mcnemar_preview")
          )
        )
      ),

      #ASSUMPTIONS check
      tabItem(
        tabName = "checkassumptions",
        fluidRow(
          box(
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            title = "Check Assumptions Before Running Your Test",
            p("Verify that your data meets the statistical
              assumptions required for your chosen test.")
          )
        ),
        fluidRow(
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            title = "Input",
            fileInput("check_file", "Upload CSV File",
                      accept = ".csv"),
            selectInput("check_test", "Select Test",
                        choices = c(
                          "T-Test"      = "ttest",
                          "One-Way ANOVA" = "anova",
                          "Two-Way ANOVA" = "anova2",
                          "Correlation" = "correlation",
                          "Regression"  = "regression"
                        )),
            uiOutput("check_dynamic_ui"),
            actionButton("check_run", "Check Assumptions",
                         class = "btn btn-primary btn-block",
                         icon = icon("clipboard-check"))
          ),
          box(
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            title = "Results",
            verbatimTextOutput("check_result"),
            hr(),
            downloadButton("check_download", "Download Report")
          )
        ),
        fluidRow(
          box(
            width = 12,
            status = "info",
            solidHeader = TRUE,
            title = "Data Preview",
            DTOutput("check_preview")
          )
        )
      ),

      #Power Analysis
      tabItem(
        tabName = "power",
        fluidRow(
          box(
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            title = "Power Analysis - Calculate Sample Size or Power",
            p("Determine the sample size needed to detect an
              effect, or calculate the achieved power of your
              existing study.")
          )
        ),
        fluidRow(
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            title = "Input",
            selectInput("power_test", "Select Test",
                        choices = c(
                          "One-Sample T-Test" = "ttest.one",
                          "Independent T-Test" = "ttest.two",
                          "Paired T-Test" = "ttest.paired",
                          "ANOVA" = "anova",
                          "Correlation" = "correlation",
                          "Chi-Square" = "chisq",
                          "Regression" = "regression"
                        )),
            numericInput("power_effect", "Effect Size",
                         value = 0.5, min = 0, step = 0.05),
            selectInput("power_mode", "Calculate",
                        choices = c(
                          "Required Sample Size" = "n",
                          "Achieved Power" = "power"
                        )),
            uiOutput("power_n_ui"),
            uiOutput("power_power_ui"),
            uiOutput("power_groups_ui"),
            uiOutput("power_predictors_ui"),
            sliderInput("power_alpha", "Alpha Level",
                        min = 0.01, max = 0.10,
                        value = 0.05, step = 0.01),
            actionButton("power_run", "Calculate",
                         class = "btn btn-primary btn-block",
                         icon = icon("play"))
          ),
          box(
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            title = "Results",
            verbatimTextOutput("power_result"),
            hr(),
            downloadButton("power_download", "Download Report")
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
            p("1.3.0"),
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
    'Paul, Uwakmfon U. (2026). statease: Simplified Statistical
Analysis with Plain-English Interpretation.
R package version 1.3.0
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

  # ── FRIEDMAN TEST ─────────────────────────────────────────
  friedman_data <- reactive({ read_csv_file(input$friedman_file) })

  output$friedman_outcome_ui <- renderUI({
    req(friedman_data())
    nums <- names(friedman_data())[sapply(friedman_data(), is.numeric)]
    selectInput("friedman_outcome", "Select Outcome Variable",
                choices = nums)
  })

  output$friedman_time_ui <- renderUI({
    req(friedman_data())
    selectInput("friedman_time", "Select Time/Condition Variable",
                choices = names(friedman_data()))
  })

  output$friedman_subject_ui <- renderUI({
    req(friedman_data())
    selectInput("friedman_subject", "Select Subject ID Variable",
                choices = names(friedman_data()))
  })

  output$friedman_preview <- renderDT({
    req(friedman_data())
    datatable(friedman_data(), options = list(pageLength = 5))
  })

  friedman_result_text <- eventReactive(input$friedman_run, {
    req(friedman_data(), input$friedman_outcome,
        input$friedman_time, input$friedman_subject)
    formula <- as.formula(paste(
      input$friedman_outcome, "~",
      input$friedman_time, "|",
      input$friedman_subject
    ))
    result <- friedman_interpret(formula,
                                 data = friedman_data(),
                                 conf.level = input$friedman_conf)
    capture_output(print(result))
  })

  output$friedman_result <- renderText({ friedman_result_text() })

  output$friedman_download <- downloadHandler(
    filename = "statease_friedman_report.txt",
    content  = function(file) {
      writeLines(friedman_result_text(), file)
    }
  )

  # ── FISHER'S EXACT TEST ───────────────────────────────────
  fisher_data <- reactive({ read_csv_file(input$fisher_file) })

  output$fisher_x_ui <- renderUI({
    req(fisher_data())
    selectInput("fisher_x", "Select Variable 1 (x)",
                choices = names(fisher_data()))
  })

  output$fisher_y_ui <- renderUI({
    req(fisher_data())
    selectInput("fisher_y", "Select Variable 2 (y)",
                choices = names(fisher_data()))
  })

  output$fisher_preview <- renderDT({
    req(fisher_data())
    datatable(fisher_data(), options = list(pageLength = 5))
  })

  fisher_result_text <- eventReactive(input$fisher_run, {
    req(fisher_data(), input$fisher_x, input$fisher_y)
    result <- fisher_interpret(
      fisher_data()[[input$fisher_x]],
      fisher_data()[[input$fisher_y]],
      conf.level = input$fisher_conf,
      simulate.p.value = input$fisher_simulate
    )
    capture_output(print(result))
  })

  output$fisher_result <- renderText({ fisher_result_text() })

  output$fisher_download <- downloadHandler(
    filename = "statease_fisher_report.txt",
    content  = function(file) {
      writeLines(fisher_result_text(), file)
    }
  )

  # ── MCNEMAR'S TEST ────────────────────────────────────────
  mcnemar_data <- reactive({ read_csv_file(input$mcnemar_file) })

  output$mcnemar_x_ui <- renderUI({
    req(mcnemar_data())
    selectInput("mcnemar_x", "Select Variable 1 (before)",
                choices = names(mcnemar_data()))
  })

  output$mcnemar_y_ui <- renderUI({
    req(mcnemar_data())
    selectInput("mcnemar_y", "Select Variable 2 (after)",
                choices = names(mcnemar_data()))
  })

  output$mcnemar_preview <- renderDT({
    req(mcnemar_data())
    datatable(mcnemar_data(), options = list(pageLength = 5))
  })

  mcnemar_result_text <- eventReactive(input$mcnemar_run, {
    req(mcnemar_data(), input$mcnemar_x, input$mcnemar_y)
    result <- mcnemar_interpret(
      mcnemar_data()[[input$mcnemar_x]],
      mcnemar_data()[[input$mcnemar_y]],
      conf.level = input$mcnemar_conf
    )
    capture_output(print(result))
  })

  output$mcnemar_result <- renderText({ mcnemar_result_text() })

  output$mcnemar_download <- downloadHandler(
    filename = "statease_mcnemar_report.txt",
    content  = function(file) {
      writeLines(mcnemar_result_text(), file)
    }
  )

  # ── CHECK ASSUMPTIONS ─────────────────────────────────────
  check_data <- reactive({ read_csv_file(input$check_file) })

  output$check_preview <- renderDT({
    req(check_data())
    datatable(check_data(), options = list(pageLength = 5))
  })

  output$check_dynamic_ui <- renderUI({
    req(check_data(), input$check_test)
    nums <- names(check_data())[sapply(check_data(), is.numeric)]

    if (input$check_test == "ttest") {
      tagList(
        selectInput("check_x", "Select Variable (x)", choices = nums),
        selectInput("check_y", "Select Variable (y) - optional",
                    choices = c("None", nums))
      )
    } else if (input$check_test == "correlation") {
      tagList(
        selectInput("check_x", "Select Variable (x)", choices = nums),
        selectInput("check_y", "Select Variable (y)", choices = nums)
      )
    } else if (input$check_test == "anova") {
      tagList(
        selectInput("check_outcome", "Select Outcome",
                    choices = nums),
        selectInput("check_group", "Select Group Variable",
                    choices = names(check_data()))
      )
    } else if (input$check_test == "anova2") {
      tagList(
        selectInput("check_outcome", "Select Outcome",
                    choices = nums),
        selectInput("check_group1", "Select Factor 1",
                    choices = names(check_data())),
        selectInput("check_group2", "Select Factor 2",
                    choices = names(check_data()))
      )
    } else if (input$check_test == "regression") {
      tagList(
        selectInput("check_outcome", "Select Outcome",
                    choices = nums),
        selectInput("check_predictors", "Select Predictors",
                    choices = nums, multiple = TRUE)
      )
    }
  })

  check_result_text <- eventReactive(input$check_run, {
    req(check_data(), input$check_test)

    result <- if (input$check_test == "ttest") {
      req(input$check_x)
      y_val <- if (!is.null(input$check_y) && input$check_y != "None") {
        check_data()[[input$check_y]]
      } else NULL
      check_assumptions("ttest",
                        x = check_data()[[input$check_x]],
                        y = y_val)
    } else if (input$check_test == "correlation") {
      req(input$check_x, input$check_y)
      check_assumptions("correlation",
                        x = check_data()[[input$check_x]],
                        y = check_data()[[input$check_y]])
    } else if (input$check_test == "anova") {
      req(input$check_outcome, input$check_group)
      formula <- as.formula(paste(input$check_outcome, "~",
                                  input$check_group))
      check_assumptions("anova", formula = formula,
                        data = check_data())
    } else if (input$check_test == "anova2") {
      req(input$check_outcome, input$check_group1, input$check_group2)
      formula <- as.formula(paste(input$check_outcome, "~",
                                  input$check_group1, "*",
                                  input$check_group2))
      check_assumptions("anova2", formula = formula,
                        data = check_data())
    } else if (input$check_test == "regression") {
      req(input$check_outcome, input$check_predictors)
      predictors <- paste(input$check_predictors, collapse = " + ")
      formula    <- as.formula(paste(input$check_outcome, "~",
                                     predictors))
      check_assumptions("regression", formula = formula,
                        data = check_data())
    }
    capture_output(print(result))
  })

  output$check_result <- renderText({ check_result_text() })

  output$check_download <- downloadHandler(
    filename = "statease_assumptions_report.txt",
    content  = function(file) {
      writeLines(check_result_text(), file)
    }
  )

  # ── POWER ANALYSIS ────────────────────────────────────────
  output$power_n_ui <- renderUI({
    req(input$power_mode)
    if (input$power_mode == "power") {
      numericInput("power_n", "Sample Size (n)",
                   value = 30, min = 2)
    }
  })

  output$power_power_ui <- renderUI({
    req(input$power_mode)
    if (input$power_mode == "n") {
      sliderInput("power_power", "Desired Power",
                  min = 0.50, max = 0.99,
                  value = 0.80, step = 0.01)
    }
  })

  output$power_groups_ui <- renderUI({
    req(input$power_test)
    if (input$power_test == "anova") {
      numericInput("power_groups", "Number of Groups",
                   value = 3, min = 2)
    }
  })

  output$power_predictors_ui <- renderUI({
    req(input$power_test)
    if (input$power_test == "regression") {
      numericInput("power_predictors_n", "Number of Predictors",
                   value = 1, min = 1)
    }
  })

  power_result_text <- eventReactive(input$power_run, {
    req(input$power_test, input$power_effect, input$power_mode)

    n_val <- if (input$power_mode == "power") input$power_n else NULL
    power_val <- if (input$power_mode == "n") {
      input$power_power
    } else {
      0.80
    }
    groups_val <- if (!is.null(input$power_groups)) {
      input$power_groups
    } else {
      2
    }
    predictors_val <- if (!is.null(input$power_predictors_n)) {
      input$power_predictors_n
    } else {
      1
    }

    result <- power_interpret(
      test         = input$power_test,
      effect_size  = input$power_effect,
      n            = n_val,
      alpha        = input$power_alpha,
      power        = power_val,
      n_groups     = groups_val,
      n_predictors = predictors_val
    )
    capture_output(print(result))
  })

  output$power_result <- renderText({ power_result_text() })

  output$power_download <- downloadHandler(
    filename = "statease_power_report.txt",
    content  = function(file) {
      writeLines(power_result_text(), file)
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
