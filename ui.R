require(shiny)
require(shinythemes)
require(shinyBS)
require(shinydashboard)
require(DT)
require(dashboardthemes)
require(plotly)
require(shinyjs)
require(shinyAce)
require(shinyalert)
require(ggplot2)
# require(shinyWidgets)
# require(sendmailR)
# devtools::install_github("nik01010/dashboardthemes")
require(dashboardthemes)

require(extrafont)
require(xkcd)


convertMenuItem <- function(mi,tabName) {
    mi$children[[1]]$attribs['data-toggle']="tab"
    mi$children[[1]]$attribs['data-value'] = tabName
    mi
}

#################
dark_grey_edited <- shinyDashboardThemeDIY(

    ### general
    appFontFamily = "Helvetica Neue, Helvetica, Arial, sans-serif"
    ,appFontColor = "rgb(205,205,205)"
    ,bodyBackColor = "rgb(39,43,48)"

    ### header
    ,logoBackColor = "rgb(70,80,90)"

    ,headerButtonBackColor = "rgb(70,80,90)"
    ,headerButtonIconColor = "rgb(198, 253, 168)"
    ,headerButtonBackColorHover = "rgb(40,50,60)"
    ,headerButtonIconColorHover = "rgb(0,0,0)"

    ,headerBackColor = "rgb(70,80,90)"
    ,headerBoxShadowColor = "rgb(198, 253, 168)"
    ,headerBoxShadowSize = "0px 0px 0px"

    ### sidebar
    ,sidebarBackColor = "rgb(52,62,72)"
    ,sidebarPadding = 0

    ,sidebarMenuBackColor = "transparent"
    ,sidebarMenuPadding = 0
    ,sidebarMenuBorderRadius = 0

    ,sidebarShadowRadius = ""
    ,sidebarShadowColor = "0px 0px 0px"

    ,sidebarUserTextColor = "rgb(205,205,205)"

    ,sidebarSearchBackColor = "rgb(39,43,48)"
    ,sidebarSearchIconColor = "rgb(153,153,153)"
    ,sidebarSearchBorderColor = "rgb(39,43,48)"

    ,sidebarTabTextColor = "rgb(205,205,205)"
    ,sidebarTabTextSize = 14
    ,sidebarTabBorderStyle = "none"
    ,sidebarTabBorderColor = "none"
    ,sidebarTabBorderWidth = 0

    ,sidebarTabBackColorSelected = "rgb(70,80,90)"
    ,sidebarTabTextColorSelected = "rgb(198, 253, 168)"
    ,sidebarTabRadiusSelected = "5px"

    ,sidebarTabBackColorHover = "rgb(55,65,75)"
    ,sidebarTabTextColorHover = "rgb(255,255,255)"
    ,sidebarTabBorderStyleHover = "none"
    ,sidebarTabBorderColorHover = "none"
    ,sidebarTabBorderWidthHover = 0
    ,sidebarTabRadiusHover = "5px"

    ### boxes
    ,boxBackColor = "rgb(52,62,72)"
    ,boxBorderRadius = 5
    ,boxShadowSize = "0px 0px 0px"
    ,boxShadowColor = ""
    ,boxTitleSize = 16
    ,boxDefaultColor = "rgb(52,62,72)"
    ,boxPrimaryColor = "rgb(200,200,200)"
    ,boxSuccessColor = "rgb(155,240,80)"
    ,boxWarningColor = "rgb(240,80,210)"
    ,boxDangerColor = "rgb(240,80,80)"

    ,tabBoxTabColor = "rgb(52,62,72)"
    ,tabBoxTabTextSize = 14
    ,tabBoxTabTextColor = "rgb(205,205,205)"
    ,tabBoxTabTextColorSelected = "rgb(205,205,205)"
    ,tabBoxBackColor = "rgb(52,62,72)"
    ,tabBoxHighlightColor = "rgb(70,80,90)"
    ,tabBoxBorderRadius = 5

    ### inputs
    ,buttonBackColor = "rgb(230,230,230)"
    ,buttonTextColor = "rgb(0,0,0)"
    ,buttonBorderColor = "rgb(50,50,50)"
    ,buttonBorderRadius = 5

    ,buttonBackColorHover = "rgb(180,180,180)"
    ,buttonTextColorHover = "rgb(50,50,50)"
    ,buttonBorderColorHover = "rgb(50,50,50)"

    ,textboxBackColor = "rgb(68,80,90)"
    ,textboxBorderColor = "rgb(76,90,103)"
    ,textboxBorderRadius = 5
    ,textboxBackColorSelect = "rgb(80,90,100)"
    ,textboxBorderColorSelect = "rgb(255,255,255)"

    ### tables
    ,tableBackColor = "rgb(52,62,72)"
    ,tableBorderColor = "rgb(70,80,90)"
    ,tableBorderTopSize = 1
    ,tableBorderRowSize = 1

)

#####################


# https://rstudio.github.io/shinydashboard/structure.html
title <- "DEAD - Diagnostic Estimates and Disease Calculator"
#  link not working
header <- dashboardHeader(
    title = div(img(src = "sq.png", height="42.885", width="34.29"), title), 
    titleWidth = 450,
    tags$head(
        # Meta description
        tags$meta(name = "description", content = "DEAD (Diagnostic Estimates and Disease) is a free statistical tool for evaluating diagnostic test performance, including sensitivity, specificity, ROC curves, and contingency tables."),
        
        # Keywords
        tags$meta(name = "keywords", content = "diagnostic test calculator, sensitivity specificity calculator, ROC curve analysis, medical statistics, diagnostic accuracy, contingency tables, McNemar test"),
        
        # Open Graph tags for social sharing
        tags$meta(property = "og:title", content = "DEAD - Diagnostic Test Calculator"),
        tags$meta(property = "og:description", content = "Free statistical tool for evaluating diagnostic test performance"),
        tags$meta(property = "og:image", content = "sq.png"),
        
        # Twitter Card tags
        tags$meta(name = "twitter:card", content = "summary"),
        tags$meta(name = "twitter:title", content = "DEAD - Diagnostic Test Calculator"),
        tags$meta(name = "twitter:description", content = "Free statistical tool for evaluating diagnostic test performance"),
        tags$meta(name = "twitter:image", content = "sq.png"),
        
        # Additional meta tags
        tags$meta(name = "author", content = "SemiQuant"),
        tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
        tags$meta(name = "robots", content = "index, follow")
    ),
    dropdownMenu(type = "notifications", icon = icon("copyright"), badgeStatus = NULL,
               notificationItem(text = "Copyright 2019 - V1.01")
    )
)

sidebar <- dashboardSidebar(
    width = 300,
    h3("Select a Test", align = "center"),
    dashboardSidebar(
        sidebarMenu(id = "sidebarmenu",
                    menuItem("Dashboard", tabName = "dashboard", icon = icon("home")),
                    
                    menuItem("Diagnostic Test Performance", tabName = "DT",
                             convertMenuItem(
                                 menuItem("One Test and Gold Standard", tabName = "DiagTAB1",
                                         div(style = "padding: 10px;",
                                             div(style = "display: grid; grid-template-columns: auto 100px 100px; gap: 5px; margin: 5px;",
                                                 # Header row
                                                 div(),
                                                 div(style = "text-align: center; color: white;", "Disease +"),
                                                 div(style = "text-align: center; color: white;", "Disease -"),
                                                 
                                                 # Test + row
                                                 div(style = "text-align: right; color: white;", "Test +"),
                                                 numericInput("tp", NULL, value = 13, min = 0, width = "100px"),
                                                 numericInput("fp", NULL, value = 12, min = 0, width = "100px"),
                                                 
                                                 # Test - row
                                                 div(style = "text-align: right; color: white;", "Test -"),
                                                 numericInput("fn", NULL, value = 41, min = 0, width = "100px"),
                                                 numericInput("tn", NULL, value = 33, min = 0, width = "100px")
                                             )
                                         )
                                 ), "DiagTAB1"),
                             menuItem("Two Tests and Gold Standard", tabName = "DiagTAB2")
                    ),

                    convertMenuItem(
                        menuItem("Contigency (RxC) Tables", tabName = "ChiTab",
                                 div(style = "padding: 10px;",
                                     div(style = "display: grid; grid-template-columns: auto 100px 100px; gap: 5px; margin: 5px;",
                                         # Header row
                                         div(),
                                         div(style = "text-align: center; color: white;", "Positive"),
                                         div(style = "text-align: center; color: white;", "Negative"),
                                         
                                         # Test + row
                                         div(style = "text-align: right; color: white;", "Positive"),
                                         numericInput("rc_pp", NULL, value = 13, min = 0, width = "100px"),
                                         numericInput("rc_pn", NULL, value = 21, min = 0, width = "100px"),
                                         
                                         # Test - row
                                         div(style = "text-align: right; color: white;", "Negative"),
                                         numericInput("rc_np", NULL, value = 51, min = 0, width = "100px"),
                                         numericInput("rc_nn", NULL, value = 9, min = 0, width = "100px")
                                     ),
                                     br(),
                                     checkboxInput("CC", "Use Continuity Correction")
                                 )
                        ), "ChiTab"
                    ),
                    convertMenuItem(
                        menuItem("ROC curve", tabName = "RocTAB",
                                 checkboxInput("smooth", "Smooth ROC?", value = T),
                                 checkboxInput("ci", "Plot CIs?", value = F)
                        ), "RocTAB"
                    ),

                    menuItem("A confusion matrix", tabName = "CM", icon = icon("info")),

                    # conditionalPanel("input.sidebarmenu !== 'dashboard'",
                    #                  checkboxInput("plot_xkcd", "Plot xkcd", value = F)
                    # ),

                    menuItem("Cite", icon = icon("info"), tabName = "cite"
                    ),

                    menuItem("Website", icon = icon("chrome"),
                             href = "http://www.semiquant.com")
        )
    ),
    br()
)


body <- dashboardBody(
    dark_grey_edited,
    tags$style(type = "text/css", "
        /* Base text styles */
        body {
            font-size: 14px !important;
            line-height: 1.6 !important;
            color: rgb(205,205,205) !important;
        }
        
        /* Heading hierarchy */
        h2 {
            font-size: 28px !important;
            font-weight: 300 !important;
            margin-bottom: 20px !important;
            color: rgb(198, 253, 168) !important;
        }
        
        h4 {
            font-size: 20px !important;
            font-weight: 400 !important;
            margin: 25px 0 15px !important;
            color: rgb(255,255,255) !important;
        }
        
        h5 {
            font-size: 18px !important;
            font-weight: 500 !important;
            margin: 20px 0 10px !important;
            color: rgb(198, 253, 168) !important;
        }
        
        /* List styling */
        .dashboard-list {
            font-size: 14px !important;
            line-height: 1.8 !important;
            margin: 10px 0 20px 20px !important;
            color: rgb(205,205,205) !important;
        }
        
        .dashboard-list li {
            margin-bottom: 8px !important;
        }
        
        /* Box styling */
        .box {
            margin-bottom: 20px !important;
            background-color: rgb(52,62,72) !important;
        }
        
        /* Description text */
        .section-description {
            font-size: 14px !important;
            color: rgb(180,180,180) !important;
            margin-bottom: 15px !important;
        }
    "),
    # fluidRow("hello"),
    tabItems(
        tabItem(tabName = "dashboard",
                fluidRow(
                    box(width = 12,
                        h2("Diagnostic Estimates and Disease (DEAD)", 
                           align = "center"),
                        p("This application helps clinicians and researchers evaluate diagnostic test performance through several statistical methods:",
                          class = "section-description"),
                        
                        h4("Available Tests:"),
                        
                        h5("1. One Test and Gold Standard"),
                        p("Compare a single diagnostic test against a reference standard (gold standard) to calculate:",
                          class = "section-description"),
                        tags$ul(
                            class = "dashboard-list",
                            tags$li("Sensitivity and Specificity with confidence intervals"),
                            tags$li("Positive and Negative Predictive Values"),
                            tags$li("Positive and Negative Likelihood Ratios"),
                            tags$li("Diagnostic Accuracy and No Information Rate (NIR)")
                        ),
                        
                        h5("2. Two Tests and Gold Standard"),
                        p("Compare two diagnostic tests against a reference standard to evaluate:",
                          class = "section-description"),
                        tags$ul(
                            class = "dashboard-list",
                            tags$li("Individual test performance metrics"),
                            tags$li("Comparative statistics between tests"),
                            tags$li("McNemar's test for paired comparisons"),
                            tags$li("Relative sensitivity and specificity")
                        ),
                        
                        h5("3. Contingency (RxC) Tables"),
                        p("Analyze categorical data using:",
                          class = "section-description"),
                        tags$ul(
                            class = "dashboard-list",
                            tags$li("Chi-square test"),
                            tags$li("Fisher's exact test (for small sample sizes)"),
                            tags$li("Optional continuity correction")
                        ),
                        
                        h5("4. ROC Curve Analysis"),
                        p("Visualize and compare test performance through:",
                          class = "section-description"),
                        tags$ul(
                            class = "dashboard-list",
                            tags$li("ROC curves with optional smoothing"),
                            tags$li("Area Under the Curve (AUC) calculations"),
                            tags$li("Confidence intervals"),
                            tags$li("Statistical comparison between curves")
                        ),
                        
                        br(),
                        h4("Tutorial Video:"),
                        div(style = "display: flex; justify-content: center;",
                            tags$video(src = "DEADTut.mp4", 
                                     type = "mp4", 
                                     autoplay = FALSE, 
                                     controls = TRUE,
                                     poster = "sq_pad.png",
                                     width = "60%")
                        ),
                        
                        br(),
                        div(
                            style = "font-size: 20px;",
                            HTML('<a href="mailto:Jason.Limberis@uct.ac.za?body=&subject=DEAD Suggestion">
                                 Please email if you want to request a specific test or have a suggestion.</a>')
                        ),
                        
                        br(),
                        p("Calculations use the following packages:",
                          style = "font-size: 16px;"),
                        p(
                            style = "font-size: 16px;",
                            tags$a(href="https://www.rdocumentation.org/packages/epiR/versions/0.9-99", "epiR"),
                            ", ",
                            tags$a(href="https://www.rdocumentation.org/packages/DTComPair/versions/1.0.3", "DTComPair"),
                            ", ",
                            tags$a(href="https://www.rdocumentation.org/packages/epitools/versions/0.09", "epitools")
                        )
                    )
                )
        ),


        tabItem(tabName = "DiagTAB1",
            fluidRow(
                box(
                    width = 12,
                    title = "Single Test Diagnostic Performance",
                    status = "info",
                    solidHeader = TRUE,
                    
                    HTML("
                        <div style='font-size: 16px; line-height: 1.5;'>
                            <p>This module evaluates the performance of a single diagnostic test against a gold standard. Enter the test results in the confusion matrix:</p>
                            
                            <ul>
                                <li><strong>True Positives (TP)</strong>: Cases correctly identified as positive</li>
                                <li><strong>False Positives (FP)</strong>: Controls incorrectly identified as positive</li>
                                <li><strong>False Negatives (FN)</strong>: Cases incorrectly identified as negative</li>
                                <li><strong>True Negatives (TN)</strong>: Controls correctly identified as negative</li>
                            </ul>
                            
                            <p>The analysis provides:</p>
                            <ul>
                                <li>Basic diagnostic metrics (sensitivity, specificity, predictive values)</li>
                                <li>Advanced metrics (likelihood ratios, diagnostic odds ratio)</li>
                                <li>Statistical measures (accuracy, kappa, McNemar's test)</li>
                                <li>Confidence intervals based on your selected confidence level</li>
                            </ul>
                        </div>
                    ")
                )
            ),
            fluidRow(
                column(6,
                    sliderInput("CI1", 
                        "Confidence Interval:", 
                        min = 0.8, 
                        max = 0.99, 
                        value = 0.95, 
                        step = 0.01,
                        width = "100%"
                    )
                ),
                column(6,
                    div(style = "margin-top: 20px;",
                        checkboxInput("roundDT1", "Round Values?", value = TRUE)
                    )
                )
            ),
            
            br(),
            h4("Confusion Matrix:"),
            verbatimTextOutput("DTPdt1"),
            verbatimTextOutput("NIR"),
            p("The No Information Rate (NIR) is the accuracy of the model if you set the prediction to the most common disease state in the sample"),
            
            fluidRow(
                column(12,
                    tags$div(
                        style = "margin: 20px 0;",
                        htmlOutput("single_diagnostic_description")
                    )
                )
            ),
            
            h4("Diagnostic Test Results:"),
            dataTableOutput("DTPdt.res1"),
            fluidRow(
                p(class = 'text-center', downloadButton('downloadDT1', 'Download Results'))
            )
        ),

        tabItem(tabName = "DiagTAB2",
                fluidRow(
                    box(
                        width = 12,
                        title = "Two Tests Diagnostic Performance",
                        status = "info",
                        solidHeader = TRUE,
                        
                        HTML("
                            <div style='font-size: 16px; line-height: 1.5;'>
                                <p>This module compares two diagnostic tests against a gold standard. Important notes:</p>
                                
                                <ul>
                                    <li><strong>Missing Values:</strong> Only complete cases are considered - results may differ from individual test analysis</li>
                                    <li><strong>Data Format:</strong> Enter tab-separated data with columns for Gold Standard (0/1), Test1 (0/1), and Test2 (0/1)</li>
                                    <li><strong>Analysis:</strong> Provides comparative statistics and individual test performance</li>
                                </ul>
                            </div>
                        ")
                    )
                ),
                fluidRow(
                    column(6,
                        sliderInput("CI2", 
                            "Confidence Interval:", 
                            min = 0.8, 
                            max = 0.99, 
                            value = 0.95, 
                            step = 0.01,
                            width = "100%"
                        )
                    ),
                    column(6,
                        div(style = "margin-top: 20px;",
                            checkboxInput("roundDT2", "Round Values?", value = TRUE)
                        )
                    )
                ),
                box(title = "Enter Data (tab seperated, you can copy paste from excel)", collapsible = T, collapsed = F, width = 6,
                    aceEditor("DTbin_input", theme = "idle_fingers",
                              value='GoldStandard\tTest1\tTest2\n0\t0\t1\n0\t1\t0\n0\t0\t1\n0\t0\t0\n1\t1\t0\n1\t0\t0\n0\t1\t1\n0\t1\t0\n0\t1\t1\n1\t1\t0\n0\t0\t0\n1\t1\t1\n1\t0\t0\n1\t1\t1\n0\t1\t0\n0\t1\t0\n1\t1\t0\n0\t0\t1\n1\t0\t0\n1\t1\t0\n1\t0\t0\n1\t1\t0\n1\t1\t1\n1\t0\t0\n1\t1\t1\n1\t1\t0\n0\t1\t0\n0\t0\t1\n0\t0\t0\n0\t1\t0\n1\t1\t0\n0\t0\t1\n1\t1\t1\n1\t0\t0\n1\t0\t1\n0\t1\t0\n1\t1\t0\n1\t0\t1\n1\t0\t0\n0\t1\t0\n1\t1\t0\n0\t1\t1\n1\t0\t1\n1\t1\t1\n0\t1\t0\n1\t0\t0\n0\t1\t1\n1\t0\t0\n1\t0\t1\n0\t0\t0\n0\t0\t1\n1\t0\t1\n0\t1\t0\n1\t1\t0\n1\t0\t1\n1\t0\t1\n1\t0\t0\n0\t0\t0\n1\t1\t0\n0\t0\t1\n1\t0\t0\n1\t1\t0\n1\t0\t0\n0\t1\t1\n0\t1\t0\n1\t0\t1\n1\t1\t1\n0\t0\t0\n1\t1\t0\n1\t0\t0\n1\t0\t1\n0\t0\t0\n1\t1\t1\n0\t0\t0\n0\t0\t1\n0\t0\t0\n0\t1\t0\n0\t1\t0\n0\t1\t0\n0\t0\t1\n1\t0\t0\n1\t1\t0\n1\t0\t0\n0\t1\t1\n1\t1\t0\n0\t1\t0\n1\t0\t0\n1\t0\t0\n1\t0\t1\n0\t1\t0\n1\t1\t1\n1\t1\t0\n1\t1\t0\n1\t1\t0\n0\t1\t0\n1\t0\t0\n1\t0\t0\n0\t0\t1\n0\t1\t0\n0\t1\t0\n0\t0\t1\n0\t1\t0\n0\t0\t0\n0\t0\t0\n0\t0\t1\n1\t0\t1\n0\t1\t0\n1\t0\t0\n0\t1\t0\n1\t1\t1\n1\t0\t0\n0\t0\t0\n1\t0\t0\n1\t1\t0\n0\t0\t0\n1\t0\t1\n1\t1\t0\n0\t1\t0\n1\t1\t0\n0\t0\t0\n0\t0\t1\n0\t0\t1\n0\t0\t0\n1\t0\t0\n0\t1\t0\n1\t0\t1\n0\t1\t1\n1\t0\t0\n1\t1\t0\n0\t0\t0\n0\t1\t0\n1\t0\t1\n0\t1\t0\n1\t1\t0\n1\t0\t0\n0\t1\t0\n0\t1\t1\n1\t0\t0\n0\t0\t0\n1\t0\t0\n1\t1\t0\n0\t0\t1\n1\t0\t1\n1\t1\t1\n0\t0\t0\n1\t0\t1\n1\t0\t0\n0\t1\t0\n1\t0\t0\n1\t1\t0\n1\t0\t1\n0\t1\t0\n1\t0\t0\n1\t1\t0\n1\t1\t1\n1\t0\t0\n1\t0\t0\n0\t1\t1\n1\t1\t0\n0\t0\t0\n1\t1\t0\n0\t0\t0\n1\t1\t0\n1\t1\t0\n0\t0\t1\n1\t1\t1\n1\t0\t0\n1\t1\t0\n1\t0\t0\n1\t0\t0\n0\t1\t1\n1\t0\t1\n1\t0\t1\n1\t1\t0\n0\t0\t0\n1\t0\t0\n1\t1\t0\n1\t0\t1\n0\t0\t0\n1\t0\t1\n1\t0\t0\n0\t0\t0\n0\t1\t0\n1\t0\t0\n0\t0\t0\n1\t0\t0\n1\t0\t0\n1\t1\t1\n1\t0\t0\n0\t0\t0\n1\t0\t0\n1\t0\t1\n0\t1\t0\n1\t0\t0\n0\t1\t1\n1\t1\t1\n1\t1\t0',
                              mode="r", height = "765px", fontSize = 14)
                ),
                box(
                    width = 6,
                    title = "Summary Tables",
                    status = "info",
                    solidHeader = TRUE,
                    verbatimTextOutput("ptabA"),
                    hr(),
                    verbatimTextOutput("ptab1")
                ),
                br(),
                box(
                    width = 12,
                    title = "Statistical Comparison",
                    status = "info",
                    solidHeader = TRUE,
                    plotlyOutput("stat.plt1")
                ),
                br(),
                box(
                    width = 12,
                    title = "Comparative Metrics",
                    status = "info",
                    solidHeader = TRUE,
                    dataTableOutput("ptab3"),
                    br(),
                    fluidRow(
                        column(12, align = "center",
                            downloadButton('downloadDT2A', 'Download Comparative Results')
                        )
                    )
                ),
                box(
                    width = 12,
                    title = "Likelihood Ratios",
                    status = "info",
                    solidHeader = TRUE,
                    dataTableOutput("ptab4"),
                    br(),
                    fluidRow(
                        column(12, align = "center",
                            downloadButton('downloadDT2B', 'Download Likelihood Ratio Results')
                        )
                    )
                ),
                fluidRow(
                    column(12,
                        htmlOutput("paired_diagnostic_description")
                    )
                )
        ),


        tabItem(tabName = "ChiTab",
                fluidRow(
                    box(
                        width = 12,
                        title = "Contingency Table Analysis",
                        status = "info",
                        solidHeader = TRUE,
                        style = "background-color: rgb(70,80,90); color: rgb(39,43,48);",
                        
                        HTML("
                            <div style='font-size: 16px; line-height: 1.5; color: rgb(39,43,48);'>
                                <p>This module analyzes 2x2 contingency tables for paired measurements. The analysis automatically selects the appropriate statistical test:</p>
                                
                                <ul>
                                    <li><strong>McNemar's Test</strong>: Used when all cell frequencies are ≥ 5</li>
                                    <li><strong>Exact Binomial Test</strong>: Used when any cell frequency is < 5</li>
                                </ul>
                                
                                <p>The continuity correction option for McNemar's test:</p>
                                <ul>
                                    <li>Recommended for small sample sizes</li>
                                    <li>Makes the test more conservative</li>
                                    <li>May be omitted for larger samples</li>
                                </ul>
                            </div>
                        ")
                    )
                ),
                h4("Statistical Test Results", style = "margin: 20px 0 10px 15px;"),
                dataTableOutput("RCdt"),
                br(),
                div(
                    style = "margin-left: 15px;",
                    checkboxInput("CC", "Use Continuity Correction")
                ),
                column(12,
                    htmlOutput("contingency_description")
                )
        ),


        tabItem(tabName = "RocTAB",
                h4("Basic ROC curve (more options will be added in future)"),
                h5("You can enter data for one or two tests (plus the reference standard)"),
                h6("Please be patient, the bootsrapping for the CIs can take some time"),
                fluidRow(
                    column(12,
                        htmlOutput("roc_description")
                    )
                ),
                plotlyOutput("ROCplotly", height = "650px"),
                br(),
                box(title = "Enter Data (tab seperated)", collapsible = T, collapsed = F, width = 6,
                    aceEditor("roc_input", theme = "idle_fingers",
                              value='Binary_Outcome\tTest1_Measurement\tTest2_Measurement\n0\t1.04020064033914\t0.171221288894348\n1\t1.83311097391425\t-0.914241155526571\n0\t-0.144033552345583\t0.25809586463931\n0\t-0.270598823329474\t-0.307250859212018\n1\t0.511717230024483\t0.657596211978792\n1\t2.06696372908171\t0.448140057797307\n0\t-0.543155753506823\t1.32393520304767\n0\t0.00190374750531683\t1.44912259996019\n0\t-0.375643283115991\t0.269397258670726\n0\t-0.398665842316117\t-0.388621465553968\n1\t1.57300817770224\t0.263187154838907\n0\t-0.534717588025751\t0.833952376311108\n0\t1.14701516869888\t0.00468717035712192\n1\t1.10955041246104\t-0.999369152096015\n0\t0.851217272327907\t-0.605311810543099\n0\t-1.53604462374537\t-0.264393003531742\n1\t1.68380515251503\t0.681710374553192\n1\t0.308179838422192\t-1.56499172452939\n0\t-1.45680935364301\t-1.06477005412532\n0\t0.108318448335247\t-0.196861914432111\n1\t1.35404558170149\t0.14337355340071\n1\t0.43248744141443\t-1.42209121752243\n0\t-0.56467351273929\t0.731922151281943\n0\t-0.946408909823568\t0.763134566140334\n1\t0.736165002180433\t0.0227805256115958\n0\t1.00009611863722\t-1.69695453235236\n0\t0.756088823715481\t0.527892585851331\n0\t0.620695503057128\t0.796732922414013\n0\t0.00818870683472545\t-0.658182602850308\n1\t1.2752076278957\t0.277873303001804\n0\t0.912592413849882\t0.158428197301852\n1\t1.36352108223142\t-0.670145423883167\n0\t0.255035545332107\t-0.351271147922691\n1\t1.73770545332842\t-0.13421867974992\n1\t1.76315977579888\t1.12164638410417\n1\t1.03175252100735\t0.000633516893881228\n1\t0.431647484457911\t-0.549499295218911\n1\t0.78630069212112\t0.804030047414196\n0\t0.24763372645939\t0.561871674777024\n0\t0.663548041645065\t-0.093763849457033\n1\t1.72905731629581\t0.637460363737535\n1\t1.8519896176992\t-1.37767304559956\n0\t0.534982160411556\t0.244934783802311\n1\t1.47758479712574\t-0.404793716855123\n1\t0.78905476878702\t-0.317492470702907\n0\t-0.43802655579879\t1.0966334166439\n0\t-2.10991537547845\t-0.451609235045044\n0\t-0.00444067451819973\t1.02558336198209\n0\t-0.999497500268568\t-0.858385250586007\n0\t-0.884728098633616\t0.280875101130759\n0\t-0.285578915173303\t-0.373720435006646\n1\t0.445818894033849\t-1.62721903267105\n1\t0.156506255679849\t0.211703699248015\n1\t0.614440257490603\t0.072918212061498\n0\t-0.385195063645805\t-0.653303695757576\n0\t0.430277582938107\t-0.174121874031626\n1\t0.752068222263921\t0.754696171893968\n0\t-0.639652560697203\t-1.7250544707174\n0\t-0.518540707547305\t-0.0763266195831492\n1\t0.31752041285114\t-0.0337197805552862\n1\t0.875919002659428\t0.120503148787822\n0\t-0.149350650067123\t-0.368329148264048\n1\t0.745066597548203\t0.488569844410021\n0\t0.125560058061372\t1.37688234269973\n0\t0.586325831313102\t-0.85926614713308\n0\t0.162786148188314\t1.45710933198272\n0\t-0.400054541288466\t0.0632259635375114\n0\t-0.633048265358359\t-0.425322904470776\n1\t0.573484420859957\t2.08921261164016\n0\t1.23437504536218\t0.444475516862589\n1\t1.33847231746514\t1.9845828294218\n1\t1.20390414254367\t1.7328458246668\n1\t1.11821977407611\t-0.204688191634045\n1\t1.02462988981841\t-1.99711770112223\n1\t1.24398979671444\t-0.0404157279696715\n0\t0.799250462675709\t1.23564774004061\n0\t0.431149373897686\t-0.939244478952344\n0\t0.976607386963819\t-0.177274885023958\n1\t0.411232505428758\t0.962636750199723\n1\t1.03264061520594\t0.914051195115594\n1\t0.414610332663507\t0.0743359472302731\n0\t-0.145669354852768\t2.30584628800616\n0\t1.1637496956986\t-1.73246686908468\n1\t1.2638513513269\t1.19596823307386\n0\t0.128435756734407\t0.0956436321223947\n1\t1.47816003172029\t-0.693537976846022\n1\t1.51696543445388\t1.09364863658484\n0\t0.362422195982443\t0.420374866278553\n0\t0.730151179948551\t0.22445707071562\n0\t0.319879988545024\t0.430290772287053\n1\t0.895614207638148\t-0.53205433447241\n0\t-0.162759844337421\t-0.136499611552568\n1\t-0.0571185638843614\t2.80971376602408\n1\t0.914222811183992\t1.55905816510734\n1\t0.171704562798926\t0.335005634556407\n0\t-0.713118219170477\t0.715945087612034\n1\t0.856239332642641\t-0.228807127258931\n1\t0.729381795996963\t-0.579493358449346\n1\t-0.397814592416522\t-0.758369331923647\n1\t1.12720381369141\t-0.808448833668536\n1\t0.301501568840812\t-0.0451164003287148\n0\t0.32383613260016\t-1.4366744594223\n1\t0.959950656507807\t-0.703548745422501\n0\t-0.268360543842611\t-1.07535368337909\n0\t-0.534397374175856\t-0.483218181346818\n1\t0.474831008638435\t0.718990065672899\n1\t1.41570741939695\t0.131520451048407\n0\t0.157714396765198\t0.555513077551223\n1\t0.385812954391714\t0.339366185062407\n0\t0.595871012207413\t0.149944436742496\n0\t-0.154081478207697\t-0.240636050124581\n1\t0.394232221812296\t-0.294717674120677\n1\t1.09930293040327\t0.400394621039983\n0\t0.379581408810956\t-0.86196900683275\n1\t1.27560249003553\t0.200309589048341\n1\t1.20378822379476\t0.237799119402684\n1\t1.51057364238005\t0.245074545885559\n0\t-0.398518723906593\t-1.47394477488295\n1\t1.01419554372281\t-0.7076445580596\n1\t0.111821257673198\t-1.20761549585326\n0\t-0.572091614556677\t1.04708534693935\n0\t0.37210889797258\t1.22816480222226\n0\t0.0344086323485963\t1.40453428834981\n1\t1.94463055190526\t0.40046317579249\n0\t0.637931089253936\t0.851203513773733\n0\t-0.955297194950908\t0.501086159966714\n0\t-0.346963568716593\t-0.0159967001080212\n0\t0.485899102090022\t0.052949993396038\n1\t1.9959222760624\t1.39732544416688\n0\t0.374736195130339\t-2.06596023158259\n1\t1.47983890411873\t0.335794181350027\n0\t-0.0237319804435941\t0.364373866930544\n0\t0.340922666485959\t-0.217870935416996\n1\t2.24680618737367\t2.25435382200633\n1\t0.646978690657485\t-0.329063559701014\n0\t-0.865646465914081\t-0.327405965318202\n1\t1.52302256356239\t-2.50980408192408\n1\t0.441357814812996\t-0.0270610240255563\n0\t-0.956171722135821\t-0.0506064776676199\n0\t-0.564641306828936\t1.78441619897331\n0\t-0.158901873658287\t0.332412643402388\n0\t-0.0184350511960584\t0.832487341322507\n0\t0.77628421565948\t-1.16270388786696\n1\t0.969300652111354\t0.849064022365464\n1\t1.0247727667547\t-1.68099261014109\n1\t1.53694771893874\t-0.924724954361048\n1\t2.48327218041238\t0.269374872909007\n1\t1.04207327683692\t-0.57802670500125\n1\t0.683062521635454\t-0.656355557329612\n1\t-0.254127335881163\t-0.676517382931893\n1\t1.70980153025112\t2.09586819821605\n0\t-0.125896368364301\t1.16586749235039\n0\t1.16869380586746\t-1.16757237830866\n1\t2.45637884974025\t0.879259637021754\n1\t0.775314215858415\t-0.781630722215035\n0\t0.191094547752889\t0.623089050150368\n1\t1.34433186390572\t0.078962412037005\n1\t0.105977790000903\t-1.65257022538256\n1\t2.29162560686501\t-0.390669993125154\n1\t0.383882224391957\t-0.124097514868261\n1\t0.680974835281014\t0.236648764063305\n1\t0.645110094825933\t0.0599838029995199\n1\t-1.00336927062398\t0.561646493991906\n0\t-0.240559619669408\t0.41902154548191\n1\t2.03353594443969\t0.140719402525697\n1\t2.1001714438645\t0.928025048480452\n1\t2.00444828108794\t0.147307309900475\n1\t1.4311945563476\t0.884838794323142\n1\t0.310775902660666\t0.18863872698444\n1\t-0.0160167868294057\t0.895283893491652\n1\t-0.105701850723356\t1.47540627358412\n0\t-0.246599907726367\t-0.396870339719621\n0\t0.0717166081171206\t0.393572749497883\n0\t0.175267133251754\t-0.38369369990041\n0\t0.383572536495523\t1.10985646009798\n0\t-0.0229538425354207\t0.198267537662544\n0\t-0.466203731192325\t-0.164350826297096\n0\t-0.0257841950133965\t0.298275858609804\n1\t0.466794439840599\t1.34009421617769\n1\t1.29741169059447\t0.257883052535128\n0\t-0.196034808337352\t0.7130397888969\n1\t-0.00928543556179706\t-0.567316023953515\n1\t1.27753409802778\t-0.894741156523835\n1\t0.691842928628322\t0.121297006347269\n1\t2.16541609232687\t-0.940297858073652\n0\t0.237728983418603\t-1.49045178497471\n0\t0.932283926677191\t0.221786195644812\n0\t-1.16663269417225\t-1.23663595061804\n0\t0.657587555556791\t0.245956046949895\n1\t0.344359281661996\t2.06941748411479\n1\t1.47387679253299\t-0.0188751556289984\n0\t0.578170154853085\t0.352246704075223\n1\t0.952407090496505\t1.0232028800684\n1\t1.91984981873486\t0.273511756932763\n0\t0.134340070515655\t0.45812409117873\n1\t0.772540624935611\t1.03389549274442\n0\t-1.08651841554356\t-0.614708273981645\n1\t0.200005062594272\t-1.2839644693252\n0\t-0.95659307039884\t-0.617715086743221\n0\t-0.127096708593605\t0.283408577232218',
                              mode="r", height = "300px", fontSize = 14)),

                box(title = "Data Entered", collapsible = T, collapsed = T, width = 6,
                    verbatimTextOutput("ROCdt"))
        ),


        tabItem(tabName = "CM",
                img(src='ConfusionMatrix_MyEdit.png', align = "center", height="80%", width="80%")
        )




    )
)


dashboardPage(
    title=title,
    header,
    sidebar,
    body
)
























