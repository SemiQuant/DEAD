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
title <- "Diagnostic Estimates and Disease (DEAD)"
#  link not working
header <- dashboardHeader(title =  div(img(src = "sq.png", height="42.885", width="34.29"), title), titleWidth = 450,
                          dropdownMenu(type = "notifications", icon = icon("medkit"), badgeStatus = NULL,
                                       notificationItem(text = "Center for Lung Infection and Immunity",
                                                        href = "http://lunginstitute.co.za/liiu/")
                          ),
                          dropdownMenu(type = "notifications", icon = icon("copyright"), badgeStatus = NULL,
                                       notificationItem(text = "Copyright 2019")
                          )
)

sidebar <- dashboardSidebar(
    width = 300,
    h3("Select a Test", align = "center"),
    dashboardSidebar(
        sidebarMenu(id = "sidebarmenu",
                    menuItem("Dashboard", tabName = "dashboard", icon = icon("home")),
                    # menuItem("Other Cals", tabName = "Ocalc",

                    menuItem("Diagnostic Test Performance", tabName = "DT",
                             menuItem("One Test and Gold Standard", tabName = "DiagTAB1"),
                             conditionalPanel("input.sidebarmenu === 'DiagTAB1'",
                                              menuItem("Enter data below using tab seperation:"),
                                              aceEditor("dtp_input",  theme = "idle_fingers",
                                                        value='Res \tPositive\tNegative\nPositive\t13\t12\nNegative\t41\t33', mode="r", height = "150px", fontSize = 14),
                                              checkboxInput("longIn", "Use Long Input Format (see box in panel to the right)")
                             ),
                             menuItem("Two Tests and Gold Standard", tabName = "DiagTAB2")
                    ),

                    menuItem("Contigency (RxC) Tables", tabName = "ChiTab"),
                    conditionalPanel("input.sidebarmenu === 'ChiTab'",
                                     menuItem("Enter data below using tab seperation:"),
                                     aceEditor("rc_input",  theme = "idle_fingers",
                                               value='Res \tPositive\tNegative\nPositive\t13\t21\nNegative\t51\t9', mode="r", height = "150px", fontSize = 14),
                                     checkboxInput("CC", "Use Continuity Correction")
                    ),

                    menuItem("ROC curve", tabName = "RocTAB"),
                    conditionalPanel("input.sidebarmenu === 'RocTAB'"
                    ),
                    # ),

                    menuItem("A confusion matrix", tabName = "CM", icon = icon("info")),

                    conditionalPanel("input.sidebarmenu !== 'dashboard'",
                                     checkboxInput("plot_xkcd", "Plot xkcd", value = F)
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
             .irs-max {font-family: 'arial'; color: white;}
             .irs-min {font-family: 'arial'; color: white;}
             "),
    # fluidRow("hello"),
    tabItems(
        tabItem(tabName = "dashboard",
                h3("This app allows you determine the performace of a diagnostic test(s)."),
                br(),
                tags$video(src = "DEADTut.mp4", type = "mp4", autoplay = F, controls = T,
                            poster = "sq_pad.png",width = "100%"), #  height ="42%"
                br(),
                "This app is currently under development and intended for use by members of CLII only.",
                br(),
                HTML(
                    '<a href="mailto:Jason.Limberis@uct.ac.za?
              body=""
              &subject="DEAD Suggestion">Please email if you want to request a specific test or have a suggestion.</a>'
                ),
                br(),
                "Calculations use the following packages: ",
                tags$a(href="https://www.rdocumentation.org/packages/epiR/versions/0.9-99", "epiR, "),
                tags$a(href="https://www.rdocumentation.org/packages/DTComPair/versions/1.0.3", "DTComPair, "),
                tags$a(href="https://www.rdocumentation.org/packages/epitools/versions/0.09", "epitools.")
        ),


        tabItem(tabName = "DiagTAB1",
                fluidRow(
                    column(3,
                           sliderInput("CI1", "Confidence Interval:", min = 0.8, max = 0.99, 0.95, step = 0.05)),
                    column(9,
                           box(title = "Enter data here if long entry selected on left panel (tab seperated, you can copy paste from excel)", collapsible = T, collapsed = T, width = 6,
                    aceEditor("DT1.long", theme = "idle_fingers",
                              value='GoldStandard\tTest\n0\t0\n1\t0\n0\t0\n1\t0\n1\t0\n1\t0\n1\t0\n1\t0\n0\t0\n0\t0\n1\t0\n1\t1\n0\t0\n0\t0\n1\t0\n1\t0\n1\t1\n1\t0\n0\t0\n0\t1\n1\t1\n0\t0\n1\t0\n0\t0\n1\t0\n1\t0\n0\t0\n0\t0\n0\t0\n1\t0\n1\t0\n0\t0\n0\t0\n1\t0\n1\t1\n0\t0\n0\t0\n1\t0\n1\t0\n0\t1\n1\t0\n1\t1\n1\t0\n0\t0\n1\t0\n0\t1\n1\t1\n1\t0\n0\t0\n1\t1\n0\t0\n1\t0\n0\t0\n0\t0\n1\t0\n0\t0\n0\t0\n1\t0\n1\t1\n1\t1\n1\t0\n1\t0\n0\t0\n1\t0\n0\t0\n0\t0\n0\t0\n1\t1\n1\t0\n1\t0\n1\t0\n1\t0\n1\t0\n0\t1\n0\t1\n1\t0\n0\t1\n0\t0\n1\t0\n1\t0\n1\t1\n0\t0\n1\t0\n1\t0\n0\t0\n1\t1\n1\t0\n0\t1\n1\t0\n1\t1\n1\t0\n1\t0\n1\t0\n0\t1\n0\t0\n0\t0\n0\t1\n1\t0\n0\t0\n0\t0\n1\t1\n0\t0\n0\t1\n1\t0\n1\t0\n1\t0\n0\t0\n1\t1\n0\t0\n1\t0\n0\t0\n1\t0\n0\t1\n1\t0\n0\t0\n1\t1\n0\t0\n0\t0\n1\t0\n1\t0\n0\t0\n1\t0\n0\t0\n0\t0\n0\t0\n1\t0\n0\t0\n0\t0\n1\t0\n1\t0\n1\t0\n1\t0\n0\t1\n0\t1\n0\t0\n1\t0\n0\t1\n1\t1\n1\t0\n1\t0\n1\t1\n1\t0\n0\t1\n0\t0\n0\t1\n0\t0\n0\t0\n0\t0\n1\t0\n1\t1\n1\t0\n1\t1\n0\t1\n1\t0\n1\t0\n1\t0\n1\t0\n1\t0\n1\t0\n1\t0\n1\t1\n1\t0\n0\t0\n0\t0\n1\t0\n1\t0\n1\t0\n0\t0\n1\t0\n1\t1\n0\t0\n1\t0\n0\t0\n0\t1\n0\t0\n1\t0\n0\t1\n1\t0\n1\t0\n0\t0\n1\t0\n1\t0\n1\t0\n0\t0\n1\t0\n0\t1\n0\t0\n1\t0\n0\t0\n0\t0\n1\t0\n1\t0\n1\t1\n0\t0\n0\t1\n1\t0\n0\t0\n1\t0\n0\t0\n0\t0\n',
                              mode="r", height = "600px", fontSize = 14)))
                ),
                br(),
                checkboxInput("roundDT1", "Round Values?", value = T),
                br(),
                verbatimTextOutput("DTPdt1"),
                verbatimTextOutput("NIR"),
                "The NIR is the accuraccy of the model if you set the prediction to the most common disease state in the sample",
                br(),
                dataTableOutput("DTPdt.res1"),
                fluidRow(
                    p(class = 'text-center', downloadButton('downloadDT1', 'Download Data Table'))
                )
        ),

        tabItem(tabName = "DiagTAB2",
                h5("If you data has missing values, then the estimates here will differ from checking against just the reference standard as only complete cases are considered for these statistics"),
                sliderInput("CI2", "Confidence Interval:", min = 0.8, max = 0.99, 0.95, step = 0.05),
                box(title = "Enter Data (tab seperated, you can copy paste from excel)", collapsible = T, collapsed = F, width = 6,

                    aceEditor("DTbin_input", theme = "idle_fingers",
                              value='GoldStandard\tTest1\tTest2\n0\t0\t1\n0\t1\t0\n0\t0\t1\n0\t0\t0\n1\t1\t0\n1\t0\t0\n0\t1\t1\n0\t1\t0\n0\t1\t1\n1\t1\t0\n0\t0\t0\n1\t1\t1\n1\t0\t0\n1\t1\t1\n0\t1\t0\n0\t1\t0\n1\t1\t0\n0\t0\t1\n1\t0\t0\n1\t1\t0\n1\t0\t0\n1\t1\t0\n1\t1\t1\n1\t0\t0\n1\t1\t1\n1\t1\t0\n0\t1\t0\n0\t0\t1\n0\t0\t0\n0\t1\t0\n1\t1\t0\n0\t0\t1\n1\t1\t1\n1\t0\t0\n1\t0\t1\n0\t1\t0\n1\t1\t0\n1\t0\t1\n1\t0\t0\n0\t1\t0\n1\t1\t0\n0\t1\t1\n1\t0\t1\n1\t1\t1\n0\t1\t0\n1\t0\t0\n0\t1\t1\n1\t0\t0\n1\t0\t1\n0\t0\t0\n0\t0\t1\n1\t0\t1\n0\t1\t0\n1\t1\t0\n1\t0\t1\n1\t0\t1\n1\t0\t0\n0\t0\t0\n1\t1\t0\n0\t0\t1\n1\t0\t0\n1\t1\t0\n1\t0\t0\n0\t1\t1\n0\t1\t0\n1\t0\t1\n1\t1\t1\n0\t0\t0\n1\t1\t0\n1\t0\t0\n1\t0\t1\n0\t0\t0\n1\t1\t1\n0\t0\t0\n0\t0\t1\n0\t0\t0\n0\t1\t0\n0\t1\t0\n0\t1\t0\n0\t0\t1\n1\t0\t0\n1\t1\t0\n1\t0\t0\n0\t1\t1\n1\t1\t0\n0\t1\t0\n1\t0\t0\n0\t1\t0\n1\t0\t0\n1\t0\t1\n0\t1\t0\n1\t1\t1\n1\t1\t0\n1\t1\t0\n1\t1\t0\n0\t1\t0\n1\t0\t0\n1\t0\t0\n0\t0\t1\n0\t1\t0\n0\t1\t0\n0\t0\t1\n0\t1\t0\n0\t0\t0\n0\t0\t0\n0\t0\t0\n0\t0\t1\n1\t0\t1\n0\t1\t0\n1\t0\t0\n0\t1\t0\n1\t1\t1\n1\t0\t0\n0\t0\t0\n1\t0\t0\n1\t1\t0\n0\t0\t0\n1\t0\t1\n1\t1\t0\n0\t1\t0\n1\t1\t0\n0\t0\t0\n0\t0\t1\n0\t0\t1\n0\t0\t0\n1\t0\t0\n0\t1\t0\n1\t0\t1\n0\t1\t1\n1\t0\t0\n1\t1\t0\n0\t0\t0\n0\t1\t0\n1\t0\t1\n0\t1\t0\n1\t1\t0\n1\t0\t0\n0\t1\t0\n0\t1\t1\n1\t0\t0\n0\t0\t0\n1\t0\t0\n1\t1\t0\n0\t0\t1\n1\t0\t1\n1\t1\t1\n0\t0\t0\n1\t0\t1\n1\t0\t0\n0\t1\t0\n1\t0\t0\n1\t1\t0\n1\t0\t1\n0\t1\t0\n1\t0\t0\n1\t1\t0\n1\t1\t0\n1\t1\t1\n1\t0\t0\n1\t0\t0\n0\t1\t1\n1\t1\t0\n0\t0\t0\n1\t1\t0\n0\t0\t0\n1\t1\t0\n1\t1\t0\n0\t0\t1\n1\t1\t1\n1\t0\t0\n1\t1\t0\n1\t0\t0\n1\t0\t0\n0\t1\t1\n1\t0\t1\n1\t0\t1\n1\t1\t0\n0\t0\t0\n1\t0\t0\n1\t1\t0\n1\t0\t1\n0\t0\t0\n1\t0\t1\n1\t0\t0\n0\t0\t0\n0\t1\t0\n1\t0\t0\n0\t0\t0\n1\t0\t0\n1\t0\t0\n1\t1\t1\n1\t0\t0\n0\t0\t0\n1\t0\t0\n1\t0\t1\n0\t1\t0\n1\t0\t0\n0\t1\t1\n1\t1\t1\n1\t1\t0',
                              mode="r", height = "765px", fontSize = 14)),
                # box(title = "Enter Data (tab seperated, you can copy paste from excel)", collapsible = T, collapsed = F, width = 6, height = "300px",
                # verbatimTextOutput("ROCdt"))

                verbatimTextOutput("ptabA"),
                verbatimTextOutput("ptab1"),
                br(),

                plotlyOutput("stat.plt1"),
                br(),
                br(),
                # br(),
                # br(),
                checkboxInput("roundDT2", "Round Values?", value = T),
                dataTableOutput("ptab3"),
                fluidRow(
                    p(class = 'text-center', downloadButton('downloadDT2A', 'Download Data Table A'))
                ),
                dataTableOutput("ptab4"),
                fluidRow(
                    p(class = 'text-center', downloadButton('downloadDT2B', 'Download Data Table B'))
                )

        ),

        tabItem(tabName = "ChiTab",
                h4("Results (if any cell contains a value less than 5, Exact Binomial test is performed as opposed to McNemar Chi Square"),
                # h4("Results (if there are less that 20 tests, the Exact Binomial test is performed as opposed to McNemar Chi Square"),
                verbatimTextOutput("RCdt"),
                h5("Select Conitnuity Correction if necessary")
        ),


        tabItem(tabName = "RocTAB",
                h4("Basic ROC curve (more options will be added in future)"),
                plotOutput("ROCplot"),
                # h4("Results (if any cell contains a value less than 5, Fishers Exact test is performed as opposed to Persons Chi Square"),
                box(title = "Enter Data (tab seperated)", collapsible = T, collapsed = F, width = 6,
                    aceEditor("roc_input", theme = "idle_fingers",
                              value='Binary_Outcome\tTest_Measurement\n0\t1.04020064033914\n1\t1.83311097391425\n0\t-0.144033552345583\n0\t-0.270598823329474\n1\t0.511717230024483\n1\t2.06696372908171\n0\t-0.543155753506823\n0\t0.00190374750531683\n0\t-0.375643283115991\n0\t-0.398665842316117\n1\t1.57300817770224\n0\t-0.534717588025751\n0\t1.14701516869888\n1\t1.10955041246104\n0\t0.851217272327907\n0\t-1.53604462374537\n1\t1.68380515251503\n1\t0.308179838422192\n0\t-1.45680935364301\n0\t0.108318448335247\n1\t1.35404558170149\n1\t0.43248744141443\n0\t-0.56467351273929\n0\t-0.946408909823568\n1\t0.736165002180433\n0\t1.00009611863722\n0\t0.756088823715481\n0\t0.620695503057128\n0\t0.00818870683472545\n1\t1.2752076278957\n0\t0.912592413849882\n1\t1.36352108223142\n0\t0.255035545332107\n1\t1.73770545332842\n1\t1.76315977579888\n1\t1.03175252100735\n1\t0.431647484457911\n1\t0.78630069212112\n0\t0.24763372645939\n0\t0.663548041645065\n1\t1.72905731629581\n1\t1.8519896176992\n0\t0.534982160411556\n1\t1.47758479712574\n1\t0.78905476878702\n0\t-0.43802655579879\n0\t-2.10991537547845\n0\t-0.00444067451819973\n0\t-0.999497500268568\n0\t-0.884728098633616\n0\t-0.285578915173303\n1\t0.445818894033849\n1\t0.156506255679849\n1\t0.614440257490603\n0\t-0.385195063645805\n0\t0.430277582938107\n1\t0.752068222263921\n0\t-0.639652560697203\n0\t-0.518540707547305\n1\t0.31752041285114\n1\t0.875919002659428\n0\t-0.149350650067123\n1\t0.745066597548203\n0\t0.125560058061372\n0\t0.586325831313102\n0\t0.162786148188314\n0\t-0.400054541288466\n0\t-0.633048265358359\n1\t0.573484420859957\n0\t1.23437504536218\n1\t1.33847231746514\n1\t1.20390414254367\n1\t1.11821977407611\n1\t1.02462988981841\n1\t1.24398979671444\n0\t0.799250462675709\n0\t0.431149373897686\n0\t0.976607386963819\n1\t0.411232505428758\n1\t1.03264061520594\n1\t0.414610332663507\n0\t-0.145669354852768\n0\t1.1637496956986\n1\t1.2638513513269\n0\t0.128435756734407\n1\t1.47816003172029\n1\t1.51696543445388\n0\t0.362422195982443\n0\t0.730151179948551\n0\t0.319879988545024\n1\t0.895614207638148\n0\t-0.162759844337421\n1\t-0.0571185638843614\n1\t0.914222811183992\n1\t0.171704562798926\n0\t-0.713118219170477\n1\t0.856239332642641\n1\t0.729381795996963\n1\t-0.397814592416522\n1\t1.12720381369141\n1\t0.301501568840812\n0\t0.32383613260016\n1\t0.959950656507807\n0\t-0.268360543842611\n0\t-0.534397374175856\n1\t0.474831008638435\n1\t1.41570741939695\n0\t0.157714396765198\n1\t0.385812954391714\n0\t0.595871012207413\n0\t-0.154081478207697\n1\t0.394232221812296\n1\t1.09930293040327\n0\t0.379581408810956\n1\t1.27560249003553\n1\t1.20378822379476\n1\t1.51057364238005\n0\t-0.398518723906593\n1\t1.01419554372281\n1\t0.111821257673198\n0\t-0.572091614556677\n0\t0.37210889797258\n0\t0.0344086323485963\n1\t1.94463055190526\n0\t0.637931089253936\n0\t-0.955297194950908\n0\t-0.346963568716593\n0\t0.485899102090022\n1\t1.9959222760624\n0\t0.374736195130339\n1\t1.47983890411873\n0\t-0.0237319804435941\n0\t0.340922666485959\n1\t2.24680618737367\n1\t0.646978690657485\n0\t-0.865646465914081\n1\t1.52302256356239\n1\t0.441357814812996\n0\t-0.956171722135821\n0\t-0.564641306828936\n0\t-0.158901873658287\n0\t-0.0184350511960584\n0\t0.77628421565948\n1\t0.969300652111354\n1\t1.0247727667547\n1\t1.53694771893874\n1\t2.48327218041238\n1\t1.04207327683692\n1\t0.683062521635454\n1\t-0.254127335881163\n1\t1.70980153025112\n0\t-0.125896368364301\n0\t1.16869380586746\n1\t2.45637884974025\n1\t0.775314215858415\n0\t0.191094547752889\n1\t1.34433186390572\n1\t0.105977790000903\n1\t2.29162560686501\n1\t0.383882224391957\n1\t0.680974835281014\n1\t0.645110094825933\n1\t-1.00336927062398\n0\t-0.240559619669408\n1\t2.03353594443969\n1\t2.1001714438645\n1\t2.00444828108794\n1\t1.4311945563476\n1\t0.310775902660666\n1\t-0.0160167868294057\n1\t-0.105701850723356\n0\t-0.246599907726367\n0\t0.0717166081171206\n0\t0.175267133251754\n0\t0.383572536495523\n0\t-0.0229538425354207\n0\t-0.466203731192325\n0\t-0.0257841950133965\n1\t0.466794439840599\n1\t1.29741169059447\n0\t-0.196034808337352\n1\t-0.00928543556179706\n1\t1.27753409802778\n1\t0.691842928628322\n1\t2.16541609232687\n0\t0.237728983418603\n0\t0.932283926677191\n0\t-1.16663269417225\n0\t0.657587555556791\n1\t0.344359281661996\n1\t1.47387679253299\n0\t0.578170154853085\n1\t0.952407090496505\n1\t1.91984981873486\n0\t0.134340070515655\n1\t0.772540624935611\n0\t-1.08651841554356\n1\t0.200005062594272\n0\t-0.95659307039884\n0\t-0.127096708593605\n',
                              mode="r", height = "300px", fontSize = 14)),

                box(title = "Data Entered", collapsible = T, collapsed = T, width = 6, height = "300px",
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
























