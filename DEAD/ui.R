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
                                               value='Res \tPositive\tNegative\nPositive\t3\t1\nNegative\t1\t3', mode="r", height = "150px", fontSize = 14),
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
                h3("This app allows you..."),
                br(),
                # tags$video(src = "SampleSizeCalcTut.mp4", type = "mp4", autoplay = F, controls = T,
                #            width = "100%", poster = "sq_pad.png"),
                br(),
                "This app is currently under development and indednded for use by members of CLII only",

                HTML(
                    '<a href="mailto:Jason.Limberis@uct.ac.za?
              body=""
              &subject="Sample Size Calculator Suggestion">Please email if you want to request a specific test or have a suggestion.</a>'
                ),
                br(),
                "Calculations use the following packages: "
                # tags$a(href="https://www.rdocumentation.org/packages/pwr/versions/1.2-2", "pwr, "),
                # tags$a(href="https://www.rdocumentation.org/packages/samplingbook/versions/1.2.2", "samplingbook, "),
                # tags$a(href="https://www.rdocumentation.org/packages/RnaSeqSampleSize/versions/1.4.2", "RnaSeqSampleSize, "),
                # tags$a(href="https://www.rdocumentation.org/packages/Hmisc/versions/4.1-1", "Hmisc")
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
                verbatimTextOutput("DTPdt1"),
                dataTableOutput("DTPdt.res1")
        ),

        tabItem(tabName = "DiagTAB2",
                h5("If you data has missing values, then the estimates here will differ from chacking against just the reference standars as only complete data can be considered for these statistics"),
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
                checkboxInput("roundDT2", "Round Values?", value = F),
                dataTableOutput("ptab3"),
                dataTableOutput("ptab4")

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
                              value='ID \tBinary_Outcome\tMeasurement\n1\t1\t1.481171553\n2\t1\t0.619944779\n3\t0\t0.576133446\n4\t1\t0.854331968\n5\t0\t0.052583416\n6\t1\t0.667039885\n7\t0\t-0.475723066\n8\t0\t0.282624226\n9\t1\t1.700781111\n10\t0\t0.768579848\n11\t1\t1.398228208\n12\t0\t-0.815113652\n13\t0\t-0.706146391\n14\t1\t1.092136485\n15\t1\t0.282118377\n16\t0\t-0.033372609\n17\t0\t-0.096828085\n18\t0\t-0.604980428\n19\t0\t-0.513515553\n20\t0\t0.042171241\n21\t0\t-0.186546032\n22\t1\t2.35004223\n23\t0\t-0.966998716\n24\t0\t0.891914592\n25\t0\t-1.068576763\n26\t1\t0.386631328\n27\t1\t0.829380319\n28\t0\t0.15875305\n29\t1\t2.447354063\n30\t1\t1.84672233\n31\t1\t1.702146679\n32\t1\t0.90324187\n33\t1\t2.285856971\n34\t1\t-0.20818886\n35\t1\t0.667847613\n36\t1\t0.295229787\n37\t1\t0.841094252\n38\t0\t-0.891944068\n39\t0\t-0.140047105\n40\t0\t1.264118961\n41\t0\t-0.434589323\n42\t1\t1.205499205\n43\t0\t0.330347098\n44\t1\t1.041408624\n45\t0\t0.337708654\n46\t1\t1.752567495\n47\t0\t-0.170272521\n48\t0\t0.391121689\n49\t0\t0.624743054\n50\t1\t1.550229788\n51\t1\t0.85446841\n52\t1\t2.810105434\n53\t1\t0.688185414\n54\t0\t0.386725378\n55\t1\t0.887024798\n56\t1\t2.603648963\n57\t1\t1.735326242\n58\t1\t1.639552284\n59\t0\t-0.217211832\n60\t0\t-0.591994683\n61\t1\t0.718613019\n62\t0\t0.852983735\n63\t1\t1.491381743\n64\t0\t-0.288554394\n65\t1\t1.536258558\n66\t1\t0.073664556\n67\t1\t-0.259730226\n68\t0\t0.040533117\n69\t0\t-0.654513463\n70\t1\t-0.357453037\n71\t0\t-0.202289128\n72\t1\t0.812745317\n73\t0\t-0.412930199\n74\t0\t0.296033321\n75\t0\t-0.35045817\n76\t1\t1.29873851\n77\t1\t1.896832343\n78\t1\t1.035243927\n79\t0\t0.242812343\n80\t0\t0.496303914\n81\t1\t0.748642302\n82\t0\t0.038829544\n83\t0\t-0.132682315\n84\t1\t-0.077457107\n85\t0\t0.332345957\n86\t1\t0.165002878\n87\t1\t1.234293994\n88\t0\t0.183468174\n89\t1\t-0.190759707\n90\t1\t0.432842698\n91\t0\t0.01682117\n92\t1\t1.300514855\n93\t1\t0.292109261\n94\t1\t1.21878156\n95\t0\t0.050928762\n96\t1\t0.274880042\n97\t1\t0.10478353\n98\t0\t0.225434758\n99\t0\t-0.924193578\n100\t0\t1.213002454\n101\t1\t0.276541973\n102\t0\t0.133165005\n103\t0\t-0.359118105\n104\t1\t0.611132537\n105\t1\t0.705843788\n106\t0\t0.008265075\n107\t0\t-0.513549832\n108\t1\t1.672671601\n109\t1\t1.365784359\n110\t0\t-1.29940313\n111\t1\t0.22059252\n112\t1\t0.184674342\n113\t1\t1.116934381\n114\t1\t1.142888595\n115\t0\t-0.732284738\n116\t0\t0.819758701\n117\t0\t-0.100340099\n118\t1\t0.594980261\n119\t0\t0.94571945\n120\t1\t1.717126988\n121\t1\t1.347012593\n122\t1\t-0.426613476\n123\t0\t0.41919973\n124\t1\t1.323292819\n125\t0\t-0.544174426\n126\t1\t0.621298873\n127\t1\t1.238476479\n128\t1\t0.16799208\n129\t0\t-0.212440076\n130\t0\t-0.731865253\n131\t1\t1.36497788\n132\t1\t1.857916863\n133\t1\t1.914733827\n134\t0\t0.804471304\n135\t1\t0.467429875\n136\t1\t1.860606663\n137\t1\t1.530392622\n138\t1\t1.478076476\n139\t1\t0.930883359\n140\t1\t0.011360264\n141\t0\t0.943800548\n142\t1\t0.185940216\n143\t1\t0.807463043\n144\t0\t0.523547654\n145\t0\t-0.491533264\n146\t0\t0.13295351\n147\t1\t1.054049332\n148\t1\t1.231916154\n149\t0\t0.303035444\n150\t1\t1.529160242\n151\t0\t1.72823875\n152\t1\t1.664167663\n153\t1\t0.959529888\n154\t1\t1.102157002\n155\t0\t0.238221551\n156\t1\t1.530214457\n157\t1\t0.104464177\n158\t0\t0.463692693\n159\t0\t-0.017808085\n160\t0\t-0.953760763\n161\t0\t0.229738978\n162\t0\t1.192374875\n163\t1\t1.704677678\n164\t1\t0.207821861\n165\t0\t-0.104414739\n166\t0\t-0.927001856\n167\t1\t1.047874858\n168\t1\t1.48133932\n169\t1\t0.651602647\n170\t0\t1.003838461\n171\t0\t-0.837728691\n172\t0\t0.736081068\n173\t1\t0.489887201\n174\t0\t-1.657313023\n175\t0\t0.117912386\n176\t0\t0.286664809\n177\t0\t-0.647571655\n178\t0\t-1.780138663\n179\t0\t-0.52717507\n180\t0\t-0.382268668\n181\t0\t-0.166953194\n182\t0\t0.367424661\n183\t1\t0.390937703\n184\t1\t0.44515669\n185\t1\t1.769153448\n186\t1\t1.935194515\n187\t0\t1.596114804\n188\t1\t2.079030752\n189\t0\t1.2652254\n190\t0\t0.198347427\n191\t0\t1.085731602\n192\t0\t-0.093470823\n193\t1\t0.448739099\n194\t1\t1.051706976\n195\t1\t1.075358609\n196\t1\t0.918083753\n197\t1\t-0.178941079\n198\t1\t0.917509408\n199\t0\t1.313157734\n200\t0\t0.300322855\n',
                              mode="r", height = "300px", fontSize = 14)),

                box(title = "Enter Data (tab seperated, you can copy paste from excel)", collapsible = T, collapsed = F, width = 6, height = "300px",
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
























