require(BiocManager)
require(shiny)
require(shinyjs)
require(DT)
require(tidyverse)
require(plotly)
require(Hmisc)
require(pwr)
require(samplingbook)
require(xkcd)
# devtools::install_github("slzhao/RnaSeqSampleSize")
require(RnaSeqSampleSize)
require(e1071)
require(caret)
require(pROC)
require(plotROC)
require(ggplot2)
require(shinyAce)
require(epiR)
require(DTComPair)
require(epitools)
require(tidyverse)
require(plotly)
require(reshape2)
require(pROC)

# TODO
# Make all outputs from diganostic test against gold standard - need to add pvalues
# Cleanup outputs
# ROC curve more options
# Option to round off values

# This code is filthy and disgusting, you shoul be ashamed. CLEAN IT UP!


# if get bioconductor error do this
# options(repos = BiocInstaller::biocinstallRepos())
# getOption("repos")

# library(mailR)
# require(sendmailR)
# require(shinyalert)
#Stuff here runs once per app


acc.paired.me <- function (tab, alpha, ...)
{
    if (missing(tab))
        stop("Table is missing.")
    if (class(tab) != "tab.paired")
        stop("Table must be of class 'tab.paired'")
    if (missing(alpha))
        alpha <- 0.05
    test1 <- read.tab.1test(tab$diseased[3, 1], tab$non.diseased[3,
                                                                 1], tab$diseased[3, 2], tab$non.diseased[3, 2], testname = tab$testnames[1])
    test2 <- read.tab.1test(tab$diseased[1, 3], tab$non.diseased[1,
                                                                 3], tab$diseased[2, 3], tab$non.diseased[2, 3], testname = tab$testnames[2])
    acc.test1 <- acc.1test(test1, alpha)
    acc.test2 <- acc.1test(test2, alpha)
    results <- list(acc.test1, acc.test2)
    names(results) <- c("Test1", "Test2")
    class(results) <- "acc.paired"
    return(results)
}


#  I put everything in a loop as not all the things can take in vetors. So this was jsut easier.
#
#
#
# Define server logic required to draw a histogram
# test <<- 0
shinyServer(function(input, output, session){
    ######################
    ## Chi Sq or fisher ##
    ######################
    RCinputData <- reactive({
        dat <- read.csv(text=input$rc_input, sep="", na.strings=c("","NA","."))
        rownames(dat) <- dat[,1]
        dat <- dat[,-c(1)]

        dat <- as.matrix(dat)

        print(addmargins(dat))
    })

    output$RCdt <- renderPrint({
        # if (sumRCinputData() < 20)
        if (any(RCinputData()<5)){
            print(binom.test(c(RCinputData()[1,1], sum(RCinputData())), p = 0.5))
        }else{
            if (input$CC)
                print(mcnemar.test(RCinputData()), correct = F)
            else
                print(mcnemar.test(RCinputData()), correct = T)
        }
    })

    # require(broom)
    # tidy(chisq.test(dat))


    #######################
    ## Diagnostic Test 1 ##
    #######################
    DTPinputData <- reactive({
        if (input$longIn){
            dat <- read.csv(text=input$DT1.long, sep="", na.strings=c("","NA","."))
            dat[dat!="0" & dat!="1"] <- NA
            colnames(dat) <- c("GoldStandard", "Test")
            dat$GoldStandard <- factor(dat$GoldStandard, levels=c("1","0"))
            dat$Test <- factor(dat$Test, levels=c("1","0"))
            as.matrix(table(dat$Test, dat$GoldStandard))
        }else{
            dat <- read.csv(text=input$dtp_input, sep="", na.strings=c("","NA","."))
            rownames(dat) <- dat[,1]
            dat <- dat[,-c(1)]
            dat <- as.matrix(dat)
            dat
        }
    })

    output$DTPdt.res1 <- renderDataTable({
        confInt <- input$CI1
        dat.tbl <- DTPinputData()
        colnames(dat.tbl) <- c("Dis+","Dis-"); rownames(dat.tbl) <- c("Test+","Test-")

        # Print the table out
        output$DTPdt1 <- renderPrint(print(addmargins(dat.tbl)))

        ep.res <- summary(epi.tests(dat.tbl, conf.level = confInt))
        ep.res$Test <- c("Apparent Prevalence","True Prevalence","Sensitivity","Specificity","Diagnostic Accuracy","Diagnostic Odds Ratio","Number Needed to Diagnose",
                         "Youden's Index","Positive Predictive Value","Negative Predictive Value","Likelihood Ratio of a Positive Test","Likelihood Ratio of a Negative Test")
        ep.res <- cbind(ep.res[c(4,1:3)], NA)
        # colnames(ep.res) <- colnames(fish.res)

        # dat.df <- expand.table(dat.tbl)
        dat.caret <- DTPinputData()
        res.car <- confusionMatrix(as.table(dat.caret))
        res.car.edit <- data.frame(
            Test = c(names(c(res.car$byClass[c(5:7, 8,9, 10)], res.car$overall[c(2)])), "No Information Rate"),
            Estimate = c(res.car$byClass[c(5:7, 8,9, 10)], res.car$overall[c(2,5)]),
            LowerCI = NA,
            UpperCI = NA,
            p.value = NA,
            row.names = NULL
        )
        res.car.edit[8, "p.value"] <- res.car$overall["AccuracyPValue"]

        if (input$roundDT1){
            # ep.res[ep.res$Test == "Number Needed to Diagnose", c(2:4)] <- ceiling(ep.res[ep.res$Test == "Number Needed to Diagnose", c(2:4)])

            ep.res$est <- round(ep.res$est, 2)
            ep.res$lower <- round(ep.res$lower, 2)
            ep.res$upper <- round(ep.res$upper, 2)
            res.car.edit$Estimate <- round(res.car.edit$Estimate, 2)
        }

        NIR <- res.car.edit[8, ]
        res.car.edit <- res.car.edit[-8,]


        output$NIR <- renderPrint(
            paste0(
                "No Information Rate [NIR] = ", NIR["Estimate"],
                ", p.value=", NIR["p.value"])
        )

        colnames(ep.res) <- colnames(res.car.edit)
        row.names(ep.res) <- NULL
        res.car.prnt <- rbind(ep.res, res.car.edit)[c(1:4)]

        output$downloadDT1 <- downloadHandler(
            filename = function() {
                paste("DEAD_SingleTest", ".csv", sep = "")
            },
            content = function(file) {
                write.csv(res.car.prnt, file, row.names = FALSE)
            }
        )

        #
        # dat.tbl.eprF <- tab.1test(d=Disease, y=Test, data=dat)
        # acc.1test(dat.tbl.eprF, 1-confInt)

        DT::datatable(res.car.prnt, options = list(pageLength = 20))




    })




    #######################
    ## Diagnostic Test 2 ##
    #######################
    DTPaired_inputData <- reactive({
        dat <- read.csv(text=input$DTbin_input, sep="", na.strings=c("","NA","."))
        # rownames(dat) <- dat[,1]
        # dat <- dat[,-c(1)]
        dat[dat!="0" & dat!="1"] <- NA
        colnames(dat) <- c("GoldStandard", "Test1", "Test2")
        # dat$GoldStandard <- factor(dat$GoldStandard, levels=c("1","0"))
        # dat$Test1 <- factor(dat$Test1, levels=c("1","0"))
        # dat$Test2 <- factor(dat$Test2, levels=c("1","0"))
        dat[c(1:3)]
    })


    observe({
        confInt <- input$CI2
        dat <- NULL
        dat <- DTPaired_inputData()

        ptab <- dlr.results <- res <- spec.tmp <- NULL
        ptab <- tab.paired(d=GoldStandard, y1=Test1, y2=Test2, data=dat)
        output$ptabA <- renderPrint(print(ptab))

        dlr.results <- dlr.regtest(ptab, alpha = 1-confInt)
        # null hypothesis rDLR = DLR of Test 1 / DLR of Test 2 = 1
        res <- rbind(LRpos=data.frame(dlr.results$pdlr), LRneg=data.frame(dlr.results$ndlr))
        colnames(res) <- c("Test1", "Test2", "Ratio", "SE.log", "TestStatistic", "p.value", "Lower Conficence Interval", "Upper Conficence Interval")
        # res$test <- c("LRpos", "LRneg")
        spec.tmp <- sesp.mcnemar(ptab)
        spec.tmp <- rbind(sensitivity=spec.tmp$sensitivity, specificity=spec.tmp$specificity)
        spec.tmp2 <- sesp.exactbinom(ptab)
        spec.tmp2 <- rbind(sensitivity=spec.tmp2$sensitivity, specificity=spec.tmp2$specificity)

        pv1 <- pv.gs(ptab)
        pv1 <- rbind(PPV=data.frame(pv1$ppv), NPV=data.frame(pv1$npv))
        pv1$Method <- "Generalized Score Statistic (gs)"
        pv2 <- pv.rpv(ptab, alpha = 1-confInt)
        pv2.a <- data.frame(pv2$ppv)
        pv2.b <- data.frame(pv2$npv)
        colnames(pv2.a) <- c("Test1", "Test2", "Ratio", "SE.log", "Lower Conficence Interval", "Upper Conficence Interval", "Test Statistic", "p.value")
        colnames(pv2.b) <- colnames(pv2.a)
        pv2 <- rbind(PPV=pv2.a, NPV=pv2.b)
        pv2$Method <- "Relative Predictive Values (rpv)"

        pv3 <- pv.wgs(ptab)
        pv3 <- rbind(PPV=data.frame(pv3$ppv), NPV=data.frame(pv3$npv))
        pv3$Method <- "Weighted Generalized Score Statistic (wgs)"


        # res.tmp.A <- acc.paired(ptab, alpha = 1-confInt)
        res.tmp.A <- acc.paired.me(ptab, alpha = 1-confInt)



        res.plt <- rbind(t(data.frame(res.tmp.A$Test1$sensitivity,
                                      res.tmp.A$Test1$specificity,
                                      res.tmp.A$Test1$ppv,
                                      res.tmp.A$Test1$npv,
                                      res.tmp.A$Test1$pdlr,
                                      res.tmp.A$Test1$ndlr)),
                         t(data.frame(
                             res.tmp.A$Test2$sensitivity,
                             res.tmp.A$Test2$specificity,
                             res.tmp.A$Test2$ppv,
                             res.tmp.A$Test2$npv,
                             res.tmp.A$Test2$pdlr,
                             res.tmp.A$Test2$ndlr)))
        res.plt <- data.frame(res.plt)
        res.plt$Test <- c(rep("Test 1", 6), rep("Test 2", 6))
        res.plt$Metric <- rep(c("Sensitivity", "Specificity", "PPV", "NPV", "PDLR", "NDLR"), 2)
        res.plt$Estimate <- res.plt$est


        Offset <- data.frame(Test = unique(res.plt$Test),
                             offset = seq(-0.1, 0.1,length.out = length(unique(res.plt$Test))))

        res.plt <- merge(res.plt, Offset, by = "Test", all.x = TRUE)

        ## Calculate an x location
        res.plt <- res.plt[order(res.plt$Metric, decreasing = T),]
        res.plt$y_location <- as.numeric(as.factor(res.plt$Metric)) + res.plt$offset
        # xend1 <- max(res.plt$est*100+res.plt$se*100)+max(res.plt$est*100+res.plt$se*100)*0.05
        hline <- function(y = 0, color = "#e7e7e7") {
            list(
                type = "line",
                x0 = 0,
                x1 = 1,
                xref = "paper",
                opacity = 0.3,
                y0 = y,
                y1 = y,
                line = list(color = color)
            )
        }



        output$stat.plt1 <- renderPlotly({
            res.plt%>%
                plot_ly( y = ~y_location, x = ~Estimate*100, type = 'scatter', mode = 'markers', color = ~Test,
                         text = ~paste0(Test,": ", Metric, "\nEstimate: ", round(Estimate ,2), " (", round(lcl ,2), ",", round(ucl ,2), ")" ),
                         hoverinfo = "text",
                         error_x = ~list(array = se*100,
                                         color = 'white')
                )%>%
                layout(
                    font=list(
                        family = "sans serif",
                        size = 14,
                        color = 'White'),
                    paper_bgcolor='#272b30',
                    plot_bgcolor='#272b30',
                    # autosize = F, width = 800, height =850,
                    yaxis=list(title=NA,
                               zeroline=FALSE,
                               tickmode = "array",
                               tickvals = unique(as.numeric(as.factor(res.plt$Metric))),
                               ticktext = unique(res.plt$Metric),
                               showgrid = TRUE,
                               showline = FALSE,
                               gridcolor = "#272b30"
                    ),
                    shapes = list(hline(0.8),
                                  hline(1.2),
                                  hline(1.8),
                                  hline(2.2),
                                  hline(2.8),
                                  hline(3.2),
                                  hline(3.8),
                                  hline(4.2),
                                  hline(4.8),
                                  hline(5.2),
                                  hline(5.8),
                                  hline(6.2)
                    )
                )

        })



        spec.tmp <- data.frame(spec.tmp[,c(1,2,3,5,4)])
        spec.tmp$Method <- "McNemar"
        spec.tmp2 <- data.frame(spec.tmp2)
        spec.tmp2$test.statistic <- NA
        spec.tmp2$Method <- "Exact Binomial (if small number [<20] of values use this)"

        comb <- rbind(spec.tmp, spec.tmp2)
        comb$Test <- rownames(comb)

        mc.tmp <- rbind(pv1, pv3)
        mc.tmp$Test <- rownames(mc.tmp)
        mc.tmp <- mc.tmp[c(1,2,3,5,4,6,7)]
        comb <- rbind(comb, mc.tmp)

        pv2 <- data.frame(pv2)
        pv2$Test <- rownames(pv2)

        res2 <- data.frame(res)
        res2 <- res2[c(1,2,3,4,7,8,5,6)]
        res2$Method <- NA
        res2$Test <- rownames(res2)
        colnames(res2) <- colnames(pv2)

        res2 <- rbind(pv2, res2)

        # rownames(res.tmp.A) <- NULL
        rownames(comb) <- NULL
        rownames(res2) <- NULL
        comb <- comb[c(7,1:6)]
        colnames(comb) <- c("Test", "Test1", "Test2", "Difference", "p.value", "Test Statistic", "Method")
        res2 <- res2[c(10,1:3,8,7, 9,4,5,6)]
        colnames(res2) <- c("Test", "Test1", "Test2", "Ratio", "p.value", "Test.Statistic", "Method", "SE.log", "Lower Conficence Interval", "Upper Conficence Interval")

        # test <<- comb
        # test2<<-res2
        if (input$roundDT2){
            for (col in c(2:4,6)){
                comb[c(col)] <- round(unlist(comb[c(col)]), 2) #dont know why its making me do this, think dataTable package doing something
            }

            for (col in c(2,3,4,6,8,9,10)){
                res2[c(col)] <- round(unlist(res2[c(col)]), 2)
            }
        }

        # datatable(, options = list(pageLength = 21))
        # test1 <<- comb[-c(6)]
        # test2 <<- res2

        comb$Test <- c("Sensitivity", "Specificty", "Sensitivity", "Specificty",
                       "Positive Predictive Value", "Negative Predictive Value","Positive Predictive Value", "Negative Predictive Value")
        res2$Test <- c("Positive Predictive Value", "Negative Predictive Value",
                       "Positive Likelihood Ratio", "Negative Likelihood Ratio")

        comb <- comb[-c(6)]
        comb <- comb[c(1,3,2,4,5,7,6,8), c(1,6,2,3,4,5)]
        res2 <- res2[-c(6, 8)]
        res2 <- res2[c(1,6,2,3,4,5,7,8)]

        #     comb$LCI <- NA
        #     comb$UCI <- NA
        #     colnames(comb)[7:8] <- c("Lower Conficence Interval", "Upper Conficence Interval")
        # comb <<- rbind(comb, res2)

        # test1<<-comb
        output$ptab1 <- renderPrint(print(res.tmp.A))
        output$ptab3 <- renderDataTable(
            datatable(comb, selection = 'none')
        )
        output$ptab4 <- renderDataTable(
            datatable(res2, selection = 'none')
        )

        comb$p.value <- as.numeric(comb$p.value)
        output$downloadDT2A <- downloadHandler(
            filename = function() {
                paste("DEAD_DualTestsA", ".csv", sep = "")
            },
            content = function(file2) {
                write.csv(comb, file2, row.names = FALSE)
            }
        )

        output$downloadDT2B <- downloadHandler(
            filename = function() {
                paste("DEAD_DualTestsB", ".csv", sep = "")
            },
            content = function(file) {
                write.csv(res2, file, row.names = FALSE)
            }
        )


    })











    #########
    ## ROC ##
    #########
    ROCinputData <- reactive({
        dat <- read.csv(text=input$roc_input, sep="", na.strings=c("","NA","."))

        colnames(dat)[1] <- "Outcome"
        if (length(unique(dat$Outcome)) != 2){
            print("Outcome not Binary!")
        }else{
            if (any(!unique(dat$Outcome)%in%c(0,1))){
                slev <- sort(levels(as.factor(dat$Outcome)))
                showNotification(paste0("Outcome not labeled 0/1, assuming ", slev[1],
                                        " = 0 and ", slev[2], " = 1!"), duration = NULL)
            }
        }

        if (ncol(dat) <=2){
            colnames(dat) <- c("Outcome", "Test1")
            dat[c(1:2)]
        }else{
            colnames(dat) <- c("Outcome", "Test1", "Test2")
            dat[c(1:3)]
        }
    })

    output$ROCdt <- renderPrint({
        dat <- ROCinputData()

        if (length(unique(dat$Outcome)) != 2){
            print("Outcome not Binary!")
        }else{
            if (any(!unique(dat$Outcome)%in%c(0,1))){
                slev <- sort(levels(as.factor(dat$Outcome)))
                showNotification(paste0("Outcome not labeled 0/1, assuming ", slev[1],
                                        " = 0 and ", slev[2], " = 1!"), duration = NULL)
            }
        }
        dat
    })

    # output$ROCplot <- renderUI({
    #     dat <- ROCinputData()
    #
    #     if (ncol(dat) <=2){
    #         txt <- roc(Outcome ~ Test1, dat, smooth=TRUE)
    #
    #         last.message <- NULL
    #         p <- tryCatch(
    #             ggplot(dat, aes(d = Outcome, m = Test1)) + geom_roc() + style_roc() +
    #                 annotate("text", label = paste0("AUC: ", round(txt$auc,2)), y=0.25, x=0.75)+
    #                 theme(panel.background = element_rect(fill="#fef7ea", colour="#fef7ea"),
    #                       plot.background = element_rect(fill = "#fef7ea"))
    #         )
    #     }else{
    #         # txt1 <- roc(Outcome ~ Test1, dat, smooth=TRUE)
    #         # txt2 <- roc(Outcome ~ Test2, dat, smooth=TRUE)
    #         withProgress(message = 'Calculating CIs', value = 0, {
    #             incProgress(1/2, detail = paste("Bootstrapping"))
                # txt1.CI <- roc(dat$Outcome, dat$Test1, ci = TRUE)
                # txt1 <- paste0(round(txt1.CI$ci[2], 2), " (95% CI=", round(txt1.CI$ci[1], 2), ", ", round(txt1.CI$ci[3], 2), ")")
                # txt2.CI <- roc(dat$Outcome, dat$Test2, ci = TRUE)
                # txt2 <- paste0(round(txt2.CI$ci[2], 2), " (95% CI=", round(txt2.CI$ci[1], 2), ", ", round(txt2.CI$ci[3], 2), ")")
                # roc.p <- roc.test(txt1.CI, txt2.CI)
    #
    #             dat.long <- melt(dat, id.vars="Outcome")
    #             colnames(dat.long) <- c("Outcome", "Test", "Measurement")
    #             p <- ggplot(dat.long, aes(d = Outcome, m = Measurement, color = Test)) + geom_roc(show.legend = F)
    #             p <- direct_label(p) + style_roc() +
    #                 annotate("text", label = paste0("AUC\nTest 1: ", txt1, "\nTest 2: ", txt2),
    #                          y=0.25, x=0.75)+
    #                 annotate("text", label = paste0("p.value (bootstrap test):\n", roc.p$p.value),
    #                          y=0.15, x=0.75)+
    #                 theme(panel.background = element_rect(fill="#fef7ea", colour="#fef7ea"),
    #                       plot.background = element_rect(fill = "#fef7ea"))
    #
    #             incProgress(2/2, detail = paste("Plotting"))
    #         })
    #     }
    #     if (input$plot_xkcd)
    #         HTML(export_interactive_roc(p + theme_xkcd())) # p + theme_xkcd() #
    #     else
    #         HTML(export_interactive_roc(p,
    #                                     width = 18, height = 10)) #p
    # })
    #
    #

    output$ROCplotly <- renderPlotly({
        dat <- ROCinputData()

        if (ncol(dat) <=2){
            txt.CI <- roc(Outcome ~ Test1, dat, smooth=input$smooth, ci = TRUE)
            p <- tryCatch(
                plot_ly(x = 1-txt.CI$specificities, y = txt.CI$sensitivities, name = 'Test', type = 'scatter', mode = 'lines') %>%
                    layout(#title = 'ROC Curve',
                        xaxis = list(title = 'False Positive Rate',
                                     zeroline = TRUE,
                                     range = c(0, 1),
                                     tickvals=c(seq(0, 0.09, 0.01), c(0.1, 0.25, 0.5, 0.75, 0.9, 1), seq(0.9, 1, 0.01)),
                                     ticktext=c(rep("", 10), c(0.1, 0.25, 0.50, 0.75, 0.90, 1.00), rep("", 11))
                        ),
                        yaxis = list(title = 'True Positive Rate',
                                     range = c(0,1),
                                     tickvals=c(seq(0, 0.09, 0.01), c(0.1, 0.25, 0.5, 0.75, 0.9, 1), seq(0.9, 1, 0.01)),
                                     ticktext=c(rep("", 10), c(0.1, 0.25, 0.50, 0.75, 0.90, 1.00), rep("", 11))
                        ))%>%layout(
                            plot_bgcolor="#fef7ea",
                            paper_bgcolor ="#fef7ea",
                            legend = list(
                                x = 0.6, y = 0.4,
                                font = list(
                                    family = "sans-serif",
                                    size = 12,
                                    color = "#000"),
                                bgcolor = "#E2E2E2",
                                bordercolor = "#FFFFFF",
                                borderwidth = 2)
                        )%>% layout(shapes = list(
                            type = "line",
                            x0 = 0,
                            x1 = 1,
                            y0 = 0,
                            y1 = 1,
                            line = list(color = "white")
                        ))
            )
        }else{
            withProgress(message = 'Calculating CIs', value = 0, {
                incProgress(1/2, detail = paste("Bootstrapping"))
                txt1.CI <- roc(dat$Outcome, dat$Test1, ci = TRUE, smooth = input$smooth)
                txt1 <- paste0(round(txt1.CI$ci[2], 2), " (95% CI=", round(txt1.CI$ci[1], 2), ", ", round(txt1.CI$ci[3], 2), ")")
                txt2.CI <- roc(dat$Outcome, dat$Test2, ci = TRUE, smooth = input$smooth)
                txt2 <- paste0(round(txt2.CI$ci[2], 2), " (95% CI=", round(txt2.CI$ci[1], 2), ", ", round(txt2.CI$ci[3], 2), ")")
                roc.p <- roc.test(txt1.CI, txt2.CI)

                p <- plot_ly(x = 1-txt1.CI$specificities, y = txt1.CI$sensitivities, name = 'Test 1', type = 'scatter', mode = 'lines') %>%
                    add_trace(x = 1-txt2.CI$specificities, y = txt2.CI$sensitivities, name = 'Test 2', mode = 'lines') %>%
                    layout(#title = 'ROC Curve',
                           xaxis = list(title = 'False Positive Rate',
                                        zeroline = TRUE,
                                        range = c(0, 1),
                                        tickvals=c(seq(0, 0.09, 0.01), c(0.1, 0.25, 0.5, 0.75, 0.9, 1), seq(0.9, 1, 0.01)),
                                        ticktext=c(rep("", 10), c(0.1, 0.25, 0.50, 0.75, 0.90, 1.00), rep("", 11))
                           ),
                           yaxis = list(title = 'True Positive Rate',
                                        range = c(0,1),
                                        tickvals=c(seq(0, 0.09, 0.01), c(0.1, 0.25, 0.5, 0.75, 0.9, 1), seq(0.9, 1, 0.01)),
                                        ticktext=c(rep("", 10), c(0.1, 0.25, 0.50, 0.75, 0.90, 1.00), rep("", 11))
                           ),
                           annotations = list(
                               y=0.25, x=0.75,
                               text = paste0("AUC\nTest 1: ", txt1, "\nTest 2: ", txt2),
                               xref = "x",
                               yref = "y",
                               showarrow = F
                           ))%>%
                    layout(annotations = list(
                        y=0.15, x=0.75,
                        text = paste0("p.value (bootstrap test):\n", roc.p$p.value),
                        xref = "x",
                        yref = "y",
                        showarrow = F
                    )
                    )%>%layout(
                        plot_bgcolor="#fef7ea",
                        paper_bgcolor ="#fef7ea",
                        legend = list(
                            x = 0.6, y = 0.4,
                            font = list(
                                family = "sans-serif",
                                size = 12,
                                color = "#000"),
                            bgcolor = "#E2E2E2",
                            bordercolor = "#FFFFFF",
                            borderwidth = 2)
                    )%>% layout(shapes = list(
                        type = "line",
                        x0 = 0,
                        x1 = 1,
                        y0 = 0,
                        y1 = 1,
                        line = list(color = "white")
                    ))



                incProgress(2/2, detail = paste("Plotting"))
            })
        }
        # if (input$plot_xkcd)
        #     p + theme_xkcd() #
        # else
            p
    })













    observeEvent(input$sidebarmenu, {
        if (input$sidebarmenu == "cite")
            showModal(modalDialog(
                title = "Citation",
                paste0("Limberis, JD. 2018.", "DEAD: ", "[Online]. Available at: ", "https://semiquant.shinyapps.io/DEAD/", "(Accessed: ", Sys.Date(), ")"),
                easyClose = TRUE,
                footer = NULL
            ))
    })

})



