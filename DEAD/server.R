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

# TODO
# Make all outputs from diganostic test against gold standard - need to add pvalues
# Cleanup outputs
# ROC curve more options
# Option to round off values


# if get bioconductor error do this
# options(repos = BiocInstaller::biocinstallRepos())
# getOption("repos")

# library(mailR)
# require(sendmailR)
# require(shinyalert)
#Stuff here runs once per app

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
                print(mcnemar.test(RCinputData()), correct = FALSE)
            else
                print(mcnemar.test(RCinputData()), correct = TRUE)
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
        #
        # dat.tbl.eprF <- tab.1test(d=Disease, y=Test, data=dat)
        # acc.1test(dat.tbl.eprF, 1-confInt)
        colnames(ep.res) <- colnames(res.car.edit)
        row.names(ep.res) <- NULL
        DT::datatable(rbind(ep.res, res.car.edit), options = list(pageLength = 21))

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


        res.tmp.A <- acc.paired(ptab, alpha = 1-confInt)


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

        rownames(res.tmp.A) <- NULL
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

        output$ptab1 <- renderPrint(print(res.tmp.A))
        output$ptab3 <- renderDataTable(
            datatable(comb[-c(6)], selection = 'none')
        )
        output$ptab4 <- renderDataTable(
            datatable(res2[-c(6, 8)], selection = 'none')
        )

    })











    #########
    ## ROC ##
    #########
    ROCinputData <- reactive({
        dat <- read.csv(text=input$roc_input, sep="", na.strings=c("","NA","."))
        rownames(dat) <- dat[,1]
        dat <- dat[,-c(1)]
        colnames(dat) <- c("Outcome", "Measurement")

        dat[c(1:2)]
    })

    output$ROCdt <- renderPrint({
        dat <- ROCinputData()
        if (length(unique(dat$Outcome)) != 2)
            print("Outcome not Binary!")
        else{
            if (any(!unique(dat$Outcome)%in%c(0,1))){
                slev <- sort(levels(as.factor(dat$Outcome)))
                showNotification(paste0("Outcome not labeled 0/1, assuming ", slev[1],
                                        " = 0 and ", slev[2], " = 1!"), duration = NULL)
            }
            dat[c(1,2)]
        }
    })

    output$ROCplot <- renderPlot({
        dat <- ROCinputData()
        txt <- roc(Outcome ~ Measurement, dat, smooth=TRUE)

        last.message <- NULL
        p <- tryCatch(
            ggplot(dat, aes(d = Outcome, m = Measurement)) + geom_roc() + style_roc() +
                annotate("text", label = paste0("AUC: ", round(txt$auc,2)), y=0.25, x=0.75)+
                theme(panel.background = element_rect(fill="#fef7ea", colour="#fef7ea"),
                      plot.background = element_rect(fill = "#fef7ea"))
        )
        if (input$plot_xkcd)
            p + theme_xkcd()
        else
            p

    })

})



