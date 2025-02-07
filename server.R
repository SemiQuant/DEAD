.libPaths(c("/home/semiquant/R/x86_64-pc-linux-gnu-library/4.4", "/usr/local/lib/R/site-library", "/usr/lib/R/site-library", "/usr/lib/R/library", .libPaths()))
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
        # Add debug logging
        cat("\nCreating contingency matrix...\n")
        
        # Validate inputs are numeric and not NULL
        req(input$rc_pp, input$rc_pn, input$rc_np, input$rc_nn)
        values <- c(input$rc_pp, input$rc_pn, 
                    input$rc_np, input$rc_nn)
        
        if (!all(sapply(values, is.numeric))) {
            stop("All inputs must be numeric values")
        }
        
        tryCatch({
            # Create 2x2 matrix with proper structure
            matrix_data <- matrix(
                values,
                nrow = 2,
                ncol = 2,
                byrow = TRUE,
                dimnames = list(
                    Test2 = c("Positive", "Negative"),
                    Test1 = c("Positive", "Negative")
                )
            )
            
            cat("Matrix created:\n")
            print(matrix_data)
            
            # Validate matrix dimensions
            if (!all(dim(matrix_data) == c(2,2))) {
                stop("Invalid matrix dimensions. Expected 2x2 matrix.")
            }
            
            # Convert to table class for compatibility
            matrix_data <- as.table(matrix_data)
            
            return(matrix_data)
            
        }, error = function(e) {
            # Log error and return NULL
            cat("Error in matrix creation:", e$message, "\n")
            NULL
        })
    })

    output$RCdt <- renderDataTable({
        # Validate input matrix
        matrix_data <- RCinputData()
        req(matrix_data)
        
        tryCatch({
            # Extract discordant pairs for McNemar's test
            n12 <- matrix_data[1,2]  # Test2+/Test1-
            n21 <- matrix_data[2,1]  # Test2-/Test1+
            
            results <- if (min(n12, n21) < 5) {
                # Use exact binomial test for small frequencies
                binom.test(x = n12, n = n12 + n21, p = 0.5)
            } else {
                # Use McNemar's test with continuity correction as specified
                mcnemar.test(matrix_data, correct = input$CC)
            }
            
            # Format results into a table with conditional fields
            metrics <- c("Test Type", "Discordant Pairs (n)")
            values <- c(
                results$method,
                paste(n12 + n21, "(Test2+/Test1-:", n12, ", Test2-/Test1+:", n21, ")")
            )
            
            # Add test-specific fields
            if (inherits(results, "htest")) {
                if (!is.null(results$statistic)) {
                    metrics <- c(metrics, "Test Statistic")
                    values <- c(values, sprintf("%.3f", unname(results$statistic)))
                }
                if (!is.null(results$p.value)) {
                    metrics <- c(metrics, "P-value")
                    values <- c(values, sprintf("%.4f", results$p.value))
                }
                if (!is.null(results$alternative)) {
                    metrics <- c(metrics, "Alternative Hypothesis")
                    values <- c(values, results$alternative)
                }
            }
            
            # Create data frame with matching rows
            results_df <- data.frame(
                Metric = metrics,
                Value = values,
                stringsAsFactors = FALSE
            )
            
            # Return formatted datatable
            DT::datatable(
                results_df,
                options = list(
                    dom = 't',
                    ordering = FALSE,
                    pageLength = 50
                ),
                rownames = FALSE
            ) %>%
                formatStyle(
                    columns = 1:2,
                    backgroundColor = "rgb(52,62,72)",
                    color = "white"
                )
        }, error = function(e) {
            # Return error message as table
            error_df <- data.frame(
                Message = "Error",
                Value = paste("Analysis error:", e$message)
            )
            
            DT::datatable(
                error_df,
                options = list(dom = 't'),
                rownames = FALSE
            ) %>%
                formatStyle(
                    columns = 1:2,
                    backgroundColor = "rgb(52,62,72)",
                    color = "white"
                )
        })
    })

    # require(broom)
    # tidy(chisq.test(dat))


    #######################
    ## Diagnostic Test 1 ##
    #######################
    DTPinputData <- reactive({
        # Create matrix from numeric inputs with proper dimnames
        result <- matrix(
            c(input$tp, input$fn, input$fp, input$tn), 
            nrow = 2,
            byrow = FALSE,
            dimnames = list(
                c("Positive", "Negative"),  # Test results
                c("Positive", "Negative")   # Disease status
            )
        )
        
        cat("\nInput matrix:\n")
        print(result)
        
        # Convert to table and ensure proper class
        result <- as.table(result)
        class(result) <- c("table", "matrix")
        
        cat("\nFinal table structure:\n")
        print(str(result))
        
        return(result)
    })

    output$DTPdt.res1 <- renderDataTable({
        withProgress(message = 'Calculating diagnostic metrics...', {
            confInt <- input$CI1
            dat.tbl <- DTPinputData()
            
            tryCatch({
                # Create results table with progress updates
                incProgress(0.3, detail = "Computing basic metrics")
                ep.res <- summary(epi.tests(dat.tbl, conf.level = confInt))
                
                incProgress(0.3, detail = "Computing additional statistics")
                res.car <- confusionMatrix(dat.tbl)
                
                # Format results
                incProgress(0.4, detail = "Formatting output")
                results <- format_diagnostic_results(ep.res, res.car, input$roundDT1)
                
                # Return formatted datatable with custom styling
                DT::datatable(
                    results,
                    options = list(
                        pageLength = 50,
                        dom = 't',
                        ordering = FALSE,
                        columnDefs = list(
                            list(className = 'dt-center', targets = 1:4)
                        )
                    ),
                    rownames = FALSE
                ) %>%
                    formatStyle(
                        'Test',
                        backgroundColor = "rgb(52,62,72)",
                        color = "white",
                        fontWeight = 'bold',
                        fontSize = '14px'
                    ) %>%
                    formatStyle(
                        c('Estimate', 'LowerCI', 'UpperCI', 'p.value'),
                        backgroundColor = "rgb(52,62,72)",
                        color = "white",
                        fontSize = '14px',
                        textAlign = 'center'
                    ) %>%
                    formatStyle(
                        columns = 1:5,
                        borderBottom = '1px solid rgba(255,255,255,0.1)'
                    ) %>%
                    formatRound(
                        columns = c('Estimate', 'LowerCI', 'UpperCI', 'p.value'),
                        digits = 2
                    ) %>%
                    formatStyle(
                        'p.value',
                        color = styleInterval(0.05, c('rgb(255,150,150)', 'white'))
                    )
            }, error = function(e) {
                # Return error message in table format
                DT::datatable(
                    data.frame(Error = paste("Error in calculation:", e$message)),
                    options = list(dom = 't')
                )
            })
        })
    })

    # Add output for confusion matrix
    output$DTPdt1 <- renderPrint({
        dat.tbl <- DTPinputData()
        margins <- addmargins(dat.tbl)
        
        cat("Confusion Matrix with Marginal Totals:\n\n")
        print(margins)
    })

    # Add output for NIR
    output$NIR <- renderPrint({
        dat.tbl <- DTPinputData()
        res.car <- confusionMatrix(dat.tbl)
        cat("No Information Rate p-value:", 
            format.pval(res.car$overall["AccuracyPValue"], digits = 3))
    })

    # Improve download handler with progress indicator
    output$downloadDT1 <- downloadHandler(
        filename = function() {
            paste0("diagnostic_test_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
        },
        content = function(file) {
            withProgress(message = 'Preparing download...', {
                # Get results
                confInt <- input$CI1
                dat.tbl <- DTPinputData()
                
                incProgress(0.5, detail = "Calculating metrics")
                results <- get_full_diagnostic_results(dat.tbl, confInt, input$roundDT1)
                
                incProgress(0.5, detail = "Writing file")
                write.csv(results, file, row.names = FALSE)
            })
        }
    )

    # Add debug output
    output$debug_output <- renderPrint({
        print("Current inputs:")
        print(paste("tp:", input$tp))
        print(paste("fn:", input$fn))
        print(paste("fp:", input$fp))
        print(paste("tn:", input$tn))
        print(paste("longIn:", input$longIn))
        print(paste("CI1:", input$CI1))
        print(paste("roundDT1:", input$roundDT1))
    })

    # For single test description
    output$single_diagnostic_description <- renderText({
        # Add debug logging
        cat("\nGenerating single diagnostic description...\n")
        
        # Get input data with validation
        dat.tbl <- DTPinputData()
        confInt <- input$CI1
        
        tryCatch({
            # Calculate metrics
            ep.res <- summary(epi.tests(dat.tbl, conf.level = confInt))
            res.car <- confusionMatrix(dat.tbl)
            
            # Format numbers - don't multiply by 100 since values are already percentages
            format_num <- function(x) format(round(x, 2), nsmall = 2)
            
            # Extract key metrics with validation and safe defaults
            sens <- ep.res$est[ep.res$statistic == "se"]
            sens_ci <- if(!is.null(ep.res$lower[ep.res$statistic == "se"])) {
                paste0(format_num(ep.res$lower[ep.res$statistic == "se"]*100), 
                       "-", format_num(ep.res$upper[ep.res$statistic == "se"]*100))
            } else {
                "not available"
            }
            
            spec <- ep.res$est[ep.res$statistic == "sp"]
            spec_ci <- if(!is.null(ep.res$lower[ep.res$statistic == "sp"])) {
                paste0(format_num(ep.res$lower[ep.res$statistic == "sp"]*100), 
                       "-", format_num(ep.res$upper[ep.res$statistic == "sp"]*100))
            } else {
                "not available"
            }
            
            # Create description
            description <- sprintf("
            <div style='background-color: rgb(52,62,72); padding: 15px; border-radius: 5px; margin-bottom: 20px;'>
                <h4 style='color: white; margin-top: 0;'>Summary of Diagnostic Test Performance</h4>
                <p style='color: white;'>
                The diagnostic test demonstrated a sensitivity of %s%% (95%% CI: %s%%) and specificity of %s%% (95%% CI: %s%%). 
                Overall diagnostic accuracy was %s%% (95%% CI: %s%%).
                </p>
            </div>",
            format_num(sens*100), sens_ci, 
            format_num(spec*100), spec_ci,
            format_num(res.car$overall["Accuracy"]*100), 
            paste0(format_num(res.car$overall["AccuracyLower"]*100), 
                   "-", format_num(res.car$overall["AccuracyUpper"]*100)))
            
            return(description)
            
        }, error = function(e) {
            # Log error
            cat("\nError in single_diagnostic_description:", e$message, "\n")
            cat("Error call:", deparse(e$call), "\n")
            
            return(sprintf("
            <div style='background-color: rgb(52,62,72); padding: 15px; border-radius: 5px; margin-bottom: 20px;'>
                <h4 style='color: white; margin-top: 0;'>Error Generating Description</h4>
                <p style='color: white;'>
                An error occurred while generating the diagnostic test description: %s
                </p>
            </div>", e$message))
        })
    })

    # Add after RCinputData() reactive function
    output$contingency_description <- renderText({
        # Get input data
        matrix_data <- RCinputData()
        
        # Extract discordant pairs
        n12 <- matrix_data[1,2]  # Test2+/Test1-
        n21 <- matrix_data[2,1]  # Test2-/Test1+
        total <- sum(matrix_data)
        
        # Calculate test results
        test_result <- if (min(n12, n21) < 5) {
            binom.test(x = n12, n = n12 + n21, p = 0.5)
        } else {
            mcnemar.test(matrix_data, correct = input$CC)
        }
        
        # Format p-value
        p_value <- format.pval(test_result$p.value, digits = 3)
        
        # Create description
        sprintf("
        <div style='background-color: rgb(52,62,72); padding: 15px; border-radius: 5px; margin-bottom: 20px;'>
            <h4 style='color: white; margin-top: 0;'>Summary of Statistical Analysis</h4>
            <p style='color: white;'>
            Analysis of %d paired measurements revealed %d discordant pairs (%d Test2+/Test1- and %d Test2-/Test1+). 
            Using %s, the difference between tests was %s statistically significant (p = %s).
            </p>
        </div>",
        total,
        n12 + n21,
        n12,
        n21,
        test_result$method,
        ifelse(test_result$p.value < 0.05, "", "not"),
        p_value)
    })

    #######################
    ## Diagnostic Test 2 ##
    #######################
    DTPaired_inputData <- reactive({
        cat("\nProcessing paired input data...\n")
        dat <- read.csv(text=input$DTbin_input, sep="", na.strings=c("","NA","."))
        cat("Raw data dimensions:", dim(dat), "\n")
        
        # Ensure proper column names and data types
        colnames(dat) <- c("GoldStandard", "Test1", "Test2")
        # Convert to numeric and ensure only 0/1 values
        dat$GoldStandard <- as.numeric(as.character(dat$GoldStandard))
        dat$Test1 <- as.numeric(as.character(dat$Test1))
        dat$Test2 <- as.numeric(as.character(dat$Test2))
        
        # Validate that values are only 0 or 1
        valid_rows <- dat$GoldStandard %in% c(0,1) & 
                      dat$Test1 %in% c(0,1) & 
                      dat$Test2 %in% c(0,1)
        
        if(!all(valid_rows)) {
            cat("Warning: Some rows contained invalid values (not 0 or 1)\n")
            dat <- dat[valid_rows, ]
        }
        
        # Validate data
        cat("Missing values in GoldStandard:", sum(is.na(dat$GoldStandard)), "\n")
        cat("Missing values in Test1:", sum(is.na(dat$Test1)), "\n")
        cat("Missing values in Test2:", sum(is.na(dat$Test2)), "\n")
        
        # Remove rows with any NA values
        dat <- dat[complete.cases(dat), ]
        cat("Final data dimensions after cleaning:", dim(dat), "\n")
        
        return(dat[c(1:3)])
    })


    observe({
        # Add debug logging
        cat("\nStarting analysis with inputs:\n")
        cat("CI2:", input$CI2, "\n")
        confInt <- input$CI2
        dat <- DTPaired_inputData()
        cat("Input data dimensions:", dim(dat), "\n")
        cat("Input data head:\n")
        print(head(dat))

        # Wrap everything in tryCatch to handle errors gracefully
        tryCatch({
            # Create paired table
            cat("\nAttempting to create paired table...\n")
            ptab <- tab.paired(d=GoldStandard, y1=Test1, y2=Test2, data=dat)
            cat("Paired table created successfully:\n")
            print(ptab)

            output$ptabA <- renderPrint({
                print(ptab)
            })

            # Get likelihood ratio results with validation
            cat("\nCalculating likelihood ratios...\n")
            dlr.results <- dlr.regtest(ptab, alpha = 1-confInt)
            cat("DLR results obtained:", !is.null(dlr.results), "\n")
            
            # Validate DLR results structure
            if (is.null(dlr.results) || !all(c("pdlr", "ndlr") %in% names(dlr.results))) {
                cat("Warning: Invalid DLR results structure\n")
                res <- data.frame(
                    Message = "Unable to calculate likelihood ratios - please check input data",
                    stringsAsFactors = FALSE
                )
            } else {
                # Create data frame directly from DLR results
                res <- data.frame(
                    Test = c("LRpos", "LRneg"),
                    Test1 = c(dlr.results$pdlr$test1, dlr.results$ndlr$test1),
                    Test2 = c(dlr.results$pdlr$test2, dlr.results$ndlr$test2),
                    Ratio = c(dlr.results$pdlr$ratio, dlr.results$ndlr$ratio),
                    SE.log = c(dlr.results$pdlr$se.log, dlr.results$ndlr$se.log),
                    TestStatistic = c(dlr.results$pdlr$test.statistic, dlr.results$ndlr$test.statistic),
                    p.value = c(dlr.results$pdlr$p.value, dlr.results$ndlr$p.value),
                    LowerCI = c(dlr.results$pdlr$lcl, dlr.results$ndlr$lcl),
                    UpperCI = c(dlr.results$pdlr$ucl, dlr.results$ndlr$ucl),
                    stringsAsFactors = FALSE
                )
            }
            
            # Add debug output for DLR results structure
            cat("\nDLR Results Structure:\n")
            str(dlr.results)
            
            # Get sensitivity/specificity results with validation
            cat("\nCalculating sensitivity/specificity...\n")
            spec.tmp <- sesp.mcnemar(ptab)
            cat("Spec results obtained:", !is.null(spec.tmp), "\n")

            # Print debug info
            cat("\nSensitivity/Specificity Results Structure:\n")
            str(spec.tmp)

            if (is.null(spec.tmp) || !all(c("sensitivity", "specificity") %in% names(spec.tmp))) {
                cat("Warning: Invalid sensitivity/specificity results structure\n")
                spec.tmp <- data.frame(
                    Test = character(),
                    Test1 = numeric(),
                    Test2 = numeric(),
                    Difference = numeric(),
                    p.value = numeric(),
                    TestStatistic = numeric(),
                    Method = character(),
                    stringsAsFactors = FALSE
                )
            } else {
                # Extract values with safe accessors - note the changed structure
                sens_test1 <- if (!is.null(spec.tmp$sensitivity["test1"])) spec.tmp$sensitivity["test1"] else NA
                sens_test2 <- if (!is.null(spec.tmp$sensitivity["test2"])) spec.tmp$sensitivity["test2"] else NA
                sens_diff <- if (!is.null(spec.tmp$sensitivity["diff"])) spec.tmp$sensitivity["diff"] else NA
                sens_pval <- if (!is.null(spec.tmp$sensitivity["p.value"])) spec.tmp$sensitivity["p.value"] else NA
                sens_stat <- if (!is.null(spec.tmp$sensitivity["test.statistic"])) spec.tmp$sensitivity["test.statistic"] else NA
                
                spec_test1 <- if (!is.null(spec.tmp$specificity["test1"])) spec.tmp$specificity["test1"] else NA
                spec_test2 <- if (!is.null(spec.tmp$specificity["test2"])) spec.tmp$specificity["test2"] else NA
                spec_diff <- if (!is.null(spec.tmp$specificity["diff"])) spec.tmp$specificity["diff"] else NA
                spec_pval <- if (!is.null(spec.tmp$specificity["p.value"])) spec.tmp$specificity["p.value"] else NA
                spec_stat <- if (!is.null(spec.tmp$specificity["test.statistic"])) spec.tmp$specificity["test.statistic"] else NA
                
                spec.tmp <- data.frame(
                    Test = c("sensitivity", "specificity"),
                    Test1 = c(sens_test1, spec_test1),
                    Test2 = c(sens_test2, spec_test2),
                    Difference = c(sens_diff, spec_diff),
                    p.value = c(sens_pval, spec_pval),
                    TestStatistic = c(sens_stat, spec_stat),
                    Method = "McNemar",
                    stringsAsFactors = FALSE
                )
            }

            # Get exact binomial results with validation
            spec.tmp2 <- sesp.exactbinom(ptab)
            cat("\nExact Binomial Results Structure:\n")
            str(spec.tmp2)

            if (is.null(spec.tmp2) || !all(c("sensitivity", "specificity") %in% names(spec.tmp2))) {
                cat("Warning: Invalid exact binomial results structure\n")
                spec.tmp2 <- data.frame(
                    Test = character(),
                    Test1 = numeric(),
                    Test2 = numeric(),
                    Difference = numeric(),
                    p.value = numeric(),
                    TestStatistic = numeric(),
                    Method = character(),
                    stringsAsFactors = FALSE
                )
            } else {
                # Extract values with safe accessors - note the changed structure
                sens_test1 <- if (!is.null(spec.tmp2$sensitivity["test1"])) spec.tmp2$sensitivity["test1"] else NA
                sens_test2 <- if (!is.null(spec.tmp2$sensitivity["test2"])) spec.tmp2$sensitivity["test2"] else NA
                sens_diff <- if (!is.null(spec.tmp2$sensitivity["diff"])) spec.tmp2$sensitivity["diff"] else NA
                
                spec_test1 <- if (!is.null(spec.tmp2$specificity["test1"])) spec.tmp2$specificity["test1"] else NA
                spec_test2 <- if (!is.null(spec.tmp2$specificity["test2"])) spec.tmp2$specificity["test2"] else NA
                spec_diff <- if (!is.null(spec.tmp2$specificity["diff"])) spec.tmp2$specificity["diff"] else NA
                
                spec.tmp2 <- data.frame(
                    Test = c("sensitivity", "specificity"),
                    Test1 = c(sens_test1, spec_test1),
                    Test2 = c(sens_test2, spec_test2),
                    Difference = c(sens_diff, spec_diff),
                    p.value = NA,
                    TestStatistic = NA,
                    Method = "Exact Binomial",
                    stringsAsFactors = FALSE
                )
            }

            # Get predictive values with validation
            pv1 <- pv.gs(ptab)
            cat("\nPredictive Values Structure:\n")
            str(pv1)

            if (is.null(pv1) || !all(c("ppv", "npv") %in% names(pv1))) {
                cat("Warning: Invalid predictive values structure\n")
                pv1 <- data.frame(
                    Test = character(),
                    Test1 = numeric(),
                    Test2 = numeric(),
                    Difference = numeric(),
                    p.value = numeric(),
                    TestStatistic = numeric(),
                    Method = character(),
                    stringsAsFactors = FALSE
                )
            } else {
                # Extract values with safe accessors - using named vector access
                ppv_test1 <- if (!is.null(pv1$ppv["test1"])) pv1$ppv["test1"] else NA
                ppv_test2 <- if (!is.null(pv1$ppv["test2"])) pv1$ppv["test2"] else NA
                ppv_diff <- if (!is.null(pv1$ppv["diff"])) pv1$ppv["diff"] else NA
                ppv_pval <- if (!is.null(pv1$ppv["p.value"])) pv1$ppv["p.value"] else NA
                ppv_stat <- if (!is.null(pv1$ppv["test.statistic"])) pv1$ppv["test.statistic"] else NA
                
                npv_test1 <- if (!is.null(pv1$npv["test1"])) pv1$npv["test1"] else NA
                npv_test2 <- if (!is.null(pv1$npv["test2"])) pv1$npv["test2"] else NA
                npv_diff <- if (!is.null(pv1$npv["diff"])) pv1$npv["diff"] else NA
                npv_pval <- if (!is.null(pv1$npv["p.value"])) pv1$npv["p.value"] else NA
                npv_stat <- if (!is.null(pv1$npv["test.statistic"])) pv1$npv["test.statistic"] else NA
                
                pv1 <- data.frame(
                    Test = c("PPV", "NPV"),
                    Test1 = c(ppv_test1, npv_test1),
                    Test2 = c(ppv_test2, npv_test2),
                    Difference = c(ppv_diff, npv_diff),
                    p.value = c(ppv_pval, npv_pval),
                    TestStatistic = c(ppv_stat, npv_stat),
                    Method = "Generalized Score Statistic (gs)",
                    stringsAsFactors = FALSE
                )
            }

            # Combine all results, ensuring they have matching columns
            comb <- rbind(spec.tmp, spec.tmp2, pv1)
            
            # Round if requested and if there's data to round
            if (nrow(comb) > 0 && input$roundDT2) {
                numeric_cols <- sapply(comb, is.numeric)
                comb[numeric_cols] <- round(comb[numeric_cols], 2)
            }
            
            if (nrow(res) > 0 && input$roundDT2) {
                numeric_cols <- sapply(res, is.numeric)
                res[numeric_cols] <- round(res[numeric_cols], 2)
            }

            # Display results
            output$ptab3 <- renderDataTable({
                if (nrow(comb) == 0) {
                    datatable(data.frame(Message = "No results available - please check your input data"),
                             options = list(dom = 't'))
                } else {
                    datatable(comb, 
                             options = list(
                                 pageLength = 20,
                                 dom = 't',
                                 ordering = FALSE,
                                 columnDefs = list(
                                     list(className = 'dt-center', targets = 1:6)
                                 )
                             ),
                             rownames = FALSE) %>%
                        formatStyle(columns = 1:ncol(comb),
                                  backgroundColor = "rgb(70,80,90)",
                                  color = "white")
                }
            })
            
            output$ptab4 <- renderDataTable({
                if ("Message" %in% names(res)) {
                    datatable(res,
                             options = list(dom = 't'),
                             rownames = FALSE) %>%
                        formatStyle(columns = 1,
                                   backgroundColor = "rgb(70,80,90)",
                                   color = "white")
                } else {
                    datatable(res,
                             options = list(
                                 pageLength = 20,
                                 dom = 't',
                                 ordering = FALSE,
                                 columnDefs = list(
                                     list(className = 'dt-center', targets = 1:8)
                                 )
                             ),
                             rownames = FALSE) %>%
                        formatStyle(columns = 1:ncol(res),
                                   backgroundColor = "rgb(70,80,90)",
                                   color = "white")
                }
            })

            # Add download handlers
            output$downloadDT2A <- downloadHandler(
                filename = function() paste0("DEAD_DualTestsA_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
                content = function(file) write.csv(comb, file, row.names = FALSE)
            )

            output$downloadDT2B <- downloadHandler(
                filename = function() paste0("DEAD_DualTestsB_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
                content = function(file) write.csv(res, file, row.names = FALSE)
            )

            # Add the statistical comparison plot
            output$stat.plt1 <- renderPlotly({
                # Create data frame for plotting with proper error checking
                tryCatch({
                    plot_data <- data.frame(
                        Test = rep(c("Sensitivity", "Specificity", "PPV", "NPV"), each = 2),
                        Method = rep(c("Test1", "Test2"), times = 4),
                        Value = c(
                            # Access sensitivity/specificity values from spec.tmp data frame
                            spec.tmp$Test1[spec.tmp$Test == "sensitivity"], 
                            spec.tmp$Test2[spec.tmp$Test == "sensitivity"],
                            spec.tmp$Test1[spec.tmp$Test == "specificity"], 
                            spec.tmp$Test2[spec.tmp$Test == "specificity"],
                            # Access PPV/NPV values from pv1 data frame
                            pv1$Test1[pv1$Test == "PPV"], 
                            pv1$Test2[pv1$Test == "PPV"],
                            pv1$Test1[pv1$Test == "NPV"], 
                            pv1$Test2[pv1$Test == "NPV"]
                        )
                    )
                    
                    # Create the plot with improved styling
                    p <- plot_ly(plot_data, x = ~Test, y = ~Value, color = ~Method, type = "bar") %>%
                        layout(
                            title = list(
                                text = "Statistical Comparison of Tests",
                                font = list(color = "white", size = 16)
                            ),
                            yaxis = list(
                                title = "Value", 
                                range = c(0, 1),
                                gridcolor = "rgba(255,255,255,0.1)",
                                zerolinecolor = "rgba(255,255,255,0.1)"
                            ),
                            xaxis = list(
                                title = "",
                                gridcolor = "rgba(255,255,255,0.1)",
                                zerolinecolor = "rgba(255,255,255,0.1)"
                            ),
                            barmode = "group",
                            showlegend = TRUE,
                            plot_bgcolor = "rgb(52,62,72)",
                            paper_bgcolor = "rgb(52,62,72)",
                            font = list(color = "white"),
                            margin = list(t = 50),
                            legend = list(
                                bgcolor = "rgba(255,255,255,0.1)",
                                font = list(color = "white")
                            )
                        )
                    
                    return(p)
                }, error = function(e) {
                    # Return an empty plot with error message if something goes wrong
                    plot_ly() %>%
                        layout(
                            title = "Error creating comparison plot",
                            annotations = list(
                                x = 0.5,
                                y = 0.5,
                                text = "Unable to generate plot - please check input data",
                                showarrow = FALSE,
                                font = list(color = "white")
                            ),
                            plot_bgcolor = "rgb(52,62,72)",
                            paper_bgcolor = "rgb(52,62,72)"
                        )
                })
            })

            # For paired test description (rename existing output)
            output$paired_diagnostic_description <- renderText({
                dat <- DTPaired_inputData()
                confInt <- input$CI2
                
                # Create paired table
                ptab <- tab.paired(d=GoldStandard, y1=Test1, y2=Test2, data=dat)
                
                # Get sensitivity/specificity results
                spec.tmp <- sesp.mcnemar(ptab)
                
                # Format numbers
                format_num <- function(x) format(round(x, 2), nsmall = 2)
                
                # Extract key differences
                sens_diff <- format_num(abs(spec.tmp$sensitivity["diff"])*100)
                sens_p <- format.pval(spec.tmp$sensitivity["p.value"], digits = 3)
                
                spec_diff <- format_num(abs(spec.tmp$specificity["diff"])*100)
                spec_p <- format.pval(spec.tmp$specificity["p.value"], digits = 3)
                
                # Calculate total sample size and disease prevalence
                total_n <- sum(ptab$diseased) + sum(ptab$non.diseased)
                prevalence <- format_num(sum(ptab$diseased) / total_n * 100)
                
                # Create description
                sprintf("
                <div style='background-color: rgb(52,62,72); padding: 15px; border-radius: 5px; margin-bottom: 20px;'>
                    <h4 style='color: white; margin-top: 0;'>Summary of Comparative Test Performance</h4>
                    <p style='color: white;'>
                    Analysis of %d subjects (disease prevalence: %s%%) compared two tests. 
                    The absolute difference in sensitivity between tests was %s%% (p = %s) and 
                    specificity differed by %s%% (p = %s). 
                    </p>
                    <p style='color: white;'>
                    Test 1 showed sensitivity of %s%% and specificity of %s%%, while 
                    Test 2 demonstrated sensitivity of %s%% and specificity of %s%%.
                    </p>
                </div>",
                total_n,
                prevalence,
                sens_diff, sens_p,
                spec_diff, spec_p,
                format_num(spec.tmp$sensitivity["test1"]*100),
                format_num(spec.tmp$specificity["test1"]*100),
                format_num(spec.tmp$sensitivity["test2"]*100),
                format_num(spec.tmp$specificity["test2"]*100))
            })
        }, error = function(e) {
            cat("\nError occurred:", e$message, "\n")
            cat("Error call:", deparse(e$call), "\n")
            # Handle any errors that occur
            msg <- paste("Error in analysis:", e$message)
            output$ptab3 <- renderDataTable({
                datatable(data.frame(Error = msg),
                         options = list(dom = 't'))
            })
            output$ptab4 <- renderDataTable({
                datatable(data.frame(Error = msg),
                         options = list(dom = 't'))
            })
        })
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
    #             txt1.CI <- roc(dat$Outcome, dat$Test1, ci = TRUE)
    #             txt1 <- paste0(round(txt1.CI$ci[2], 2), " (95% CI=", round(txt1.CI$ci[1], 2), ", ", round(txt1.CI$ci[3], 2), ")")
    #             txt2.CI <- roc(dat$Outcome, dat$Test2, ci = TRUE)
    #             txt2 <- paste0(round(txt2.CI$ci[2], 2), " (95% CI=", round(txt2.CI$ci[1], 2), ", ", round(txt2.CI$ci[3], 2), ")")
    #             roc.p <- roc.test(txt1.CI, txt2.CI)
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
            try({
                if(input$ci){
                    ci.sp.obj <- ci.sp(txt.CI, sensitivities=seq(0, 1, .01), boot.n=100)
                    ci.sp.obj <- data.frame(ci.sp.obj)
                    ci.sp.obj$Sensitivity <- rownames(ci.sp.obj)
                    p <- p%>%
                        add_trace(y = ci.sp.obj$Sensitivity, x = 1-ci.sp.obj$X2.5., type = 'scatter', mode = 'lines',
                                  line = list(color = '#dff7ff',opacity=0.05),
                                  showlegend = FALSE)%>%
                        add_trace(y = ci.sp.obj$Sensitivity, x = 1-ci.sp.obj$X97.5., type = 'scatter', mode = 'lines',
                                  fill = 'tonexty', fillcolor=list(color='#dff7ff',opacity=0.05), line = list(color = '#dff7ff',opacity=0.05),
                                  showlegend = FALSE)
                }
            })
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


                if(input$ci){
                    ci.sp.obj1 <- ci.sp(txt1.CI, sensitivities=seq(0, 1, .01), boot.n=100)
                    ci.sp.obj1 <- data.frame(ci.sp.obj1)
                    ci.sp.obj1$Sensitivity <- rownames(ci.sp.obj1)
                    p <- p%>%
                        add_trace(y = ci.sp.obj1$Sensitivity, x = 1-ci.sp.obj1$X2.5., type = 'scatter', mode = 'lines',
                                  line = list(color = '#dff7ff',opacity=0.05),
                                  showlegend = FALSE)%>%
                        add_trace(y = ci.sp.obj1$Sensitivity, x = 1-ci.sp.obj1$X97.5., type = 'scatter', mode = 'lines',
                                  fill = 'tonexty', fillcolor=list(color='#dff7ff',opacity=0.05), line = list(color = '#dff7ff',opacity=0.05),
                                  showlegend = FALSE)
                    ci.sp.obj2 <- ci.sp(txt2.CI, sensitivities=seq(0, 1, .01), boot.n=100)
                    ci.sp.obj2 <- data.frame(ci.sp.obj2)
                    ci.sp.obj2$Sensitivity <- rownames(ci.sp.obj2)
                    p <- p%>%
                        add_trace(y = ci.sp.obj2$Sensitivity, x = 1-ci.sp.obj2$X2.5., type = 'scatter', mode = 'lines',
                                  line = list(color = '#ffd394',opacity=0.05),
                                  showlegend = FALSE)%>%
                        add_trace(y = ci.sp.obj2$Sensitivity, x = 1-ci.sp.obj2$X97.5., type = 'scatter', mode = 'lines',
                                  fill = 'tonexty', fillcolor=list(color='#ffd394',opacity=0.05), line = list(color = '#ffd394',opacity=0.05),
                                  showlegend = FALSE)
                }

                incProgress(2/2, detail = paste("Plotting"))
            })
        }
        # if (input$plot_xkcd)
        #     p + theme_xkcd() #
        # else
            p
    })

    # Add ROC curve description
    output$roc_description <- renderText({
        dat <- ROCinputData()
        
        if (ncol(dat) <= 2) {
            # Single test ROC analysis
            txt.CI <- roc(Outcome ~ Test1, dat, smooth=input$smooth, ci = TRUE)
            
            sprintf("
            <div style='background-color: rgb(52,62,72); padding: 15px; border-radius: 5px; margin-bottom: 20px;'>
                <h4 style='color: white; margin-top: 0;'>Summary of ROC Analysis</h4>
                <p style='color: white;'>
                Analysis of %d measurements showed an area under the ROC curve (AUC) of %.2f 
                (95%% CI: %.2f to %.2f). This indicates %s discriminative ability.
                </p>
            </div>",
            nrow(dat),
            txt.CI$auc,
            txt.CI$ci[1],
            txt.CI$ci[3],
            case_when(
                txt.CI$auc >= 0.9 ~ "excellent",
                txt.CI$auc >= 0.8 ~ "good",
                txt.CI$auc >= 0.7 ~ "fair",
                txt.CI$auc >= 0.6 ~ "poor",
                TRUE ~ "very poor"
            ))
        } else {
            # Comparative ROC analysis
            txt1.CI <- roc(dat$Outcome, dat$Test1, ci = TRUE)
            txt2.CI <- roc(dat$Outcome, dat$Test2, ci = TRUE)
            roc.p <- roc.test(txt1.CI, txt2.CI)
            
            sprintf("
            <div style='background-color: rgb(52,62,72); padding: 15px; border-radius: 5px; margin-bottom: 20px;'>
                <h4 style='color: white; margin-top: 0;'>Summary of Comparative ROC Analysis</h4>
                <p style='color: white;'>
                Analysis of %d measurements compared two tests. Test 1 showed an AUC of %.2f 
                (95%% CI: %.2f to %.2f), while Test 2 had an AUC of %.2f (95%% CI: %.2f to %.2f). 
                The difference between tests was %s statistically significant (p = %.3f).
                </p>
            </div>",
            nrow(dat),
            txt1.CI$auc, txt1.CI$ci[1], txt1.CI$ci[3],
            txt2.CI$auc, txt2.CI$ci[1], txt2.CI$ci[3],
            ifelse(roc.p$p.value < 0.05, "", "not"),
            roc.p$p.value)
        }
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

format_diagnostic_results <- function(ep.res, res.car, should_round) {
    # Create mapping for statistic names
    stat_names <- c(
        "ap" = "Apparent Prevalence",
        "tp" = "True Prevalence",
        "se" = "Sensitivity",
        "sp" = "Specificity",
        "diag.ac" = "Diagnostic Accuracy",
        "diag.or" = "Diagnostic Odds Ratio",
        "nnd" = "Number Needed to Diagnose",
        "youden" = "Youden's Index",
        "ppv" = "Positive Predictive Value",
        "npv" = "Negative Predictive Value",
        "plr" = "Positive Likelihood Ratio",
        "nlr" = "Negative Likelihood Ratio"
    )
    
    # Create epiR results data frame
    ep.res_df <- data.frame(
        Test = stat_names[ep.res$statistic],
        Estimate = ep.res$est,
        LowerCI = ep.res$lower,
        UpperCI = ep.res$upper,
        p.value = NA,
        stringsAsFactors = FALSE,
        row.names = NULL
    )
    
    # Get additional metrics from caret
    caret_metrics <- data.frame(
        Test = c("Balanced Accuracy", "Kappa", "McNemar's Test P-Value",
                "No Information Rate"),
        Estimate = c(res.car$byClass["Balanced Accuracy"],
                    res.car$overall["Kappa"],
                    NA,
                    res.car$overall["No Information Rate"]),
        LowerCI = NA,
        UpperCI = NA,
        p.value = c(NA, NA, 
                    res.car$overall["McnemarPValue"],
                    res.car$overall["AccuracyPValue"]),
        stringsAsFactors = FALSE,
        row.names = NULL
    )
    
    # Combine results
    results <- rbind(ep.res_df, caret_metrics)
    
    # Clean up test names to be more readable
    results$Test <- gsub("\\.", " ", results$Test)
    results$Test <- tools::toTitleCase(results$Test)
    
    # Ensure all columns are properly formatted
    results$Test <- as.character(results$Test)
    results$Estimate <- as.numeric(as.character(results$Estimate))
    results$LowerCI <- as.numeric(as.character(results$LowerCI))
    results$UpperCI <- as.numeric(as.character(results$UpperCI))
    results$p.value <- as.numeric(as.character(results$p.value))
    
    # Round numeric columns if requested
    if(should_round) {
        numeric_cols <- sapply(results, is.numeric)
        results[numeric_cols] <- round(results[numeric_cols], 2)
    }
    
    return(results)
}

get_full_diagnostic_results <- function(dat.tbl, confInt, should_round) {
    # Get epiR results
    ep.res <- summary(epi.tests(dat.tbl, conf.level = confInt))
    
    # Get caret results
    res.car <- confusionMatrix(as.table(dat.tbl))
    
    # Format and return results
    return(format_diagnostic_results(ep.res, res.car, should_round))
}

format_contingency_results <- function(test_result) {
    # Extract test statistics and format them
    if (inherits(test_result, "htest")) {
        results <- data.frame(
            Metric = c("Test Type", "Statistic", "P-value", "Alternative Hypothesis"),
            Value = c(
                test_result$method,
                sprintf("%.3f", test_result$statistic),
                sprintf("%.4f", test_result$p.value),
                test_result$alternative
            ),
            stringsAsFactors = FALSE
        )
        
        return(results)
    } else {
        return(data.frame(
            Metric = "Error",
            Value = "Invalid test result format"
        ))
    }
}



