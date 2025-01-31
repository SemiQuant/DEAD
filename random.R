Notes
dat=test

confInt <- input$CI2
dat.diagnose <- apply(dat, 1:2, function (x) ifelse(!x%in%c(1,0), NA, x))
dat.diagnose <- data.frame(dat.diagnose[complete.cases(dat.diagnose),])
colnames(dat.diagnose) <- c("GoldStandard", "Test1", "Test2")
dat.diagnose$GoldStandard <- factor(dat.diagnose$GoldStandard, levels=c("1","0"))
dat.diagnose$Test1 <- factor(dat.diagnose$Test1, levels=c("1","0"))
dat.diagnose$Test2 <- factor(dat.diagnose$Test2, levels=c("1","0"))
dat.diagnose.tbl1 <- as.matrix(table(dat.diagnose$Test1, dat.diagnose$GoldStandard))
dat.diagnose.tbl2 <- as.matrix(table(dat.diagnose$Test2, dat.diagnose$GoldStandard))

summary(epi.tests(dat.diagnose.tbl1, conf.level = confInt))[c("nnd", "diag.or"),]
summary(epi.tests(dat.diagnose.tbl2, conf.level = confInt))[c("nnd", "diag.or"),]




#
# txt1.CI=test1
# txt2.CI=test2




p1 <- plot_ly(x = 1-txt1.CI$specificities, y = txt1.CI$sensitivities, name = 'Test 1', type = 'scatter', mode = 'lines') %>%
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
      paper_bgcolor ="#fef7ea"
    )



p2 <- plot_ly(x = 1-txt2.CI$specificities, y = txt2.CI$sensitivities, name = 'Test 2', type = 'scatter', mode = 'lines') %>%
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
      paper_bgcolor ="#fef7ea"
    )

subplot(p1, p2, shareY = T)


%>% layout(shapes = list(
  type = "line",
  x0 = 0,
  x1 = 1,
  y0 = 0,
  y1 = 1,
  line = list(color = "white")),
  legend = list(
    x = 0.6, y = 0.4,
    font = list(
      family = "sans-serif",
      size = 12,
      color = "#000"),
    bgcolor = "#E2E2E2",
    bordercolor = "#FFFFFF",
    borderwidth = 2)
)







data(aSAH)

# Plot and CI (see plot.roc and ci for more options):
roc(aSAH$outcome, aSAH$s100b,
    percent=TRUE, plot=TRUE, ci=TRUE)


txt.CI <- roc(aSAH$outcome, aSAH$s100b)



plot(rocobj) # restart a new plot
ci.se.obj <- ci(rocobj, of="se", boot.n=500)
plot(ci.se.obj)




plot(rocobj) # restart a new plot
plot(ci.sp.obj, type="shape", col="blue")


ci.sp.obj <- ci.sp(rocobj, sensitivities=seq(0, 1, .01), boot.n=100)
ci.sp.obj <- data.frame(ci.sp.obj)
ci.sp.obj$Sensitivity <- rownames(ci.sp.obj)






input$ci

tryCatch(
  plot_ly(x = 1-txt.CI$specificities, y = txt.CI$sensitivities, name = 'Test', type = 'scatter', mode = 'lines',
          line = list(color = '#add8e6')) %>%
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
      ))%>%
    add_trace(y = ci.sp.obj$Sensitivity, x = 1-ci.sp.obj$X2.5., type = 'scatter', mode = 'lines',
              line = list(color = '#dff7ff',opacity=0.05),
              showlegend = FALSE)%>%
    add_trace(y = ci.sp.obj$Sensitivity, x = 1-ci.sp.obj$X97.5., type = 'scatter', mode = 'lines',
              fill = 'tonexty', fillcolor=list(color='#dff7ff',opacity=0.05), line = list(color = '#dff7ff',opacity=0.05),
              showlegend = FALSE)
)

p














require(plotly)
month <- c('January', 'February', 'March', 'April', 'May', 'June', 'July',
           'August', 'September', 'October', 'November', 'December')
high_2014 <- c(28.8, 28.5, 37.0, 56.8, 69.7, 79.7, 78.5, 77.8, 74.1, 62.6, 45.3, 39.9)
low_2014 <- c(12.7, 14.3, 18.6, 35.5, 49.9, 58.0, 60.0, 58.6, 51.7, 45.2, 32.2, 29.1)
data <- data.frame(month, high_2014, low_2014)
data$average_2014 <- rowMeans(data[,c("high_2014", "low_2014")])

#The default order will be alphabetized unless specified as below:
data$month <- factor(data$month, levels = data[["month"]])

plot_ly(data, x = ~month, y = ~high_2014, type = 'scatter', mode = 'lines',
             line = list(color = 'rgba(0,100,80,1)')) %>%
  add_trace(y = ~low_2014, type = 'scatter', mode = 'lines',
            fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'rgba(0,100,80,1)'),
            showlegend = FALSE, name = 'Low 2014')

