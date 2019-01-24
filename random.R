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


