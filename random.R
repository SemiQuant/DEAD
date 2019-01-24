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



