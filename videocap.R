###Database curation##
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("actuar","AICcmodavg","agricolae","apaTables","aod","arm","astsa",
              "boot","broom","car","carData","caret","cmprsk","corrr","corrplot",
              "cowplot","correlationfunnel","explore","DescTools","DALEX","DataExplorer","datasets","DataEditR","data.table","dplyr",
              "dynlm","dlookr","editData","ellipse","easystats","esquisse","effects","effectsize","faraway","fdth","fable","flexmix","flexsurv","forcats",
              "forecast","foreign","gapminder","GGally","ggcorrplot","ggalt","ggpubr","ggstatsplot",
              "gmodels","gtsummary","ggridges","ggfortify","ggplot2","gplots","gridExtra","ggthemes","haven","HH","Hmisc",
              "hrbrthemes","ISLR","ISLR2","janitor","kableExtra","KMsurv","lubridate","lsr","lmtest","lmSupport","MASS","missForest","modelr",
              "modelsummary","modelStudio","pls","mFilter","MLmetrics","mstate","multcompView","multcomp",
              "nlme","oddsratio","parameters","PairedData","palmerpenguins","PASWR",
              "pwr","pls","PerformanceAnalytics","PASWR","picante","psych","psychometric","QuantPsyc","rlang","reshape2","rattle","ranger","RColorBrewer","readr","rmarkdown","rcompanion",
              "riskRegression","readxl","rstatix","randomForest","rpivotTable","RVAideMemoire","splines","skimr","SMPracticals",
              "stats","sjstats","ssym","summarytools","survMisc","survminer","survival","texreg","tidyverse",
              "timsac","tidyverse","timetk","tsdl","tsbox","tsibble","tseries","TSstudio","vars","viridis","wesanderson","xts","xtable","yarrr")
ipak(packages)
videocaps <- read_excel("C:/Users/Admin/Dropbox/MARÍA CAMILA/VIDEOCAPSULAS.xlsx")
videocaps<- videocaps[,c(-1,-35)]
View(videocaps)
attach(videocaps)
str(videocaps)
names(videocaps)
dim(videocaps)
head(videocaps)
tail(videocaps)

##Descriptive analysis##
#Table 1#
videocaps %>%
  shapiro_test(edad,GTT,SBTT)
summary(edad)
freq(sexo)
freq(anemia_cons)
freq(sangrado_cons)
freq(crhn_cons)
freq(estig_sangr)
freq(sangr_activ)
summary(GTT)
summary(SBTT)

freq(hall_normal)
tapply(edad, hall_normal, summary)
wilcox.test(edad ~ hall_normal, data=videocaps,
            na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
with(videocaps, ctable(sexo, hall_normal, prop = 'c'))
CrossTable(sexo, hall_normal, expected = F,
           chisq = T, prop.chisq = F, format="SPSS",
           dnn = c("Sexo", "Hallazgos"))
with(videocaps, ctable(anemia_cons, hall_normal, prop = 'c'))
CrossTable(anemia_cons, hall_normal, expected = F,
           chisq = T, prop.chisq = F, format="SPSS",
           dnn = c("Sexo", "Hallazgos"))
with(videocaps, ctable(sangrado_cons, hall_normal, prop = 'c'))
CrossTable(sangrado_cons, hall_normal, expected = F,
           chisq = T, prop.chisq = F, format="SPSS",
           dnn = c("Sexo", "Hallazgos"))
with(videocaps, ctable(crhn_cons, hall_normal, prop = 'c'))
CrossTable(crhn_cons, hall_normal, expected = F,
           chisq = T, prop.chisq = F, format="SPSS",
           dnn = c("Sexo", "Hallazgos"))
with(videocaps, ctable(calid_prepar, hall_normal, prop = 'c'))
CrossTable(calid_prepar, hall_normal, expected = F,
           chisq = T, prop.chisq = F, format="SPSS",
           dnn = c("Sexo", "Hallazgos"))
with(videocaps, ctable(estud_limit, hall_normal, prop = 'c'))
CrossTable(estud_limit, hall_normal, expected = F,
           chisq = T, prop.chisq = F, format="SPSS",
           dnn = c("Sexo", "Hallazgos"))
with(videocaps, ctable(estig_sangr, hall_normal, prop = 'c'))
CrossTable(estig_sangr, hall_normal, expected = F,
           chisq = T, prop.chisq = F, format="SPSS",
           dnn = c("Sexo", "Hallazgos"))
with(videocaps, ctable(sangr_activ, hall_normal, prop = 'c'))
CrossTable(sangr_activ, hall_normal, expected = F,
           chisq = T, prop.chisq = F, format="SPSS",
           dnn = c("Sexo", "Hallazgos"))

tapply(TTG, hall_normal, summary)
wilcox.test(GTT ~ hall_normal, data=videocaps,
            na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
tapply(TTID, hall_normal, summary)
wilcox.test(SBTT ~ hall_normal, data=videocaps,
            na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)

#Medians TTID, TTG#
anem_anormal_filt<-filter(videocaps, anemia_cons == "Yes")
wilcox.test(SBTT ~ hall_normal, data=anem_anormal_filt,
              na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
sangr_anormal_filt<-filter(videocaps, sangrado_cons == "Yes")
wilcox.test(SBTT ~ hall_normal, data=sangr_anormal_filt,
            na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
anem_anormal_filt<-filter(videocaps, anemia_cons == "Yes")
wilcox.test(GTT ~ hall_normal, data=anem_anormal_filt,
            na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
sangr_anormal_filt<-filter(videocaps, sangrado_cons == "Yes")
wilcox.test(GTT ~ hall_normal, data=sangr_anormal_filt,
            na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)

#Frequency findings#
findings <- as.numeric(findings)
summary(findings)
findings <- as.factor(findings)
tapply(GTT, findings, summary)
tapply(SBTT, findings, summary)
par(mfrow=c(1,2))
plot(findings,GTT,col = c("white","lightgray","gray","darkgray","lightblue"),
     varwidth=F, horizontal=F, ylab= "Gastric transit time (minutes)",
     xlab = "Frequency of abnormal findings (counts)")
plot(findings,SBTT,col = c("white","lightgray","gray","darkgray","lightblue"),
     varwidth=F, horizontal=F, ylab= "Small bowell transit time (minutes)",
     xlab = "Frequency of abnormal findings (counts)")

boxLabels = c("01-Red spots (28.3%)","02-Erosive (17.6%)","03-Vascular ectasia (12.5%)","04-Gastritis (9.0%)",
              "05-Ulcer (7.2%)","06-Duodenitis (6.5%)","07-Lymphangiectasia (5.7%)","08-Phlebectasia (2.9%)",
              "09-Polyps (2.5%)","10-Subepithelial lesion (1.07%)")
df <- data.frame(yAxis = length(boxLabels):10, 
                 boxProportions = c(28.3,17.6,12.5,9.0,7.2,6.5,5.7,2.9,2.5,1.07), 
                 boxCILow = c(23.1,13.3,8.9,5.9,4.4,3.9,3.3,1.2,1.02,0.22), 
                 boxCIHigh = c(33.9,22.5,17.0,12.9,10.8,10.0,9.1,5.6,5.1,3.11))

(p <- ggplot(df, aes(x = boxProportions, y = boxLabels)) + 
    geom_vline(aes(xintercept = 10), size = .8, linetype = "dashed", col= "black") + 
    geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = 1, height = .2, color = "black") +
    geom_point(size = 3.5, color = "darkblue") +
    scale_x_continuous(breaks = seq(0,35,2), labels = seq(0,35,2), limits = c(0,35)) +
    theme_economist () +
    theme(axis.title = element_text(size = 20)) +
    theme(axis.text.x = element_text(size = 16)) +
    theme(axis.text.y = element_text(size = 18)) +
    theme(axis.text = element_text(face="bold")) +
    theme(plot.title = element_text(size = 20)) +
    theme(legend.text = element_text(size = 18)) +
    theme(panel.grid.minor = element_blank()) +
    ylab("") +
    xlab("Proportion, IC95%") +
    ggtitle("Findings on endoscopic video-capsule")
)
