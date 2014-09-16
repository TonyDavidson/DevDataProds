# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CardioVascular Risk Calculator
# https://www.framinghamheartstudy.org/risk-functions/cardiovascular-disease/10-year-risk.php
# Based on Framington Heart Study
# excel spreadsheets to derive calculations downloaded 2014-09-03 @ 7:41 GMT +8hrs
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file is written for the purpose of data visualization.The calculations are as per the Framingham
# Study and should be viewed as a guide only.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(shiny)
library(ggplot2)
library(markdown)
# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
Data <- reactive({
# input ----


age <- input$age
male <- ifelse(input$gender=="Male", 0, 1)
units <- input$units
chol<-input$cholesterol
hd<-input$hdl

if(units=="mmol/l") cholesterol <- chol
if(units=="mmol/l") hdl <- hd
if(units=="mg/dl") cholesterol <- chol/0.0295
if(units=="mg/dl") hdl <- hd/0.0295
     
    
  
sbp <- input$sbp
treatment <- ifelse(input$treatment=="Yes", 0, 1)
diabetes <- ifelse(input$diabetes=="Yes", 1, 0)
smoker <- ifelse(input$smoker=="Yes", 1, 0)


# actual
# construct from input
lnage <- log(age)
lntot <- log(cholesterol)
lnhdl <- log(hdl)
lnsbp <- log(sbp)

constlntot<-log(180)
constlnhdl <- log(45)
constlnsbp <- log(125)



Coeff.m.NoTreat.Age <-3.06117
Coeff.m.NoTreat.sbp <-1.93303
Coeff.m.NoTreat.tot <-1.1237
Coeff.m.NoTreat.hdl <--0.93263
Coeff.m.NoTreat.smoke <-0.65451
Coeff.m.Treat.sbp <-1.99881
Coeff.m.Treat.diab <-0.57367
m.Constant<- 23.9802

Coeff.f.NoTreat.Age <-2.32888
Coeff.f.NoTreat.sbp <-2.76157
Coeff.f.NoTreat.tot <-1.20904
Coeff.f.NoTreat.hdl <--0.70833
Coeff.f.NoTreat.smoke <-0.52873
Coeff.f.Treat.sbp <-2.82263
Coeff.f.Treat.diab <-0.69154
f.Constant<- 26.1931

ConstiNum.f <- exp(-((Coeff.f.NoTreat.sbp*constlnsbp+Coeff.f.NoTreat.tot*constlntot+Coeff.f.NoTreat.hdl*constlnhdl-f.Constant)/2.32888))
a.f<--log(0.95012)
b.f<-(1/2.32888)
ConstiDenom.f <- a.f^b.f
Consti.f <- ConstiNum.f/ConstiDenom.f
Expo.f <-  1/2.328888
EpsBetx.f <- (lnage*Coeff.f.NoTreat.Age+Coeff.f.NoTreat.sbp*lnsbp+Coeff.f.NoTreat.tot*lntot+Coeff.f.NoTreat.hdl*lnhdl+Coeff.f.NoTreat.smoke*smoker+Coeff.f.Treat.diab*diabetes)
Exp.f<-exp(EpsBetx.f-26.1931)
RiskScore.f<-1-(0.95012^Exp.f)
Term.f <- (-log(1-RiskScore.f))^Expo.f
HeartAge.f <- Consti.f*Term.f


ConstiNum.m <- exp(-((Coeff.m.NoTreat.sbp*constlnsbp+Coeff.m.NoTreat.tot*constlntot+Coeff.m.NoTreat.hdl*constlnhdl-m.Constant)/3.06117))
a.m<--log(0.88936)
b.m<-(1/3.06117)
ConstiDenom.m <- a.m^b.m
Consti.m <- ConstiNum.m/ConstiDenom.m
Expo.m <-  1/3.06117
EpsBetx.m <- (lnage*Coeff.m.NoTreat.Age+Coeff.m.NoTreat.sbp*lnsbp+Coeff.m.NoTreat.tot*lntot+Coeff.m.NoTreat.hdl*lnhdl+Coeff.m.Treat.diab*diabetes+Coeff.m.NoTreat.smoke*smoker)
Exp.m<-exp(EpsBetx.m-23.9802)
RiskScore.m<-1-(0.88936^Exp.m)
Term.m <- (-log(1-RiskScore.m))^Expo.m
HeartAge.m <- Consti.m*Term.m


ConstiNum.f.treatbp <- exp(-((Coeff.f.Treat.sbp*constlnsbp+Coeff.f.NoTreat.tot*constlntot+Coeff.f.NoTreat.hdl*constlnhdl-f.Constant)/2.32888))
a.f<--log(0.95012)
b.f<-(1/2.32888)
ConstiDenom.f <- a.f^b.f
Consti.f.treatbp <- ConstiNum.f/ConstiDenom.f
Expo.f <-  1/2.328888
EpsBetx.f.treatbp <- (lnage*Coeff.f.NoTreat.Age+Coeff.f.Treat.sbp*lnsbp+Coeff.f.NoTreat.tot*lntot+Coeff.f.NoTreat.hdl*lnhdl+Coeff.f.Treat.diab*diabetes+Coeff.f.NoTreat.smoke*smoker)
Exp.f.treatbp<-exp(EpsBetx.f.treatbp-26.1931)
RiskScore.f.treatbp<-1-(0.95012^Exp.f.treatbp)
Term.f.treatbp <- (-log(1-RiskScore.f.treatbp))^Expo.f
HeartAge.f.treatbp <- Consti.f.treatbp*Term.f.treatbp


ConstiNum.m.treatbp <- exp(-(Coeff.m.Treat.sbp*constlnsbp+Coeff.m.NoTreat.tot*constlntot+Coeff.m.NoTreat.hdl*constlnhdl-m.Constant)/3.06117)
a.m<--log(0.88936)
b.m<-(1/3.06117)
ConstiDenom.m <- a.m^b.m
Consti.m.treatbp <- ConstiNum.m/ConstiDenom.m
Expo.m <-  1/3.06117
EpsBetx.m.treatbp<-(lnage*Coeff.m.NoTreat.Age+Coeff.m.Treat.sbp*lnsbp+Coeff.m.NoTreat.tot*lntot+Coeff.m.NoTreat.hdl*lnhdl+Coeff.m.Treat.diab*diabetes+Coeff.m.NoTreat.smoke*smoker)
Exp.m.treatbp<-exp(EpsBetx.m.treatbp-23.9802)
RiskScore.m.treatbp<-1-(0.88936^Exp.m.treatbp)
Term.m.treatbp <- (-log(1-RiskScore.m.treatbp))^Expo.m
HeartAge.m.treatbp <- Consti.m.treatbp*Term.m.treatbp


{cvr <- ifelse(male==1 & treatment==1 ,HeartAge.f,
ifelse(male==1 & treatment==0 ,HeartAge.f.treatbp,
ifelse(male==0 & treatment==1 ,HeartAge.m,
ifelse(male==0 & treatment==0 ,HeartAge.m.treatbp,
NA))))

cvr

output$prediction <- renderText({cvr})}

#Risk Score


RiskScore <- ifelse(male==1 & treatment==1 ,RiskScore.f,
ifelse(male==1 & treatment==0 ,RiskScore.f.treatbp,
ifelse(male==0 & treatment==1 ,RiskScore.m,
ifelse(male==0 & treatment==0 ,RiskScore.m.treatbp,
NA))))


# Normal Risk Score

NormRiskexp.f <- (lnage*Coeff.f.NoTreat.Age+Coeff.f.NoTreat.sbp*log(125)+Coeff.f.NoTreat.tot*log(180)+Coeff.f.NoTreat.hdl*log(45))
NormRiskScore.f <- 1-(0.95012^exp(NormRiskexp.f-26.1931))
NormRiskexp.m <- (lnage*Coeff.m.NoTreat.Age+Coeff.m.NoTreat.sbp*log(125)+Coeff.m.NoTreat.tot*log(180)+Coeff.m.NoTreat.hdl*log(45))
NormRiskScore.m <- 1-(0.88936^exp(NormRiskexp.m-23.9802))
NormRiskScore <- ifelse(male==1 ,NormRiskScore.f,
ifelse(male==0 ,NormRiskScore.m,
NA))



# Optimal Risk Score
OptExp.f<- (lnage*Coeff.f.NoTreat.Age+Coeff.f.NoTreat.sbp*log(110)+Coeff.f.NoTreat.tot*log(160)+Coeff.f.NoTreat.hdl*log(60))
OptRiskScore.f <- 1-(0.95012^exp(OptExp.f-26.1931))
OptExp.m<- (lnage*Coeff.m.NoTreat.Age+Coeff.m.NoTreat.sbp*log(110)+Coeff.m.NoTreat.tot*log(160)+Coeff.m.NoTreat.hdl*log(60))
OptRiskScore.m <- 1-(0.88936^exp(OptExp.m-23.9802))


OptRiskScore <- ifelse(male==1 ,OptRiskScore.f,
ifelse(male==0 ,OptRiskScore.m,
NA))

# dataframe ----

dat <- NULL
dat <- as.data.frame(dat)


dat[1,1] <- 1
dat[1,2] <- "Optimal Risk"
dat[1,3] <- OptRiskScore*100

dat[2,1] <- 0
dat[2,2] <- "Your Risk"
dat[2,3] <- RiskScore*100

dat[3,1] <- 2
dat[3,2] <- "Normal Risk"
dat[3,3] <- NormRiskScore*100
names(dat) <- c("colour", "measure", "value")
dat



})
# plot ----
output$riskPlot <- renderPlot( {
dat <- Data()
p <- ggplot(dat, aes(x=measure, y=value, colour=factor(colour))) +
theme_bw() +
geom_point(size=8) +
geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=Inf),
fill="pink", alpha = 0.05, linetype=0) +
ylab("Risk (%)") +
scale_colour_manual(values=c("red", "green","blue"),
name="Comparison",
breaks=c(0,1,2),
labels=c("You", "Optimal You", "Normal You")) +
theme(axis.title.x = element_text(face="bold", size=20)) +
theme(axis.title.y = element_text(face="bold", size=20)) +
theme(axis.text.x = element_text(face="bold", size=15)) +
theme(axis.text.y = element_text(face="bold", size=15)) +
labs(title = "CardioVascular Risk Calculator") +
theme(plot.title = element_text(size = rel(2)))
print(p)
})
output$readme <- renderUI({
includeMarkdown("readme.md")
})
})