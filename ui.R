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
library(markdown)
# Define UI
shinyUI(pageWithSidebar(
# Application title
headerPanel("10 Year CardioVascular Risk Calculator"),
sidebarPanel(
# gender
selectInput("gender", "Gender:",
choices = c("Male", "Female")),
# age
sliderInput("age", "Age: Years",
min=20, max=79, value=50),

#Choose Cholesterol Units
selectInput("units", "Units used for Cholesterol Readings:",
choices = c("mmol/l","mg/dl" )),


conditionalPanel(condition = "input.units == 'mmol/l'",
 # total cholesterol
sliderInput("cholesterol", "Total Cholesterol mmol/l:",
min=2.5, max=8.0, value=5.0, format = "#,##0.#"),
# hdl cholesterol
sliderInput("hdl", "HDL-Cholesterol mmol/l:",
min=0.5, max=3, value=1.5, format = "#,##0.#")),



conditionalPanel(condition = "input.units == 'mg/dl'",
    # total cholesterol
sliderInput("cholesterol", "Total Cholesterol: mg/dl",
min=100, max=405, value=150),
# hdl cholesterol
sliderInput("hdl", "HDL-Cholesterol: mg/dl",
min=10, max=100, value=40)),


# systolic blood pressure
sliderInput("sbp", "Systolic Blood Pressure: mmHg",
min=90, max=200, value=110),
# treatment for high blood pressure
selectInput("treatment", "Treatment for High Blood Pressure:",
choices = c("Yes", "No")),
# diabetes
selectInput("diabetes", "Diabetes:",
choices = c("Yes", "No")),
# smoker
selectInput("smoker", "Smoker:",
choices = c("Yes", "No")),
submitButton("Submit and Press after Changing Units to Refresh Sliders", icon("refresh"))
),
# plot
mainPanel(
h3('Your Cardiovascular Age in Years'),
verbatimTextOutput("prediction"),
tabsetPanel(
tabPanel("Plot", plotOutput("riskPlot")),
tabPanel("QuickGuide", uiOutput("readme"))
)
)
))