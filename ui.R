#install.packages("shiny","leaflet")
library(leaflet)
library(shiny)
library(shinyWidgets)
library(plotly)
library(DT)
library(dplyr)
library(shinythemes)
setwd("/Volumes/KINGSTON/projects/houseprice_app")
mean_income = read.csv("CSV_Files/mean_monthly_household_income_1970-2020.csv")
mean_houseprice = read.csv("CSV_Files/quarter_house_price_2009-2021.csv")
cordinate = read.csv("CSV_Files/cordinate.csv")
affordability = read.csv("CSV_Files/affordability.csv")

#make the data year to be consecutive
Year = data.frame(Year = seq(1970, 2020, 0.25))
##Outer merge the Year created with the mean_income
mean_income = merge(Year, mean_income, by = "Year", all = T)

##replace n.a. or "" with null
library(naniar)
mean_income = replace_with_na_all(data = mean_income, condition = ~ . %in% c("n.a", ""))
mean_income[, 2:24] <- sapply(mean_income[, 2:24], as.numeric)

##use linear interpolation to fill up the null value in each column
library(imputeTS)
for (i in c(2:24)) {
  mean_income[, i] = na_interpolation(mean_income[, i], option = "linear")
}

mean_income2 = data.frame(t(mean_income))
mean_income2 = cbind(rownames(mean_income2),
                     data.frame(mean_income2, row.names = NULL))[c(1, 2, 9:24), ]
colnames(mean_income2) = mean_income2[1, ]
mean_income2 = mean_income2[-1, ]

mean_houseprice2 = data.frame(t(mean_houseprice))
mean_houseprice2 = cbind(rownames(mean_houseprice2),
                         data.frame(mean_houseprice2, row.names = NULL))
colnames(mean_houseprice2) = mean_houseprice2[1, ]
mean_houseprice2 = mean_houseprice2[-1, ]

affordability2 = data.frame(t(affordability))
affordability2 = cbind(rownames(affordability2),
                       data.frame(affordability2, row.names = NULL))
colnames(affordability2) = affordability2[1, ]
affordability2 = affordability2[-1, ]
statess1 = mean_income2[, 1]
statess2 = mean_houseprice2[, 1]
statess3 = affordability2[, 1]
mean_income3 = subset (mean_income, select = -c(3:8))

ui <-
  navbarPage(
    theme = shinytheme("sandstone"),
    "Malaysia Mean House House Price and Mean Household Income",
    id = "nav",
    tabPanel("Interactive map", (
      div(
        class = "outer",
        tags$head(# Include our custom CSS
          includeCSS("style.css")),
        
        leafletOutput("map", width = "100%", height = "100%"),
        absolutePanel(
          id = "controls",
          class = "panel panel-default",
          fixed = TRUE,
          draggable = TRUE,
          top = 60,
          left = "auto",
          right = 20,
          bottom = "auto",
          width = 330,
          height = "auto",
          uiOutput("slider"),
          textOutput("Malaysia"),
          plotOutput("timeplot", height = 200),
          plotOutput("timeplotstate", height = 250)
        ),
        absolutePanel(
          id = "map_input",
          class = "panel panel-default",
          fixed = TRUE,
          draggable = FALSE,
          top = 60,
          left = 45,
          right = "auto",
          bottom = "auto",
          width = 330,
          selectInput(
            "select_map_types",
            h3("Select map types"),
            choices = list(
              "House Price" = 1,
              "Household Income" = 2,
              "Affordability" = 3
            ),
            selected = 1
          )
        ),
        
        
      )
    )),
    tabPanel("Comparison",
             
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "comparison_type",
                   "Type of Comparison",
                   c(
                     "Mean Income" = 1,
                     "Mean Housprice" = 2,
                     "Affordability" = 3
                   ),
                   selected = 1
                 ),
                 conditionalPanel(
                   condition = "input.comparison_type == 1",
                   pickerInput(
                     "state_select1",
                     "States:",
                     choices = as.character(statess1),
                     options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                     selected = statess1[1],
                     multiple = TRUE
                   ),
                 ),
                 
                 conditionalPanel(
                   condition = "input.comparison_type == 2",
                   pickerInput(
                     "state_select2",
                     "States:",
                     choices = as.character(statess2),
                     options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                     selected = statess1[1],
                     multiple = TRUE
                   ),
                 ),
                 conditionalPanel(
                   condition = "input.comparison_type == 3",
                   pickerInput(
                     "state_select3",
                     "States:",
                     choices = as.character(statess3),
                     options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                     selected = statess1[1],
                     multiple = TRUE
                   ),
                 )
               ),
               
               
               mainPanel(plotlyOutput("comparison_plot"), width = 6)
               
             )),
    tabPanel(
      "Ranking",
      titlePanel("Ranking of Each State"),
      
      # Create a new Row in the UI for selectInputs
      fluidRow(column(
        4,
        selectInput(
          "comparison_type2",
          "Type of Comparison",
          c(
            "Mean Income" = 1,
            "Mean Housprice" = 2,
            "Affordability" = 3
          ),
          selected = 1
        )
        
      ),
      column(4,
             uiOutput("slider2")),),
      DTOutput('tbl')
    )
    
    
  )
