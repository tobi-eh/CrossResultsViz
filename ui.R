library(shiny)
library(DT)
library(stringr)
library(glue)
options(stringsAsFactors = F)
#
raceLink <- "https://www.crossresults.com/race/9312"

# CLIENT
ui <- fluidPage(
  theme=shinythemes::shinytheme("sandstone"),
  tags$head(
    HTML("<!-- Global site tag (gtag.js) - Google Analytics --> <script async src='https://www.googletagmanager.com/gtag/js?id=UA-77704559-4'></script> <script>window.dataLayer = window.dataLayer || []; function gtag(){dataLayer.push(arguments);} gtag('js', new Date()); gtag('config', 'UA-77704559-4'); </script>"),
    includeCSS("www/custom.css")
  ),
  # 
  titlePanel("CrossResults Viz ~ CX-Race Lap Viewer"),
  #   
  sidebarLayout(
    sidebarPanel(
      textInput("raceLink", "Enter CrossResults Race Link (only works for races with lap times):", 
                value=raceLink, placeholder=raceLink),
      uiOutput("raceOptions"),
      width=2
      #TODO crossresults race name selection -- autofill input for race options to display
    ),
    #
    # Show the table that for now is a table and sortable based on cumtime
    mainPanel(
      htmlOutput("raceTitle"),
      tabsetPanel(
        tabPanel("Results / Lap Table",
                 HTML("<div><h3>Click on table entries to select racers you want to highlight in the other tabs.<h3></div>"),
                 dataTableOutput("timetab")),
        tabPanel("Lap Chart", 
                 uiOutput("lapSortOptions"),
                 HTML("<div class='note'><b>Note:</b> To highlight certain riders in this chart, navigate to the results table (first tab) and select riders of interest.</div>"),
                 plotOutput("racePlot"),style='min-width:700px; width: auto;'),
        tabPanel("Race Progression Chart", 
                 radioButtons("raceProgressionChartType", label = "",
                              choices = list("Show Ranks / Lap" = T, "Show Time Gaps / Lap" = F), inline = T,
                              selected = T),
                 HTML("<div class='note'><b>Note:</b> To highlight certain riders in this chart, navigate to the results table (first tab) and select riders of interest.</div>"),
                 plotOutput("raceProgressionPlot"),style='min-width:900px; width: auto;'),
        tabPanel("About", includeHTML("about.html"))#,
      )
    )
  )
)
