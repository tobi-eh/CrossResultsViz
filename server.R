source("R/functions.R")
options(stringsAsFactors = F)

### SERVER
shinyServer(function(input, output, session) {
  #
  racePreSelection <- reactiveVal({ NULL })
  #
  observe({
    query <- parseQueryString(session$clientData$url_search)
    # print(query)
    if (!is.null(query[['raceID']])) {
      updateTextInput(session, "raceLink", value = query[['raceID']])
    }
    if (!is.null(query[['raceName']])) {
      racePreSelection(query[['raceName']])
    }
  })
  #
  #
  raceLink <- reactive({
    raceLink <- input$raceLink
    raceLink <- CleanRaceLink(raceLink)
    raceLink 
  })
  #
  racePage <- reactiveVal(NULL)
  #
  start.laps.finish.tab <- reactive({
    # raceLink <- input$raceLink
    raceSelected <- input$raceSelection #"Men 1/2/3 and  40+"
    validate(need(!is.null(raceSelected),""))
    #
    page <- racePage()
    validate(need(!is.null(page), ""))
    #
    raceOptions <- GetRaceOptionsFromPage(page)
    #
    # extract times into table
    raceTable <- GetRaceTableFromPage(page, raceSelected, raceOptions)
    GetRaceTimeTable(raceTable)
  })
  # 
  #
  output$raceOptions <- renderUI({
    validate(need(RaceLinkOk(raceLink()),"Invalid Race Link!"))
    racePage(ReadRacePage(raceLink()))
    # 
    racePreSelected <- racePreSelection()
    # print(racePreSelected)
    raceOptions <- GetRaceOptionsFromPage(racePage())
    if (!is.null(racePreSelected) && !racePreSelected %in% names(raceOptions)) {
      racePreSelected <- NULL
    } else {
      racePreSelected <- raceOptions[racePreSelected]
    }
    #
    selectInput("raceSelection", label = "Select Race:",
                choices = unname(raceOptions), 
                selected = racePreSelected,
                multiple = F)
  })
  output$lapSortOptions <- renderUI({
    nLaps <- ncol(start.laps.finish.tab())-2
    sliderInput("lapSortSelection", "Show Rankings for Lap",
                min = 1, max = nLaps, 
                value = nLaps, step = 1,
                pre = "Lap ",width = "100%", #sep = ",",
                animate = TRUE)
  })
  #
  #
  output$raceTitle <- renderUI({
    validate(need(!is.null(racePage()), ""))
    HTML(str_c("<div><h2>", 
               GetRaceTitle(racePage()), 
               "</h2></div>"))
  })
  #
  #
  output$timetab <- renderDataTable({
    #
    tab.show <- start.laps.finish.tab()
    #
    # tab.show <- as.data.frame(tab.show)
    tab.show[,ncol(tab.show)] <- attr(tab.show[,ncol(tab.show)], ATTR_PLACING)
    tab.show <- cbind(data.frame(Team=comment(tab.show)), tab.show)
    tab.show[,ncol(tab.show)] <- str_pad(tab.show[,ncol(tab.show)], width = 10, pad='0', side = "left") # to make sorting work properly
    # colnames(tab.show)[ncol(tab.show)-1] <- str_c(colnames(tab.show)[ncol(tab.show)-1], " / Finish Time")
    datatable(tab.show, rownames = str_c("<b>",str_replace_all(rownames(tab.show),"\\."," "),"</b>"), escape=F,
              caption=HTML("Cumulative Lap Times + Start and Finish Positions (Start position from CrossResults <i>'Carried Points'</i>)."),
              options=list(
                pageLength = nrow(tab.show),
                autoWidth = F,
                dom='tic',
                columnDefs = list(
                  list( # center align placing columns
                    className = 'dt-center', targets=c(2,ncol(tab.show))),
                  list( ## sorting of placing column
                    targets = ncol(tab.show),
                    render = JS(
                      "function(data, type, row, meta) {",
                      "  var res = (type ==='display') ? Number(data) : data;",
                      "  if (isNaN(res)) res = data.replace(/^0+/gi,''); ",
                      "  console.log(type + ' ' + data + ' ' + res);", # DEBUG msg
                      "  return res; ",
                      "}")),
                      ## attempted to simply have values and use sort return, but doesn't work :(
                      # "function(data, type, row, meta) {",
                      # "  var res = (type ==='sort') ? Number(data) : data;",
                      # "  if (isNaN(res)) res = 999; ",
                      # "  if (type === 'type') res = 'num'; ", # TODO Somehow this doesn't work :(
                      # "  console.log(type + ' ' + data + ' ' + res);", # DEBUG msg
                      # "  return res; ",
                      # "}")),
                  list( ## display lap time seconds as min:sec
                    targets = 3:(ncol(tab.show)-1),
                    render = JS(
                      "function(data, type, row, meta) {",
                      "  function pad(n, width, z) { z = z || '0'; n = n + ''; return n.length >= width ? n : new Array(width - n.length + 1).join(z) + n; }",
                      "  var res = data;",
                      "  if (type =='display') { res = (Math.floor(data/60)+':'+pad(data%60, 2,'0')); res = res=='0:00' ? '' : res; }",
                      "  return res;",
                      "}"))
                )
              ))
  })
  #
  #
  output$racePlot <- renderPlot({
    tab <- start.laps.finish.tab()
    lapSortSelection <- input$lapSortSelection
    # print(lapSortSelection)
    highlightRacers <- rownames(tab)[input$timetab_rows_selected]
    # 
    # tryCatch({
        PlotRace(tab, orderByLap=lapSortSelection, highlightRacers=highlightRacers)
    #   }, 
    #   error = function(e) validate(need(F, "Something went wrong. Likely, your screen is currently not wide enough. If you are on your smartphone, try rotating your screen! (The plots need width to display properly!)"))
    # )
  }, 
  height=function(){
    nrow(start.laps.finish.tab())*20+100
  })
  #
  #
  output$raceProgressionPlot <- renderPlot({
    tab <- start.laps.finish.tab()
    highlightRacers <- rownames(tab)[input$timetab_rows_selected]
    # 
    PlotRaceProgression(tab, highlightRacers=highlightRacers, showRanksRatherThanTime=as.logical(input$raceProgressionChartType))
  }, 
  height=function(){
    nrow(start.laps.finish.tab())*20+100
  })
})
