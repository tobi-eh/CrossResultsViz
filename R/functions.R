library(shiny)
library(DT)
library(stringr)
library(glue)
library(rvest)
library(matrixStats)
library(RColorBrewer)
options(stringsAsFactors = F)
#
#
raceLink <- "https://www.crossresults.com/race/9590"
ATTR_PLACING <- "PLACING"
# ATTR_TEAM <- "TEAM"
#
{ # UTIL FUNCTIONS/etc
  #
  #
  CleanRaceLink <- function(raceLink) str_split(raceLink,"#")[[1]][1]
  #
  #
  RaceLinkOk <- function(raceLink) {
    str_detect(raceLink, "^\\d++$") ||
      str_detect(raceLink, "(https?://)?(www\\.)(crossresults\\.com/race/)\\d+$")
  }
  #
  #
  ReadRacePage <- function(raceLink="https://www.crossresults.com/race/9313") {
    if(str_detect(raceLink,"^\\d+$")) raceLink <- str_c("https://www.crossresults.com/race/", raceLink)
    #
    page.html <- tryCatch({ 
      read_html(raceLink)
    }, 
    error= function(e) NULL )
    validate(need(!is.null(page.html), "Error Reading Page - Maybe invalid link?"))
    # print(page.html)
    # TODO error handling!!
    page.html
  }
  #
  #
  GetRaceOptionsFromPage <- function(racePage) {
    options.races <- str_trim(str_replace_all(html_text(html_nodes(racePage, xpath = "//h1")),"[\\r\\t\\n]",""))
    names(options.races) <- str_trim(str_replace_all(html_text(html_nodes(racePage, xpath = "//h1/preceding-sibling::a/@id")),"[\\r\\t\\n]",""))
    # print(options.races)
    options.races
  }
  #
  #
  GetRaceTitle <- function(racePage) {
    raceTit <- str_trim(str_replace_all(html_text(html_nodes(racePage, xpath = "//div[@id='resultstitle']/text()")),"[\\r\\t\\n]",""))[1]
    # print(options.races)
    raceTit
  }
  #
  #
  GetRaceTableFromPage <- function(racePage, raceName, raceOptions) {
    # print(raceOptions)
    # print(raceName)
    raceNr <- which(raceOptions==raceName)
    table <- 
      table <- tryCatch({ 
        html_nodes(racePage, xpath = "//table[@class='datatable1 resultstable']")[[raceNr]] %>% html_table
      }, 
      error= function(e) NULL )
    validate(need(sum(str_detect(colnames(table), '[Ll][Aa][Pp]'))>1, "No Lap Times Available"))
    table
  }
  #
  #
  GetRaceTimeTable <- function(raceTable, cumulativeTimes=T) {
    #
    riderNames <- str_c(raceTable$`First Name`," ", raceTable$`Last Name`)
    #
    lapTimes <- raceTable[,which(str_detect(colnames(raceTable), "[Ll][Aa][Pp]"))]
    lapTimesSecs <- TimeTableToSecondsTable(lapTimes) # turn times into seconds
    cumLapTimes <- SecondsTableToCumTable(lapTimesSecs) # calc cumulative time
    #
    finalLapIxs <- t(apply(cumLapTimes, 1, function(lts) {
      finalLap <- NA
      if (sum(!is.na(lts))>0) finalLap <- max(which(!is.na(lts)), na.rm = T)
      finalLap
    }))
    #
    for (ri in 1:nrow(cumLapTimes)) {
      if (is.na(finalLapIxs[ri])) next;
      cumTime <- TimeToSeconds(raceTable$Time[ri])
      if (!is.na(cumTime) && cumTime > 0) {
        cumLapTimes[ri,finalLapIxs[ri]] <- cumTime#ifelse(is.na(cumTime) && raceTable$Time[ri]!="DNF", cumTime, )
      }
    }
    #
    raceTable$`Carried Points` <- sapply(raceTable$`Carried Points`, function(ptsStr) as.numeric(str_split(ptsStr," +")[[1]][2]))
    raceTable$`Carried Points`[which(raceTable$`Carried Points`==0)] <- 999 # for first timers / day-of'ers
    #
    finishPositions <- 1:nrow(raceTable)
    attr(finishPositions, ATTR_PLACING) <- raceTable$Pl
    names(attr(finishPositions, ATTR_PLACING)) <- riderNames
    #
    tab.show <- cbind(
      data.frame(
        # get starting order from crossresults points of riders --> carried points
        "Start Position"=rank(raceTable$`Carried Points`,ties.method='first')
      ),
      cumLapTimes,
      data.frame(
        "Finish Position"=finishPositions
      )
    )
    rownames(tab.show) <- riderNames
    # attr(rownames(tab.show), ATTR_TEAM) <- raceTable$Team
    comment(tab.show) <- raceTable$Team
    #
    tab.show
  }
  #
  #
  TimeToSeconds <- function(timestring) {
    timeParts <- as.numeric(str_split(timestring, ":")[[1]])
    secs <- sum(timeParts*rev(c(1, 60, 60*60)[1:length(timeParts)])) # assuming max is 3 parts h:m:s
    secs 
  }
  #
  #
  TimeTableToSecondsTable <- function(timeTable) {
    secondsTab <- sapply(timeTable, function(rws) sapply(rws, TimeToSeconds))
  }
  #
  #
  SecondsTableToCumTable <- function(secondsTable) {
    t(apply(secondsTable,1, cumsum))
  }
  #
  #
  #
} # END # UTIL FUNCTIONS/ETC
#
#
##############################################################################
#
#
{ # PLOTTING FUNCTIONS
  #
  #
  HighlightColorsForRacers <- function(highlightRacers) {
    n <- length(highlightRacers)
    if (n == 0) {
      colors <- c()
    } else {
      if (n > 0 && n <= 4) {
        colors <- c('deepskyblue',"yellow","hotpink","red")[1:n]
      } else if (n<=12) {
        colors <- brewer.pal(n, "Set3")
      } else {
        colors <- rainbow(n)[sample(n)]
      }
      names(colors) <- highlightRacers
    }
    colors
  }
  #
  #
  PlotRace <- function(start.laps.finish.tab, 
                       orderByLap=NULL, 
                       highlightRacers=c(), highlight.colFun=HighlightColorsForRacers, highlight.lwd=4,
                       marLeft=max(nchar(rownames(start.laps.finish.tab))*0.7),
                       marBottom=5, marRight=0.5, marTop=3, timeBufferRight=20, 
                       raceName=""
                       ) 
  {
    #
    tab <- start.laps.finish.tab  # <- tab.show
    riderPlacings <- attr(tab[,ncol(tab)], ATTR_PLACING)
    lapTimeColumns <- 2:(ncol(tab)-1)
    #
    tab <- tab[nrow(tab):1,]
    if(!is.null(orderByLap)) {
      tab <- tab[order(tab[,orderByLap+1],tab[,ncol(tab)], decreasing=T, na.last=F),]
    }
    #
    multiLapRacers.maxLaps <- sapply(1:nrow(tab), function(ri) max(which(!is.na(tab[ri,lapTimeColumns])),na.rm = T))
    multiLapRacers.maxLaps[which(is.infinite(multiLapRacers.maxLaps))] <- NA
    multiLapRacers.maxLaps.cols <- lapTimeColumns[multiLapRacers.maxLaps]
    multiLapRacers.rows <- which(!is.na(multiLapRacers.maxLaps.cols))
    #
    xlim <- c(1, max(tab[,lapTimeColumns], na.rm=T)+timeBufferRight)
    ylim <- c(0.5,nrow(tab))
    #
    par(mar=c(marBottom,marLeft,marTop,marRight))
    #
    highlightColors <- highlight.colFun(highlightRacers)
    plot(rep(0,nrow(tab)), 1:nrow(tab), 
         pch="", xlim=xlim, ylim=ylim, axes=F, main=NULL, xlab=NA, ylab=NA); par(new=T)
    text(rep(0,nrow(tab)), 1:nrow(tab), labels="S",pos=2, xlim=xlim, ylim=ylim); par(new=T)
    segments(x0=0, x1=sapply(multiLapRacers.rows, function(ri) tab[ri,multiLapRacers.maxLaps.cols[ri]]),
             y0=multiLapRacers.rows,  y1=multiLapRacers.rows, 
             col=sapply(rownames(tab)[multiLapRacers.rows], function(rn) ifelse(rn%in%highlightRacers,highlightColors[rn],'grey')),
             lwd=sapply(multiLapRacers.rows, function(ri) ifelse(rownames(tab)[ri]%in%highlightRacers,highlight.lwd,1))
    ); par(new=T)
    for(lap.i in lapTimeColumns) {
      plot(tab[,lap.i], 1:nrow(tab), 
           pch=as.character(lap.i-1),
           xlim=xlim, ylim=ylim, 
           axes=F, main=NULL, xlab=NA, ylab=NA); par(new=T)
    }
    lapLeaders <- colMins(as.matrix(tab[lapTimeColumns]), na.rm=T)
    segments(x0=lapLeaders, x1=lapLeaders, y0=0.5, y1=nrow(tab), col='pink', lty=3, lwd=0.5)
    #
    # TODO add actual rider ranks for last lap (incl dnf status)
    names.show <- str_c(rownames(tab), " - ",nrow(tab):1)
    if (!is.null(orderByLap) && orderByLap == ncol(tab)-2) { # last lap
      names.show <- str_c(rownames(tab), " - ", riderPlacings[rownames(tab)])
    }
    if (length(highlightRacers) < nrow(tab)) {
      is <- which(!rownames(tab)%in%highlightRacers)
      axis(2, at=(1:nrow(tab))[is], labels=names.show[is], las=1, font=1)
    }
    if (length(highlightRacers) > 0) {
      is <- which(rownames(tab)%in%highlightRacers)
      axis(2, at=(1:nrow(tab))[is], labels=names.show[is], las=1, font=2)
    }
    timetags.minutes.max <- max(tab[,lapTimeColumns],na.rm=T)/60
    timetags.minutes <- seq(0, floor(timetags.minutes.max), by=5)
    axis(1, at=60*timetags.minutes, labels = as.character(timetags.minutes))
    #
    title(main=glue("{raceName}\nRanking after {orderByLap} Lap{ifelse(orderByLap==1,'','s')}"))
    title(xlab = "Laps over Time (Minutes)", line = 2)
  }
  #
  #
  PlotRaceProgression <- function(start.laps.finish.tab, 
                                  orderByLap=NULL, 
                                  highlightRacers=c(), highlight.colFun=HighlightColorsForRacers,  highlight.lwd=4,
                                  showRanksRatherThanTime = F, 
                                  marLeft=max(nchar(rownames(start.laps.finish.tab))*0.7),
                                  marBottom=5.5, marRight=marLeft, marTop=3, 
                                  raceName=""
                                  ) 
  { 
    #
    tab <- start.laps.finish.tab  # <- tab.show
    riderPlacings <- attr(tab[,ncol(tab)], ATTR_PLACING)
    #
    lapTimeColumns <- 2:(ncol(tab)-1)
    # tab2 <- tab
    #
    tab <- tab[order(tab[,1],decreasing = T),]
    #
    tab2 <- tab
    F.offset <- 0
    if (showRanksRatherThanTime) {
      tab[,lapTimeColumns] <- apply(tab[,lapTimeColumns],2, function(lapCumTimes) {
        lapCumTimes <- abs(rank(lapCumTimes, na.last = 'keep', ties.method='first') - 1 - length(lapCumTimes))
        lapCumTimes
      })
    } else {
      tab[,lapTimeColumns] <- apply(tab[,lapTimeColumns], 2, function(lapCumTimes) {
        lapCumTimes.notNA <- which(!is.na(lapCumTimes))
        lapCumTimes.times <- lapCumTimes[lapCumTimes.notNA]
        lcts.norm <- (lapCumTimes.times - min(lapCumTimes.times)) / (max(lapCumTimes.times) - min(lapCumTimes.times))
        lcts <- (1-lcts.norm) * (length(lcts.norm)-1) + 1 + sum(is.na(lapCumTimes))
        lapCumTimes[lapCumTimes.notNA] <- lcts
        lapCumTimes
      })
      F.offset <- 0.25
    }
    #
    highlightColors <- highlight.colFun(highlightRacers)
    #
    xlim <- c(0, length(lapTimeColumns)+F.offset)
    ylim <- c(0.5,nrow(tab))
    #
    par(mar=c(marBottom,marLeft,marTop,marRight))
    # plot.new()
    plot(rep(0,nrow(tab)), 1:nrow(tab), 
         pch="",
         xlim=xlim, ylim=ylim, 
         axes=F, main=NULL, xlab=NA, ylab=NA); par(new=T)
    #
    segments(x0=0, x1=1,
             y0=1:nrow(tab),
             y1=tab[,2],
             col=sapply(rownames(tab), function(rn) ifelse(rn %in% highlightRacers, highlightColors[rn], 'lightgrey')),
             lwd=sapply(rownames(tab), function(r) ifelse(r %in% highlightRacers, highlight.lwd, 1)),
             xlim=xlim, ylim=ylim); par(new=T)
    for (lap.i in lapTimeColumns[-length(lapTimeColumns)]) {
      segments(x0=lap.i-1, x1=lap.i, 
               y0=tab[,lap.i],
               y1=tab[,lap.i+1], 
               col=sapply(rownames(tab), function(rn) ifelse(rn %in% highlightRacers, highlightColors[rn], 'grey')),
               lwd=sapply(rownames(tab), function(r) ifelse(r %in% highlightRacers, highlight.lwd, 1)),
               xlim=xlim, ylim=ylim); par(new=T)
    }
    #
    if (!showRanksRatherThanTime) {
      segments(x0=(max(lapTimeColumns)-1), x1=(max(lapTimeColumns)-1+F.offset),
               y0=tab[,max(lapTimeColumns)],
               y1=abs(tab[,ncol(tab)] - 1 - nrow(tab)),
               col=sapply(rownames(tab), function(rn) ifelse(rn %in% highlightRacers, highlightColors[rn], 'lightgrey')),
               lwd=1,sapply(rownames(tab), function(r) ifelse(r %in% highlightRacers, highlight.lwd, 1)),
               lty=2,
               xlim=xlim, ylim=ylim); par(new=T)
    }
    #
    segments(x0=c(0,lapTimeColumns-1), x1=c(0,lapTimeColumns-1),
             y0=0.5, y1=nrow(tab), col='pink', lwd=0.5, lty=3); par(new=T)
    #
    text(rep(0,nrow(tab)), 1:nrow(tab), 
         labels="S", pos=2,
         xlim=xlim, ylim=ylim); par(new=T)
    text(rep(ncol(tab)-2+F.offset, nrow(tab)), 1:nrow(tab), 
         labels="F", pos=4,
         xlim=xlim, ylim=ylim); par(new=T)
    #
    names.show.start <- rownames(tab)
    names.show.finish.ixs <- order(tab[,ncol(tab)],decreasing=T)
    names.show.finish <- rownames(tab)[names.show.finish.ixs]
    names.show.finish.labels <- str_c(riderPlacings[names.show.finish]," - ", names.show.finish)
    if (length(highlightRacers) < length(names.show.start)) {
      is <- which(!names.show.start%in%highlightRacers)
      axis(2, at=(1:length(names.show.start))[is], labels=str_c(names.show.start, " - S",length(names.show.start):1)[is], las=1, font=1)
      is <- which(!names.show.finish%in%highlightRacers)
      # axis(4, at=(1:length(names.show.finish))[is], labels=str_c("F",length(names.show.finish):1, " - ", names.show.finish)[is], las=1, font=1)
      axis(4, at=(1:length(names.show.finish.labels))[is], labels=names.show.finish.labels[is], las=1, font=1)
    }
    if (length(highlightRacers) > 0) {
      is <- which(names.show.start%in%highlightRacers)
      axis(2, at=(1:length(names.show.start))[is], labels=str_c(names.show.start, " - S",length(names.show.start):1)[is], las=1, font=2)
      is <- which(names.show.finish%in%highlightRacers)
      # axis(4, at=(1:length(names.show.finish))[is], labels=str_c("F",length(names.show.finish):1, " - ", names.show.finish)[is], las=1, font=2)
      axis(4, at=(1:length(names.show.finish.labels))[is], labels=names.show.finish.labels[is], las=1, font=2)
    }
    axis(1, at=c(0:1,lapTimeColumns), labels=c("Start",str_c("Lap ", lapTimeColumns-1), "Finish"))
    #
    title(main=glue("{raceName}\nRace Progression"), 
          sub=ifelse(showRanksRatherThanTime,
                     "(showing riders' ranks on each lap)",
                     "(showing riders' ranks on each lap, spaced proportional to time gaps)"))
    #
  }
} # END  # PLOTTING FUNCTIONS
