# Create folder and download data
if (!dir.exists("./data")) dir.create("./data")
url <- "https://github.com/pabrodez/LDSQ_2017/blob/master/data/ANON_EDashboard_Jun-Nov2017.xlsx?raw=true"
download.file(url, destfile = "./data/data_raw.xlsx", mode = "wb")

# Load packages
library(xlsx)
library(tidyverse)
library(lubridate)
library(data.table)
library(devtools)
library(makeR)
library(chron)
library(lattice)
library(grid)
# 1. Read data -----------------------------------------------------------
list.files("./data", pattern = ".xlsx")
adults_raw <-  read.xlsx("./data/data_raw.xlsx", sheetIndex = 1, startRow = 2, header = TRUE, 
                         colClasses = rep("character", 50), stringsAsFactors = FALSE)
child_raw <- read.xlsx("./data/data_raw.xlsx", sheetIndex = 2, startRow = 2, header = TRUE, 
                       colClasses = rep("character",53), stringsAsFactors = FALSE)

# lower case
adults_raw[] <- lapply(adults_raw, tolower)
child_raw[] <- lapply(child_raw, tolower)

# Substitue blank rows and non-valid with NA. Using na.string in read.xlsx throws error
adults_raw[adults_raw == ""] <- NA
adults_raw[adults_raw == "n/a"] <- NA
adults_raw[adults_raw == "na"] <- NA

child_raw[child_raw == ""] <- NA
child_raw[child_raw == "n/a"] <- NA
child_raw[child_raw == "na"] <- NA

# Remove rows with empty values in all columns
rem_row <- function(x) {
        x[rowSums(is.na(x)) != ncol(x), ]
}
adults_raw <- rem_row(adults_raw)
child_raw <- rem_row(child_raw)

# Remove columns with empty values in all rows
rem_col <- function(x) {
        x[, colSums(is.na(x)) != nrow(x)]
}
adults_raw <- rem_col(adults_raw)
child_raw <- rem_col(child_raw)
        
# Set class of columns
adults_dates <- read.xlsx("./data/data_raw.xlsx", sheetIndex = 1, colIndex = 2, startRow = 2, header = TRUE, colClasses = "Date")
adults_dates <- data.frame(rem_col(adults_dates))
adults_dates <- data.frame(rem_row(adults_dates))
adults_raw$Date.attended <- adults_dates[, 1]

child_dates <- read.xlsx("./data/data_raw.xlsx", sheetIndex = 2, colIndex = 2, startRow = 2, header = TRUE, colClasses = "Date")
child_dates <- data.frame(rem_col(child_dates))
child_dates <- data.frame(rem_row(child_dates))
child_raw$Date.attended <- child_dates[, 1]
# set Date as data type of 2 col in colClasses arg in read.xlsx

adults_raw$Age <- as.numeric(adults_raw$Age)
adults_raw$LDSQ.. <- as.numeric(adults_raw$LDSQ..)

child_raw$Age <- as.numeric(child_raw$Age)
child_raw$CAIDSQ.. <- as.numeric(child_raw$CAIDSQ..)




# 2. Check unique values and make corrections--------------------------------------------------
# Adults
unique(adults_raw$Referrer)
adults_raw$Referrer[adults_raw$Referrer == "police" | adults_raw$Referrer == "social services"] <- "police/social services"
unique(adults_raw$Pathway)
unique(adults_raw$Gender)
unique(adults_raw$Area.of.residence)
adults_raw$Area.of.residence[adults_raw$Area.of.residence == "no fixed abode"] <- "no fixed above"
adults_raw$Area.of.residence[adults_raw$Area.of.residence == "gm salford"] <- "gm-salford"
unique(adults_raw$Ethnicity)
adults_raw$Ethnicity[adults_raw$Ethnicity == "not known"] <- NA
adults_raw$Ethnicity[adults_raw$Ethnicity == "not given"] <- NA
unique(adults_raw$Religion)
unique(adults_raw$Uni..Student)
adults_raw$Uni..Student[adults_raw$Uni..Student == "unknown"] <- NA
unique(adults_raw$Physical.disability)
adults_raw$Physical.disability[adults_raw$Physical.disability == "yes, details in comments"] <- "yes"
adults_raw$Physical.disability[adults_raw$Physical.disability == "unknown"] <- NA
unique(adults_raw$Learning.disability)
adults_raw$Learning.disability[adults_raw$Learning.disability == "unknown"] <- NA
unique(adults_raw$Mental.health)
adults_raw$Mental.health[adults_raw$Mental.health == "unknown"] <- NA
unique(adults_raw$Interpreter.used)
adults_raw$Interpreter.used[adults_raw$Interpreter.used == "english speaking"] <- "yes" 
unique(adults_raw$DV.history)
adults_raw$DV.history[adults_raw$DV.history == "unknown"] <- NA
unique(adults_raw$If.yes..DASH.done)
unique(adults_raw$DASH.Score)
unique(adults_raw$Referred.to.MARAC)
unique(adults_raw$FME.context)
unique(adults_raw$Time.since.assault)
unique(adults_raw$Assault.type.1)
adults_raw$Assault.type.1[adults_raw$Assault.type.1 == "unknown"] <- NA
adults_raw$Assault.type.1[adults_raw$Assault.type.1 == "digital vaginal"] <- "digital penetration"
unique(adults_raw$Assault.type.2)
adults_raw$Assault.type.2[adults_raw$Assault.type.2 == "unknown"] <- NA
unique(adults_raw$Assault.type.3)
adults_raw$Assault.type.3[adults_raw$Assault.type.3 == "unknown"] <- NA
unique(adults_raw$Assault.type.4)
unique(adults_raw$Strangulation)
adults_raw$Strangulation[adults_raw$Strangulation == "notrecorded in notes"] <- NA
unique(adults_raw$No..of.perps.)
adults_raw$No..of.perps.[adults_raw$No..of.perps. == "unknown"] <- NA
unique(adults_raw$Relationship.to.alleged.perp.)
adults_raw$Relationship.to.alleged.perp.[adults_raw$Relationship.to.alleged.perp. == "unknown"] <- NA
adults_raw$Relationship.to.alleged.perp.[adults_raw$Relationship.to.alleged.perp. == "aquaintance >24 hours "] <- "acquaintance > 24 hours"
unique(adults_raw$Met.on.internet)
adults_raw$Met.on.internet[adults_raw$Met.on.internet == "vaginal rape"] <- NA
unique(adults_raw$Emergency.contraception)
adults_raw$Emergency.contraception[adults_raw$Emergency.contraception == "not documented"] <- NA
unique(adults_raw$HIV.PEP)
adults_raw$HIV.PEP[adults_raw$HIV.PEP == "not documented"] <- NA
unique(adults_raw$Hep.B)
adults_raw$Hep.B[adults_raw$Hep.B == "not documented"] <- NA
unique(adults_raw$Self.harm)
adults_raw$Self.harm[adults_raw$Self.harm == "not recorded"] <- NA
unique(adults_raw$Substance.misuse)
adults_raw$Substance.misuse[adults_raw$Substance.misuse == "not recorded"] <- NA
unique(adults_raw$Sex.Worker)
adults_raw$Sex.Worker[adults_raw$Sex.Worker == "unknown"] <- NA
unique(adults_raw$Client.referred.to.aftercare)
adults_raw$Client.referred.to.aftercare[adults_raw$Client.referred.to.aftercare == "rasa"] <- "rasasc"
unique(adults_raw$Chain.of.custody.correct)
unique(adults_raw$LDSQ..)

# Children
unique(child_raw$Referrer)
child_raw$Referrer[child_raw$Referrer == "police" | child_raw$Referrer == "social services"] <- "police/social services"
unique(child_raw$Pathway)
unique(child_raw$Gender)
unique(child_raw$Area.of.residence)
child_raw$Area.of.residence[child_raw$Area.of.residence == "gm - manchester"] <- "gm-manchester"
unique(child_raw$Ethnicity)
child_raw$Ethnicity[child_raw$Ethnicity == "not known"] <- NA
child_raw$Ethnicity[child_raw$Ethnicity == "not given"] <- NA
child_raw$Ethnicity[child_raw$Ethnicity == "white - britishn"] <- "white - british"
unique(child_raw$Religion)
unique(child_raw$Physical.disability)
child_raw$Physical.disability[child_raw$Physical.disability == "unknown"] <- NA
unique(child_raw$Learning.disability)
child_raw$Learning.disability[child_raw$Learning.disability == "unknown"] <- NA
unique(child_raw$Mental.health)
child_raw$Mental.health[child_raw$Mental.health == "unknown"] <- NA
unique(child_raw$Interpreter.used)
child_raw$Interpreter.used[child_raw$Interpreter.used == "english speaking"] <- "yes" 
unique(child_raw$DV.history)
child_raw$DV.history[child_raw$DV.history == "unknown"] <- NA
child_raw$NA..2 <- NULL
unique(child_raw$If.yes..DASH.done)
unique(child_raw$If.done..DASH.Score)
unique(child_raw$Referred.to.MARAC)
unique(child_raw$FME.context)
unique(child_raw$Time.since.assault)
child_raw$Time.since.assault[child_raw$Time.since.assault == "<1year"] <- "<1 year"
unique(child_raw$Assault.type.1)
child_raw$Assault.type.1[child_raw$Assault.type.1 == "unknown"] <- NA
child_raw$Assault.type.1[child_raw$Assault.type.1 == "not disclosed"] <- NA
child_raw$Assault.type.1[child_raw$Assault.type.1 == "sexual touching"] <- "non penetrative assault"
unique(child_raw$Assault.type.2)
child_raw$Assault.type.2[child_raw$Assault.type.2 == "unknown"] <- NA
child_raw$Assault.type.2[child_raw$Assault.type.2 == "digital"] <- "digital penetration"
unique(child_raw$Assault.type.3)
unique(child_raw$Assault.type.4)
unique(child_raw$Strangulation)
child_raw$Strangulation[child_raw$Strangulation == "notrecorded in notes"] <- NA
unique(child_raw$No..of.perps.)
child_raw$No..of.perps.[child_raw$No..of.perps. == "unknown"] <- NA
unique(child_raw$Relationship.to.alleged.perp.)
child_raw$Relationship.to.alleged.perp.[child_raw$Relationship.to.alleged.perp. == "unknown"] <- NA
child_raw$Relationship.to.alleged.perp.[child_raw$Relationship.to.alleged.perp. == "acquaintance >24 hours"] <- "acquaintance > 24 hours"
child_raw$Relationship.to.alleged.perp.[child_raw$Relationship.to.alleged.perp. == "aunty"] <- "aunt"
unique(child_raw$Met.on.internet)
unique(child_raw$Emergency.contraception)
child_raw$Emergency.contraception[child_raw$Emergency.contraception == "already had"] <- "not required"
unique(child_raw$HIV.PEP)
child_raw$HIV.PEP[child_raw$HIV.PEP == "not documented"] <- NA
unique(child_raw$Hep.B)
child_raw$Hep.B[child_raw$Hep.B == "not documented"] <- NA
unique(child_raw$U.16.DVD)
child_raw$U.16.DVD[child_raw$U.16.DVD == "dvd"] <- "yes"
unique(child_raw$STI.screening.status)
unique(child_raw$Tested.for.HIV)
unique(child_raw$Tested.for.HepB)
unique(child_raw$Tested.for.HepC)
unique(child_raw$Self.harm)
child_raw$Self.harm[child_raw$Self.harm == "recent"] <- "recent/current"
child_raw$Self.harm[child_raw$Self.harm == "not recorded"] <- NA
unique(child_raw$Substance.misuse)
child_raw$Substance.misuse[child_raw$Substance.misuse == "not recorded"] <- NA
unique(child_raw$Child.safeguarding.referral)
unique(child_raw$FGM)
unique(child_raw$CSE.CSE.risk)
child_raw$CSE.CSE.risk[child_raw$CSE.CSE.risk == "unknown"] <- NA
unique(child_raw$Client.referred.to.aftercare)
child_raw$Client.referred.to.aftercare[child_raw$Client.referred.to.aftercare == "rasa"] <- "rasasc"
unique(child_raw$Chain.of.custody.correct)
child_raw$Chain.of.custody.correct[child_raw$Chain.of.custody.correct == "not correct"] <- "no"
unique(child_raw$CAIDSQ..)



# 3. Group variables and create new ones------------------------------------------------------
# New DF with Date.attended from both DFs. For use later in calendar heat-map and time-series
child_dates <- ymd(child_raw$Date.attended)
adults_dates <- ymd(adults_raw$Date.attended)
dates_all<- c(child_dates, adults_dates)
dates_all <- tibble(Date = dates_all)

dates_all <- dates_all %>% group_by(Date) %>% summarise(date_count = n())

dates_all %>% mutate(month = month(Date)) %>% 
        group_by(month) %>% summarise(month_count = n()) %>% 
        ggplot(.) + aes(x = month, y = month_count) + geom_line()  # 1st look

# calendar heat: https://github.com/jbryer/makeR/blob/master/R/calendarHeat.R -----------------------------------------------------------

calendario <- function(dates, 
                         values, 
                         ncolors=99, 
                         color="r2g", 
                         varname="Values",
                         date.form = "%Y-%m-%d", ...) {
        require(lattice)
        require(grid)
        require(chron)
        if (class(dates) == "character" | class(dates) == "factor" ) {
                dates <- strptime(dates, date.form)
        }
        caldat <- data.frame(value = values, dates = dates)
        min.date <- as.Date(paste(format(min(dates), "%Y"),
                                  "-1-1",sep = ""))
        max.date <- as.Date(paste(format(max(dates), "%Y"),
                                  "-12-31", sep = ""))
        dates.f <- data.frame(date.seq = seq(min.date, max.date, by="days"))
        
        # Merge moves data by one day, avoid
        caldat <- data.frame(date.seq = seq(min.date, max.date, by="days"), value = NA)
        dates <- as.Date(dates) 
        caldat$value[match(dates, caldat$date.seq)] <- values
        
        caldat$dotw <- as.numeric(format(caldat$date.seq, "%w"))
        caldat$woty <- as.numeric(format(caldat$date.seq, "%U")) + 1
        caldat$yr <- as.factor(format(caldat$date.seq, "%Y"))
        caldat$month <- as.numeric(format(caldat$date.seq, "%m"))
        yrs <- as.character(unique(caldat$yr))
        d.loc <- as.numeric()                        
        for (m in min(yrs):max(yrs)) {
                d.subset <- which(caldat$yr == m)  
                sub.seq <- seq(1,length(d.subset))
                d.loc <- c(d.loc, sub.seq)
        }  
        caldat <- cbind(caldat, seq=d.loc)
        
        #color styles
        r2b <- c("#0571B0", "#92C5DE", "#F7F7F7", "#F4A582", "#CA0020") #red to blue                                                                               
        r2g <- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384")   #red to green
        w2b <- c("#045A8D", "#2B8CBE", "#74A9CF", "#BDC9E1", "#F1EEF6")   #white to blue
        
        assign("col.sty", get(color))
        calendar.pal <- colorRampPalette((col.sty), space = "Lab")
        def.theme <- lattice.getOption("default.theme")
        cal.theme <-
                function() {  
                        theme <-
                                list(
                                        strip.background = list(col = "transparent"),
                                        strip.border = list(col = "transparent"),
                                        axis.line = list(col="transparent"),
                                        par.strip.text=list(cex=0.8))
                }
        lattice.options(default.theme = cal.theme)
        yrs <- (unique(caldat$yr))
        nyr <- length(yrs)
        print(cal.plot <- levelplot(value~woty*dotw | yr, data=caldat,
                                    as.table=TRUE,
                                    aspect=.12,
                                    layout = c(1, nyr),
                                    between = list(x=0, y=c(0.5, 0.5)),
                                    strip=TRUE,
                                    main = paste("Calendar Heat Map of ", varname, sep = ""),
                                    scales = list(
                                            x = list(
                                                    at= c(seq(2.9, 52, by=4.42)),
                                                    labels = month.abb,
                                                    alternating = c(1, rep(0, (nyr-1))),
                                                    tck=0,
                                                    cex = 0.7),
                                            y=list(
                                                    at = c(0, 1, 2, 3, 4, 5, 6),
                                                    labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                                                               "Friday", "Saturday"),
                                                    alternating = 1,
                                                    cex = 0.6,
                                                    tck=0)),
                                    xlim =c(0.4, 54.6),
                                    ylim=c(6.6,-0.6),
                                    cuts= ncolors - 1,
                                    col.regions = (calendar.pal(ncolors)),
                                    xlab="" ,
                                    ylab="",
                                    colorkey= list(col = calendar.pal(ncolors), width = 0.6, height = 0.5),
                                    subscripts=TRUE
        ) )
        panel.locs <- trellis.currentLayout()
        for (row in 1:nrow(panel.locs)) {
                for (column in 1:ncol(panel.locs))  {
                        if (panel.locs[row, column] > 0)
                        {
                                trellis.focus("panel", row = row, column = column,
                                              highlight = FALSE)
                                xyetc <- trellis.panelArgs()
                                subs <- caldat[xyetc$subscripts,]
                                dates.fsubs <- caldat[caldat$yr == unique(subs$yr),]
                                y.start <- dates.fsubs$dotw[1]
                                y.end   <- dates.fsubs$dotw[nrow(dates.fsubs)]
                                dates.len <- nrow(dates.fsubs)
                                adj.start <- dates.fsubs$woty[1]
                                
                                for (k in 0:6) {
                                        if (k < y.start) {
                                                x.start <- adj.start + 0.5
                                        } else {
                                                x.start <- adj.start - 0.5
                                        }
                                        if (k > y.end) {
                                                x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] - 0.5
                                        } else {
                                                x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] + 0.5
                                        }
                                        grid.lines(x = c(x.start, x.finis), y = c(k -0.5, k - 0.5), 
                                                   default.units = "native", gp=gpar(col = "grey", lwd = 1))
                                }
                                if (adj.start <  2) {
                                        grid.lines(x = c( 0.5,  0.5), y = c(6.5, y.start-0.5), 
                                                   default.units = "native", gp=gpar(col = "grey", lwd = 1))
                                        grid.lines(x = c(1.5, 1.5), y = c(6.5, -0.5), default.units = "native",
                                                   gp=gpar(col = "grey", lwd = 1))
                                        grid.lines(x = c(x.finis, x.finis), 
                                                   y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
                                                   gp=gpar(col = "grey", lwd = 1))
                                        if (dates.fsubs$dotw[dates.len] != 6) {
                                                grid.lines(x = c(x.finis + 1, x.finis + 1), 
                                                           y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
                                                           gp=gpar(col = "grey", lwd = 1))
                                        }
                                        grid.lines(x = c(x.finis, x.finis), 
                                                   y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
                                                   gp=gpar(col = "grey", lwd = 1))
                                }
                                for (n in 1:51) {
                                        grid.lines(x = c(n + 1.5, n + 1.5), 
                                                   y = c(-0.5, 6.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
                                }
                                x.start <- adj.start - 0.5
                                
                                if (y.start > 0) {
                                        grid.lines(x = c(x.start, x.start + 1),
                                                   y = c(y.start - 0.5, y.start -  0.5), default.units = "native",
                                                   gp=gpar(col = "black", lwd = 1.75))
                                        grid.lines(x = c(x.start + 1, x.start + 1),
                                                   y = c(y.start - 0.5 , -0.5), default.units = "native",
                                                   gp=gpar(col = "black", lwd = 1.75))
                                        grid.lines(x = c(x.start, x.start),
                                                   y = c(y.start - 0.5, 6.5), default.units = "native",
                                                   gp=gpar(col = "black", lwd = 1.75))
                                        if (y.end < 6  ) {
                                                grid.lines(x = c(x.start + 1, x.finis + 1),
                                                           y = c(-0.5, -0.5), default.units = "native",
                                                           gp=gpar(col = "black", lwd = 1.75))
                                                grid.lines(x = c(x.start, x.finis),
                                                           y = c(6.5, 6.5), default.units = "native",
                                                           gp=gpar(col = "black", lwd = 1.75))
                                        } else {
                                                grid.lines(x = c(x.start + 1, x.finis),
                                                           y = c(-0.5, -0.5), default.units = "native",
                                                           gp=gpar(col = "black", lwd = 1.75))
                                                grid.lines(x = c(x.start, x.finis),
                                                           y = c(6.5, 6.5), default.units = "native",
                                                           gp=gpar(col = "black", lwd = 1.75))
                                        }
                                } else {
                                        grid.lines(x = c(x.start, x.start),
                                                   y = c( - 0.5, 6.5), default.units = "native",
                                                   gp=gpar(col = "black", lwd = 1.75))
                                }
                                
                                if (y.start == 0 ) {
                                        if (y.end < 6  ) {
                                                grid.lines(x = c(x.start, x.finis + 1),
                                                           y = c(-0.5, -0.5), default.units = "native",
                                                           gp=gpar(col = "black", lwd = 1.75))
                                                grid.lines(x = c(x.start, x.finis),
                                                           y = c(6.5, 6.5), default.units = "native",
                                                           gp=gpar(col = "black", lwd = 1.75))
                                        } else {
                                                grid.lines(x = c(x.start + 1, x.finis),
                                                           y = c(-0.5, -0.5), default.units = "native",
                                                           gp=gpar(col = "black", lwd = 1.75))
                                                grid.lines(x = c(x.start, x.finis),
                                                           y = c(6.5, 6.5), default.units = "native",
                                                           gp=gpar(col = "black", lwd = 1.75))
                                        }
                                }
                                for (j in 1:12)  {
                                        last.month <- max(dates.fsubs$seq[dates.fsubs$month == j])
                                        x.last.m <- dates.fsubs$woty[last.month] + 0.5
                                        y.last.m <- dates.fsubs$dotw[last.month] + 0.5
                                        grid.lines(x = c(x.last.m, x.last.m), y = c(-0.5, y.last.m),
                                                   default.units = "native", gp=gpar(col = "black", lwd = 1.75))
                                        if ((y.last.m) < 6) {
                                                grid.lines(x = c(x.last.m, x.last.m - 1), y = c(y.last.m, y.last.m),
                                                           default.units = "native", gp=gpar(col = "black", lwd = 1.75))
                                                grid.lines(x = c(x.last.m - 1, x.last.m - 1), y = c(y.last.m, 6.5),
                                                           default.units = "native", gp=gpar(col = "black", lwd = 1.75))
                                        } else {
                                                grid.lines(x = c(x.last.m, x.last.m), y = c(- 0.5, 6.5),
                                                           default.units = "native", gp=gpar(col = "black", lwd = 1.75))
                                        }
                                }
                        }
                }
                trellis.unfocus()
        } 
        lattice.options(default.theme = def.theme)
}


calendar1 <- calendario(dates = dates_all$Date, values = dates_all$date_count)

