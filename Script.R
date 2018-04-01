# 1/4/18. To do list:
# 1. generate histogram for assault types (melt data before)
# 1.1. Descriptives of the whole population
# 2. generate mosaics (?)
# 3. Create a adults_dates and achild_dates dataframes grouped by count
# 4. Geocode area of residence
# 5. Map data
# Ages (numerical) boxplot
# LDSQ (numerical) scores boxplot
# Run a regex to check invalid strings and substitute them for NA


# Create folders and download data ----------------------------------------


if (!dir.exists("./data")) dir.create("./data")
if (!dir.exists("./plots")) dir.create("./plots")
url <- "https://github.com/pabrodez/LDSQ_2017/blob/master/data/data_raw.xlsx?raw=true"
download.file(url, destfile = "./data/data_raw.xlsx", mode = "wb")



# load packages -----------------------------------------------------------

library(xlsx)
library(tidyverse)
library(lubridate)
library(data.table)
library(devtools)
library(makeR)
library(chron)
library(lattice)
library(grid)
library(gridExtra)
library(ggthemes)
library(psych)
library(vcd)
library(cowplot)


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
adults_raw$Referrer[adults_raw$Referrer == "NA"] <- NA
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

# NAs plot
plotNa <- function(dataFrame, title = NULL) {
        tempDf <- as.data.frame(ifelse(is.na(dataFrame), 0, 1))
        tempDf <- tempDf[, order(colSums(tempDf))]
        tempData <- expand.grid(list(x = 1:nrow(tempDf), y = colnames(tempDf)))
        tempData$v <- as.vector(as.matrix(tempDf))
        tempData <- data.frame(x = unlist(tempData$x), y = unlist(tempData$y), v = unlist(tempData$v))
        ggplot(tempData) + geom_tile(aes(x=x, y=y, fill=factor(v))) +
                scale_fill_manual(values=c("white", "black"), name="Missing value\n1=No, 0=Yes") +
                theme_light() + ylab("") + xlab("Rows of data set") + ggtitle(title)
        
}

adults_nas<- plotNa(adults_raw)
child_nas <- plotNa(child_raw)
ggsave(filename = "adults_nas.pdf", path = "./plots", plot = adults_nas, device = "pdf", units = "cm", height = 25, width = 20)
ggsave(filename = "child_nas.pdf", path = "./plots", plot = child_nas, device = "pdf", units = "cm", height = 25, width = 20)

# 3. Group variables and create new ones------------------------------------------------------
# New DF with Date.attended from both DFs. For use later in calendar heat-map and time-series
child_dates <- ymd(child_raw$Date.attended)
adults_dates <- ymd(adults_raw$Date.attended)
dates_all<- c(child_dates, adults_dates)
dates_all <- tibble(Date = dates_all)
dates_all <- dates_all %>% group_by(Date) %>% summarise(date_count = n())

adult_date_tib <- tibble(Date = ymd(adults_raw$Date.attended))
adult_date_tib <- adult_date_tib %>% group_by(Date) %>% summarise(date_count = n())

# ldsq yes/no
adults_raw <- mutate(adults_raw, LD_diag = ifelse(LDSQ.. < 46, "yes", "no"))

# dates_all %>% mutate(month = month(Date)) %>% 
#         group_by(month) %>% summarise(month_count = n()) %>% 
#         ggplot(.) + aes(x = month, y = month_count) + geom_line()
# 1st look



# ignore: calendar heat: https://github.com/jbryer/makeR/blob/master/R/calendarHeat.R -----------------------------------------------------------
# 
# calendario <- function(dates, 
#                          values, 
#                          ncolors=99, 
#                          color="r2g", 
#                          varname="Values",
#                          date.form = "%Y-%m-%d", ...) {
#         require(lattice)
#         require(grid)
#         require(chron)
#         if (class(dates) == "character" | class(dates) == "factor" ) {
#                 dates <- strptime(dates, date.form)
#         }
#         caldat <- data.frame(value = values, dates = dates)
#         min.date <- as.Date(paste(format(min(dates), "%Y"),
#                                   "-1-1",sep = ""))
#         max.date <- as.Date(paste(format(max(dates), "%Y"),
#                                   "-12-31", sep = ""))
#         dates.f <- data.frame(date.seq = seq(min.date, max.date, by="days"))
#         
#         # Merge moves data by one day, avoid
#         caldat <- data.frame(date.seq = seq(min.date, max.date, by="days"), value = NA)
#         dates <- as.Date(dates) 
#         caldat$value[match(dates, caldat$date.seq)] <- values
#         
#         caldat$dotw <- as.numeric(format(caldat$date.seq, "%w"))
#         caldat$woty <- as.numeric(format(caldat$date.seq, "%U")) + 1
#         caldat$yr <- as.factor(format(caldat$date.seq, "%Y"))
#         caldat$month <- as.numeric(format(caldat$date.seq, "%m"))
#         yrs <- as.character(unique(caldat$yr))
#         d.loc <- as.numeric()                        
#         for (m in min(yrs):max(yrs)) {
#                 d.subset <- which(caldat$yr == m)  
#                 sub.seq <- seq(1,length(d.subset))
#                 d.loc <- c(d.loc, sub.seq)
#         }  
#         caldat <- cbind(caldat, seq=d.loc)
#         
#         #color styles
#         r2b <- c("#0571B0", "#92C5DE", "#F7F7F7", "#F4A582", "#CA0020") #red to blue                                                                               
#         r2g <- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384")   #red to green
#         w2b <- c("#045A8D", "#2B8CBE", "#74A9CF", "#BDC9E1", "#F1EEF6")   #white to blue
#         
#         assign("col.sty", get(color))
#         calendar.pal <- colorRampPalette((col.sty), space = "Lab")
#         def.theme <- lattice.getOption("default.theme")
#         cal.theme <-
#                 function() {  
#                         theme <-
#                                 list(
#                                         strip.background = list(col = "transparent"),
#                                         strip.border = list(col = "transparent"),
#                                         axis.line = list(col="transparent"),
#                                         par.strip.text=list(cex=0.8))
#                 }
#         lattice.options(default.theme = cal.theme)
#         yrs <- (unique(caldat$yr))
#         nyr <- length(yrs)
#         print(cal.plot <- levelplot(value~woty*dotw | yr, data=caldat,
#                                     as.table=TRUE,
#                                     aspect=.12,
#                                     layout = c(1, nyr),
#                                     between = list(x=0, y=c(0.5, 0.5)),
#                                     strip=TRUE,
#                                     main = paste("Calendar Heat Map of ", varname, sep = ""),
#                                     scales = list(
#                                             x = list(
#                                                     at= c(seq(2.9, 52, by=4.42)),
#                                                     labels = month.abb,
#                                                     alternating = c(1, rep(0, (nyr-1))),
#                                                     tck=0,
#                                                     cex = 0.7),
#                                             y=list(
#                                                     at = c(0, 1, 2, 3, 4, 5, 6),
#                                                     labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
#                                                                "Friday", "Saturday"),
#                                                     alternating = 1,
#                                                     cex = 0.6,
#                                                     tck=0)),
#                                     xlim =c(0.4, 54.6),
#                                     ylim=c(6.6,-0.6),
#                                     cuts= ncolors - 1,
#                                     col.regions = (calendar.pal(ncolors)),
#                                     xlab="" ,
#                                     ylab="",
#                                     colorkey= list(col = calendar.pal(ncolors), width = 0.6, height = 0.5),
#                                     subscripts=TRUE
#         ) )
#         panel.locs <- trellis.currentLayout()
#         for (row in 1:nrow(panel.locs)) {
#                 for (column in 1:ncol(panel.locs))  {
#                         if (panel.locs[row, column] > 0)
#                         {
#                                 trellis.focus("panel", row = row, column = column,
#                                               highlight = FALSE)
#                                 xyetc <- trellis.panelArgs()
#                                 subs <- caldat[xyetc$subscripts,]
#                                 dates.fsubs <- caldat[caldat$yr == unique(subs$yr),]
#                                 y.start <- dates.fsubs$dotw[1]
#                                 y.end   <- dates.fsubs$dotw[nrow(dates.fsubs)]
#                                 dates.len <- nrow(dates.fsubs)
#                                 adj.start <- dates.fsubs$woty[1]
#                                 
#                                 for (k in 0:6) {
#                                         if (k < y.start) {
#                                                 x.start <- adj.start + 0.5
#                                         } else {
#                                                 x.start <- adj.start - 0.5
#                                         }
#                                         if (k > y.end) {
#                                                 x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] - 0.5
#                                         } else {
#                                                 x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] + 0.5
#                                         }
#                                         grid.lines(x = c(x.start, x.finis), y = c(k -0.5, k - 0.5), 
#                                                    default.units = "native", gp=gpar(col = "grey", lwd = 1))
#                                 }
#                                 if (adj.start <  2) {
#                                         grid.lines(x = c( 0.5,  0.5), y = c(6.5, y.start-0.5), 
#                                                    default.units = "native", gp=gpar(col = "grey", lwd = 1))
#                                         grid.lines(x = c(1.5, 1.5), y = c(6.5, -0.5), default.units = "native",
#                                                    gp=gpar(col = "grey", lwd = 1))
#                                         grid.lines(x = c(x.finis, x.finis), 
#                                                    y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
#                                                    gp=gpar(col = "grey", lwd = 1))
#                                         if (dates.fsubs$dotw[dates.len] != 6) {
#                                                 grid.lines(x = c(x.finis + 1, x.finis + 1), 
#                                                            y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
#                                                            gp=gpar(col = "grey", lwd = 1))
#                                         }
#                                         grid.lines(x = c(x.finis, x.finis), 
#                                                    y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
#                                                    gp=gpar(col = "grey", lwd = 1))
#                                 }
#                                 for (n in 1:51) {
#                                         grid.lines(x = c(n + 1.5, n + 1.5), 
#                                                    y = c(-0.5, 6.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
#                                 }
#                                 x.start <- adj.start - 0.5
#                                 
#                                 if (y.start > 0) {
#                                         grid.lines(x = c(x.start, x.start + 1),
#                                                    y = c(y.start - 0.5, y.start -  0.5), default.units = "native",
#                                                    gp=gpar(col = "black", lwd = 1.75))
#                                         grid.lines(x = c(x.start + 1, x.start + 1),
#                                                    y = c(y.start - 0.5 , -0.5), default.units = "native",
#                                                    gp=gpar(col = "black", lwd = 1.75))
#                                         grid.lines(x = c(x.start, x.start),
#                                                    y = c(y.start - 0.5, 6.5), default.units = "native",
#                                                    gp=gpar(col = "black", lwd = 1.75))
#                                         if (y.end < 6  ) {
#                                                 grid.lines(x = c(x.start + 1, x.finis + 1),
#                                                            y = c(-0.5, -0.5), default.units = "native",
#                                                            gp=gpar(col = "black", lwd = 1.75))
#                                                 grid.lines(x = c(x.start, x.finis),
#                                                            y = c(6.5, 6.5), default.units = "native",
#                                                            gp=gpar(col = "black", lwd = 1.75))
#                                         } else {
#                                                 grid.lines(x = c(x.start + 1, x.finis),
#                                                            y = c(-0.5, -0.5), default.units = "native",
#                                                            gp=gpar(col = "black", lwd = 1.75))
#                                                 grid.lines(x = c(x.start, x.finis),
#                                                            y = c(6.5, 6.5), default.units = "native",
#                                                            gp=gpar(col = "black", lwd = 1.75))
#                                         }
#                                 } else {
#                                         grid.lines(x = c(x.start, x.start),
#                                                    y = c( - 0.5, 6.5), default.units = "native",
#                                                    gp=gpar(col = "black", lwd = 1.75))
#                                 }
#                                 
#                                 if (y.start == 0 ) {
#                                         if (y.end < 6  ) {
#                                                 grid.lines(x = c(x.start, x.finis + 1),
#                                                            y = c(-0.5, -0.5), default.units = "native",
#                                                            gp=gpar(col = "black", lwd = 1.75))
#                                                 grid.lines(x = c(x.start, x.finis),
#                                                            y = c(6.5, 6.5), default.units = "native",
#                                                            gp=gpar(col = "black", lwd = 1.75))
#                                         } else {
#                                                 grid.lines(x = c(x.start + 1, x.finis),
#                                                            y = c(-0.5, -0.5), default.units = "native",
#                                                            gp=gpar(col = "black", lwd = 1.75))
#                                                 grid.lines(x = c(x.start, x.finis),
#                                                            y = c(6.5, 6.5), default.units = "native",
#                                                            gp=gpar(col = "black", lwd = 1.75))
#                                         }
#                                 }
#                                 for (j in 1:12)  {
#                                         last.month <- max(dates.fsubs$seq[dates.fsubs$month == j])
#                                         x.last.m <- dates.fsubs$woty[last.month] + 0.5
#                                         y.last.m <- dates.fsubs$dotw[last.month] + 0.5
#                                         grid.lines(x = c(x.last.m, x.last.m), y = c(-0.5, y.last.m),
#                                                    default.units = "native", gp=gpar(col = "black", lwd = 1.75))
#                                         if ((y.last.m) < 6) {
#                                                 grid.lines(x = c(x.last.m, x.last.m - 1), y = c(y.last.m, y.last.m),
#                                                            default.units = "native", gp=gpar(col = "black", lwd = 1.75))
#                                                 grid.lines(x = c(x.last.m - 1, x.last.m - 1), y = c(y.last.m, 6.5),
#                                                            default.units = "native", gp=gpar(col = "black", lwd = 1.75))
#                                         } else {
#                                                 grid.lines(x = c(x.last.m, x.last.m), y = c(- 0.5, 6.5),
#                                                            default.units = "native", gp=gpar(col = "black", lwd = 1.75))
#                                         }
#                                 }
#                         }
#                 }
#                 trellis.unfocus()
#         } 
#         lattice.options(default.theme = def.theme)
# }
# 
# 
# calendar1 <- calendario(dates = dates_all$Date, values = dates_all$date_count)
# 



# 4. Descriptive stats ----------------------------------------------------
# Subset DF to keep variables of interest
adults_sub <- adults_raw[, -c(1, 15, 16, 17, 18, 19, 20, 21, 22, 33,34, 35, 38, 39, 40, 42, 43, 44)]
adults_sub <- adults_sub[-which(adults_sub$Age < 18),]
# Group Age var
adults_sub$Age[adults_sub$Age >= 18 & adults_sub$Age <= 25] <- "18-25"
adults_sub$Age[adults_sub$Age > 25 & adults_sub$Age <= 35] <- "26-35"
adults_sub$Age[adults_sub$Age > 35 & adults_sub$Age <= 45] <- "36-45"
adults_sub$Age[adults_sub$Age > 45 & adults_sub$Age <= 55] <- "46-55"
adults_sub$Age[adults_sub$Age > 55] <- "55+"

# Tables. Keep NAs. Also use prop.table(table, margin = 1) * 100
ldsq_gender <- table(adults_sub$LD_diag, adults_sub$Gender, exclude = NULL)  # ldsq + gender
ldsq_age <- table(adults_sub$LD_diag, adults_sub$Age, exclude = NULL)  # ldsq + age
ldsq_ethn <- table(adults_sub$LD_diag, adults_sub$Ethnicity, exclude = NULL)  # ldsq + ethnicity
ldsq_rel <- table(adults_sub$LD_diag, adults_sub$Religion, exclude = NULL)  # ldsq + religion
ldsq_uni <- table(adults_sub$LD_diag, adults_sub$Uni..Student, exclude = NULL)  # ldsq + uni student
ldsq_phys <- table(adults_sub$LD_diag, adults_sub$Physical.disability, exclude = NULL)  # ldsq + physical disability
ldsq_learn <- table(adults_sub$LD_diag, adults_sub$Learning.disability, exclude = NULL)  # ldsq + learning disability
ldsq_DV <- table(adults_sub$LD_diag, adults_sub$DV.history, exclude = NULL)  # ldsq + DV
ldsq_context <- table(adults_sub$LD_diag, adults_sub$FME.context, exclude = NULL)  # ldsq + context
ldsq_time <- table(adults_sub$LD_diag, adults_sub$Time.since.assault, exclude = NULL)  # ldsq + time since assault
ldsq_as_1 <- table(adults_sub$LD_diag, adults_sub$Assault.type.1, exclude = NULL)  # ldsq + assault type 1
ldsq_as_2 <- table(adults_sub$LD_diag, adults_sub$Assault.type.2, exclude = NULL)  # ldsq + assault type 2
ldsq_as_3 <- table(adults_sub$LD_diag, adults_sub$Assault.type.3, exclude = NULL)  # ldsq + assault type 3
ldsq_as_4 <- table(adults_sub$LD_diag, adults_sub$Assault.type.4, exclude = NULL)  # ldsq + assault type 4
ldsq_strang <- table(adults_sub$LD_diag, adults_sub$Strangulation, exclude = NULL)  # ldsq + strangulation
ldsq_nperp <- table(adults_sub$LD_diag, adults_sub$No..of.perps., exclude = NULL)  # ldsq + number of perpetrators
ldsq_relat <- table(adults_sub$LD_diag, adults_sub$Relationship.to.alleged.perp., exclude = NULL)  # ldsq + relationship to perp

# list of tables here

# For percentages
tables_perc <- function(x) {prop.table(x, margin = 1) * 100}  # to use with lapply() over the list of tables


# Use this to print tables
grid.arrange(
        tableGrob(ldsq_ethn),
        tableGrob(ldsq_age[, c(2, 3, 4, 5, 1)]),
        ncol = 1
)

# Mosaics
mosaic(~ LD_diag + Gender, data = adults_sub, shade = TRUE, legend = FALSE)
mosaic(~ LD_diag + Age, data = adults_sub, shade = TRUE, legend = FALSE)


# 4.2. Histograms ---------------------------------------------------------

catad <- names(adults_sub)[which(sapply(adults_sub, is.character))]
adult_cat <- select(adults_sub, catad)  # select categorical variables for histograms
legend_plot <- ggplot(data=adults_sub, aes(x=factor(Gender), fill = factor(LD_diag))) + stat_count() + 
        labs(fill = "Likely to have LD") + 
        theme_fivethirtyeight() + 
        theme(axis.text.x = element_text(angle = 70, hjust = 1)) + 
        scale_fill_brewer(palette = "Set3", na.value = "grey")
legendd <- cowplot::get_legend(legend_plot)  # legend to use for combined plots in grid_plot()
ggsave(filename = "legend_ld.pdf", path = "./plots", plot = legendd, device = "pdf")  # save in same folder as plots
      

plotHist <- function(input, i) {
        data <- data.frame(x=input[[i]])
        his <- ggplot(data=data, aes(x=factor(x), fill = factor(adults_sub$LD_diag))) + stat_count() + 
                xlab(colnames(input)[i]) +
                theme_fivethirtyeight() + 
                theme(axis.text.x = element_text(angle = 70, hjust = 1), legend.position = "none") + 
                scale_fill_brewer(palette = "Set3", na.value = "grey") +
                labs(fill = "", title = names(input[i]))
                
        return (his)
}

grid_plot <- function(input, fun, ii, ncols=1, nrows = 2, name, formatt = "pdf",  width = 20, height = 20) {
        plot_list <- list()  # For legend in every one: plot_list <- list(legendd) 
        for (i in ii) {
                plot <- fun(input=input, i=i)
                plot_list <- c(plot_list, list(plot))
        }
        
        plots_temp <- do.call("grid.arrange", c(plot_list, ncol = ncols, nrow = nrows))
                                
        ggsave(filename = paste0(name, ".", formatt), path = "./plots", 
               plot = plots_temp, device = formatt,
               width = width, height = height, units = "cm")
        
}

# plotHist generates histogram of variables and fill with yes/no likely to have a learning disability
# grid_plot generates histograms for columns selected and displays them in a defined layout
# COlumns on their own: Area of residence, Ethnicity, Assault types, Relationship to perp

grid_plot(adults_sub, plotHist, ii= 1, ncol = 1, nrows = 1, name = "Referrer_hist", formatt = "pdf")  # Referrer
grid_plot(adults_sub, plotHist, ii= c(2,3), ncol = 1, nrows = 2, name = "Path_age_hist", formatt = "pdf") # Pathway and age
grid_plot(adults_sub, plotHist, ii= c(4,7), ncol = 1, nrows = 2, name = "Gend_rel_hist", formatt = "pdf")  # Gender + religion
grid_plot(adults_sub, plotHist, ii= 8:11, ncol = 2, nrows = 2, name = "Uni_dis_hist", formatt = "pdf")  # Uni + physical disability + learning disability + mental health
grid_plot(adults_sub, plotHist, ii= 12:13, ncol = 1, nrows = 2, name = "Inter_DV_hist", formatt = "pdf")  # Interperter used + DV history
grid_plot(adults_sub, plotHist, ii= c(14,15,20,21), ncol = 2, nrows = 2, name = "FME_time_strang_nperp_hist", formatt = "pdf")  # FME context + time since assault + strangulation + n of perps
grid_plot(adults_sub, plotHist, ii= 23:26, ncol = 2, nrows = 2, name = "int_harm_subst_sex_hist", formatt = "pdf")  # Met on internet + self harm + substance misuse + sex worker

grid_plot(adults_sub, plotHist, ii= 5, ncol = 1, nrows = 1, name = "Areaofresidence_hist", formatt = "pdf")
grid_plot(adults_sub, plotHist, ii= 6, ncol = 1, nrows = 1, name = "Ethnicity_hist", formatt = "pdf")
grid_plot(adults_sub, plotHist, ii= 22, ncol = 1, nrows = 1, name = "Relationship_hist", formatt = "pdf")













