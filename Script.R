# To do list:
# Generate legend for time plots
# Ages (numerical) boxplot
# LDSQ (numerical) scores boxplot
# Run a regex to check invalid strings and substitute them for NA
# change ggsaves to format = "tiff", dpi = 600, compression = "lzw"

# Create folders and download data ----------------------------------------

if (!dir.exists("./data")) dir.create("./data")
if (!dir.exists("./plots")) dir.create("./plots")
if (!dir.exists("./plots/tables")) dir.create("./plots/tables")
if (!dir.exists("./plots/time")) dir.create("./plots/time")
if (!dir.exists("./plots/maps")) dir.create("./plots/maps")

url <- "https://github.com/pabrodez/LDSQ_2017/blob/master/data/data.xlsx?raw=true"
download.file(url, destfile = "./data/data.xlsx", mode = "wb")

# load packages -----------------------------------------------------------

library(xlsx);library(tidyverse);
library(lubridate);library(data.table);library(devtools);
library(makeR);library(chron);library(lattice);
library(grid);library(gridExtra);library(ggthemes);
library(psych);library(vcd);library(cowplot);
library(reshape2);library(ggmap);library(sf); library(classInt); library(viridis)

# 1. Read data -----------------------------------------------------------
list.files("./data", pattern = ".xlsx")
adults_raw <-  read.xlsx("./data/data.xlsx", sheetIndex = 1, startRow = 2, header = TRUE, 
                         colClasses = rep("character", 50), stringsAsFactors = FALSE)
child_raw <- read.xlsx("./data/data.xlsx", sheetIndex = 2, startRow = 2, header = TRUE, 
                       colClasses = rep("character",53), stringsAsFactors = FALSE)

# lower case
adults_raw[] <- lapply(adults_raw, tolower)
child_raw[] <- lapply(child_raw, tolower)

# Substitue blank rows and non-valid with NA. Using na.string in read.xlsx throws error
adults_raw[adults_raw == ""] <- NA
adults_raw[adults_raw == "n/a"] <- NA
adults_raw[adults_raw == "na"] <- NA
adults_raw[adults_raw == "not known/not recorded"] <- NA

child_raw[child_raw == ""] <- NA
child_raw[child_raw == "n/a"] <- NA
child_raw[child_raw == "na"] <- NA
child_raw[child_raw == "not known/not recorded"] <- NA

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
adults_dates <- read.xlsx("./data/data.xlsx", sheetIndex = 1, colIndex = 3, startRow = 2, header = TRUE, colClasses = "Date")
adults_dates <- data.frame(rem_col(adults_dates))
adults_dates <- data.frame(rem_row(adults_dates))
adults_raw$Date.attended <- adults_dates[, 1]

childs_dates <- read.xlsx("./data/data.xlsx", sheetIndex = 2, colIndex = 3, startRow = 2, header = TRUE, colClasses = "Date")
childs_dates <- data.frame(rem_col(childs_dates))
childs_dates <- data.frame(rem_row(childs_dates))
child_raw$Date.attended <- childs_dates[, 1]
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
adults_raw$Gender[adults_raw$Gender == "woman"] <- "female"
adults_raw$Gender[adults_raw$Gender == "man"] <- "male"
unique(adults_raw$Area.of.residence)
adults_raw$Area.of.residence[adults_raw$Area.of.residence == "no fixed abode"] <- "no fixed above"
adults_raw$Area.of.residence[adults_raw$Area.of.residence == "gm salford"] <- "gm-salford"
adults_raw$Area.of.residence[adults_raw$Area.of.residence == "heterosexual"] <- NA
unique(adults_raw$Ethnicity)
adults_raw$Ethnicity[adults_raw$Ethnicity == "not known"] <- NA
adults_raw$Ethnicity[adults_raw$Ethnicity == "not given"] <- NA
unique(adults_raw$Religion)
unique(adults_raw$Uni..Student)
adults_raw$Uni..Student[adults_raw$Uni..Student == "unknown"] <- NA
adults_raw$Uni..Student[adults_raw$Uni..Student == "unemployed"] <- "no"
adults_raw$Uni..Student[adults_raw$Uni..Student == "f/t employed"] <- "no"
adults_raw$Uni..Student[adults_raw$Uni..Student == "p/t employed"] <- "no"
adults_raw$Uni..Student[adults_raw$Uni..Student == "university"] <- "yes"
adults_raw$Uni..Student[adults_raw$Uni..Student == "college/6th form"] <- "no"
unique(adults_raw$Physical.disability)
adults_raw$Physical.disability[adults_raw$Physical.disability == "yes, details in comments"] <- "yes"
adults_raw$Physical.disability[adults_raw$Physical.disability == "unknown"] <- NA
unique(adults_raw$Learning.disability)
adults_raw$Learning.disability[adults_raw$Learning.disability == "unknown"] <- NA
unique(adults_raw$Mental.health)
adults_raw$Mental.health[adults_raw$Mental.health == "unknown"] <- NA
unique(adults_raw$Interpreter.used)
adults_raw$Interpreter.used[adults_raw$Interpreter.used == "english speaking"] <- "no" 
adults_raw$Interpreter.used[adults_raw$Interpreter.used == "yes-needed + present"] <- "yes" 
adults_raw$Interpreter.used[adults_raw$Interpreter.used == "no-needed + not present"] <- "no" 
unique(adults_raw$DV.history)
adults_raw$DV.history[adults_raw$DV.history == "unknown"] <- NA
unique(adults_raw$If.yes..DASH.done)
unique(adults_raw$DASH.Score)
unique(adults_raw$Referred.to.MARAC)
unique(adults_raw$FME.context)
adults_raw$FME.context[adults_raw$FME.context == "yes"] <- NA
unique(adults_raw$Time.since.assault)
unique(adults_raw$Assault.type.1)
adults_raw$Assault.type.1[adults_raw$Assault.type.1 == "unknown"] <- NA
adults_raw$Assault.type.1[adults_raw$Assault.type.1 == "digital vaginal"] <- "digital penetration"
adults_raw$Assault.type.1[adults_raw$Assault.type.1 == "not disclosed"] <- NA
unique(adults_raw$Assault.type.2)
adults_raw$Assault.type.2[adults_raw$Assault.type.2 == "unknown"] <- NA
adults_raw$Assault.type.2[adults_raw$Assault.type.2 == "not disclosed"] <- NA
unique(adults_raw$Assault.type.3)
adults_raw$Assault.type.3[adults_raw$Assault.type.3 == "unknown"] <- NA
adults_raw$Assault.type.3[adults_raw$Assault.type.3 == "not disclosed"] <- NA
unique(adults_raw$Assault.type.4)
adults_raw$Assault.type.4[adults_raw$Assault.type.4 == "not disclosed"] <- NA
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
adults_raw$Self.harm[adults_raw$Self.harm == "yes - unclear"] <- "yes"
adults_raw$Self.harm[adults_raw$Self.harm == "not recent"] <- "yes"
adults_raw$Self.harm[adults_raw$Self.harm == "recent/current"] <- "yes"
unique(adults_raw$Substance.misuse)
adults_raw$Substance.misuse[adults_raw$Substance.misuse == "not recorded"] <- NA
adults_raw$Substance.misuse[adults_raw$Substance.misuse == "yes - unclear"] <- "yes"
adults_raw$Substance.misuse[adults_raw$Substance.misuse == "not recent"] <- "yes"
adults_raw$Substance.misuse[adults_raw$Substance.misuse == "recent/current"] <- "yes"
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
child_raw$Gender[child_raw$Gender == "woman"] <- "female"
child_raw$Gender[child_raw$Gender == "man"] <- "male"
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
child_raw$Interpreter.used[child_raw$Interpreter.used == "english speaking"] <- "no" 
child_raw$Interpreter.used[child_raw$Interpreter.used == "yes-needed + present"] <- "yes" 
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
child_raw$Assault.type.3[child_raw$Assault.type.3 == "unknown"] <- NA
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
child_raw$Emergency.contraception[child_raw$Emergency.contraception == "not documented"] <- NA
unique(child_raw$HIV.PEP)
child_raw$HIV.PEP[child_raw$HIV.PEP == "not documented"] <- NA
unique(child_raw$Hep.B)
child_raw$Hep.B[child_raw$Hep.B == "not documented"] <- NA
child_raw$Hep.B[child_raw$Hep.B == "not"] <- "not appropriate"
unique(child_raw$Under.16.Recording)
child_raw$Under.16.Recording[child_raw$Under.16.Recording == "dvd"] <- "yes"
unique(child_raw$STI.screening.status)
unique(child_raw$Tested.for.HIV)
unique(child_raw$Tested.for.HepB)
unique(child_raw$Tested.for.HepC)
unique(child_raw$Self.harm)
child_raw$Self.harm[child_raw$Self.harm == "recent"] <- "recent/current"
child_raw$Self.harm[child_raw$Self.harm == "not recorded"] <- NA
child_raw$Self.harm[child_raw$Self.harm == "yes - unclear"] <- "yes"
child_raw$Self.harm[child_raw$Self.harm == "recent/current"] <- "yes"
child_raw$Self.harm[child_raw$Self.harm == "not recent"] <- "yes"
unique(child_raw$Child.affected.by.DV)
child_raw$Child.affected.by.DV[child_raw$Child.affected.by.DV == "unknown"] <- NA
unique(child_raw$Substance.misuse)
child_raw$Substance.misuse[child_raw$Substance.misuse == "not recorded"] <- NA
child_raw$Substance.misuse[child_raw$Substance.misuse == "recent"] <- "recent/current"
child_raw$Substance.misuse[child_raw$Substance.misuse == "not recorded"] <- NA
child_raw$Substance.misuse[child_raw$Substance.misuse == "yes - unclear"] <- "yes"
child_raw$Substance.misuse[child_raw$Substance.misuse == "recent/current"] <- "yes"
child_raw$Substance.misuse[child_raw$Substance.misuse == "not recent"] <- "yes"
unique(child_raw$Child.safeguarding.referral)
unique(child_raw$FGM)
unique(child_raw$CSE.Risk.of.CSE)
child_raw$CSE.Risk.of.CSE[child_raw$CSE.Risk.of.CSE == "unknown"] <- NA
unique(child_raw$Client.referred.to.aftercare)
child_raw$Client.referred.to.aftercare[child_raw$Client.referred.to.aftercare == "rasa"] <- "rasasc"
unique(child_raw$Chain.of.custody.correct)
child_raw$Chain.of.custody.correct[child_raw$Chain.of.custody.correct == "not correct"] <- "no"
unique(child_raw$CAIDSQ..)

# Fix dates
# Adults
lubridate::day(adults_raw$Date.attended) %>% unique()
lubridate::month(adults_raw$Date.attended) %>% unique()
lubridate::year(adults_raw$Date.attended) %>% unique()  # fix: 3017, 2917
lubridate::year(adults_raw$Date.attended)[which(lubridate::year(adults_raw$Date.attended) %in% c(3017, 2917))] <- 2017
# Children
lubridate::day(child_raw$Date.attended) %>% unique()
lubridate::month(child_raw$Date.attended) %>% unique()
lubridate::year(child_raw$Date.attended) %>% unique() 

# this function seems to work:
check_dates <- function(dates, days = 1:31, months = 1:12, years = year(today())) {
        require(lubridate)
        for (i in 1:length(dates)) {
        if (!lubridate::day(dates[i]) %in% days) stop("Check unique(lubridate::day(dates))")
        if (!lubridate::month(dates[i]) %in% months) stop("Check unique(lubridate::month(dates))")
        if (!lubridate::year(dates[i]) %in% years) stop("Check unique(lubridate::year(dates))")
        }
        return("No trace of time travellers")
}
check_dates(adults_raw$Date.attended, months = c(6:12, 1, 2, 3), years = 2017:2018)
check_dates(child_raw$Date.attended, months = c(6:12, 1, 2, 3), years = 2017:2018)

# NAs plot
plotNa <- function(dataFrame) {
        tempDf <- as.data.frame(ifelse(is.na(dataFrame), 0, 1))
        tempDf <- tempDf[, order(colSums(tempDf))]
        tempData <- expand.grid(list(x = 1:nrow(tempDf), y = colnames(tempDf)))
        tempData$v <- as.vector(as.matrix(tempDf))
        tempData <- data.frame(x = unlist(tempData$x), y = unlist(tempData$y), v = unlist(tempData$v))
        ggplot(tempData) + geom_tile(aes(x=x, y=y, fill=factor(v))) +
                scale_fill_manual(values=c("white", "black"), name="Missing value\n1=No, 0=Yes") +
                theme_light() + ylab("") + xlab("Rows of data set") + ggtitle("")
        
}

adults_nas <- plotNa(adults_raw)
child_nas <- plotNa(child_raw)
ggsave(filename = "adults_nas.pdf", path = "./plots", plot = adults_nas, device = "pdf", units = "cm", height = 25, width = 20)
ggsave(filename = "child_nas.pdf", path = "./plots", plot = child_nas, device = "pdf", units = "cm", height = 25, width = 20)

# 3. Group variables and create new ones------------------------------------------------------
# ldsq yes/no
adults_raw <- mutate(adults_raw, LD_diag = ifelse(LDSQ.. < 46, "yes", "no"))
child_raw <- mutate(child_raw, LD_diag = ifelse(CAIDSQ.. < 46, "yes", "no"))

# relationship to perp
adults_raw$Relationship.to.alleged.perp.[adults_raw$Relationship.to.alleged.perp. == "son"] <- "1st degree relative"
adults_raw$Relationship.to.alleged.perp.[adults_raw$Relationship.to.alleged.perp. == "father"] <- "1st degree relative"
adults_raw$Relationship.to.alleged.perp.[adults_raw$Relationship.to.alleged.perp. == "brother"] <- "1st degree relative"
adults_raw$Relationship.to.alleged.perp.[adults_raw$Relationship.to.alleged.perp. == "brother-in-law"] <- "2nd degree relative"
adults_raw$Relationship.to.alleged.perp.[adults_raw$Relationship.to.alleged.perp. == "male cousin"] <- "2nd degree relative"
adults_raw$Relationship.to.alleged.perp.[adults_raw$Relationship.to.alleged.perp. == "uncle"] <- "2nd degree relative"
adults_raw$Relationship.to.alleged.perp.[adults_raw$Relationship.to.alleged.perp. == "step father"] <- "2nd degree relative"
adults_raw$Relationship.to.alleged.perp.[adults_raw$Relationship.to.alleged.perp. == "grandfather"] <- "2nd degree relative"
adults_raw$Relationship.to.alleged.perp.[adults_raw$Relationship.to.alleged.perp. == "carer"] <- "authority figure"
adults_raw$Relationship.to.alleged.perp.[adults_raw$Relationship.to.alleged.perp. == "lodger/flatmate"] <- NA
adults_raw$Relationship.to.alleged.perp.[adults_raw$Relationship.to.alleged.perp. == "family friend"] <- "friend"
adults_raw$Relationship.to.alleged.perp.[adults_raw$Relationship.to.alleged.perp. == "school peer"] <- "friend"
adults_raw$Relationship.to.alleged.perp.[adults_raw$Relationship.to.alleged.perp. == "taxi driver"] <- "stranger"
adults_raw$Relationship.to.alleged.perp.[adults_raw$Relationship.to.alleged.perp. == "partner" | adults_raw$Relationship.to.alleged.perp. == "ex partner"] <- "partner and ex"

# DV history
adults_raw$DV.history[adults_raw$DV.history == "none"] <- "no"

# self-harm
unique(adults_raw$Self.harm)
# subtance misuse
unique(adults_raw$Substance.misuse)
# assault type
adults_raw$Assault.type.1[adults_raw$Assault.type.1 == "non penetrative assault"] <- "non-penetrative assault"
adults_raw$Assault.type.1[adults_raw$Assault.type.1 == "forced to perform a sexual act"] <- "non-penetrative assault"
adults_raw$Assault.type.1[adults_raw$Assault.type.1 == "object penetration"] <- "non-penile penetration"
adults_raw$Assault.type.1[adults_raw$Assault.type.1 == "digital penetration"] <- "non-penile penetration"
adults_raw$Assault.type.1[adults_raw$Assault.type.1 == "historic abuse"] <- NA

adults_raw$Assault.type.2[adults_raw$Assault.type.2 == "non penetrative assault"] <- "non-penetrative assault"
adults_raw$Assault.type.2[adults_raw$Assault.type.2 == "forced to perform a sexual act"] <- "non-penetrative assault"
adults_raw$Assault.type.2[adults_raw$Assault.type.2 == "object penetration"] <- "non-penile penetration"
adults_raw$Assault.type.2[adults_raw$Assault.type.2 == "digital penetration"] <- "non-penile penetration"
adults_raw$Assault.type.2[adults_raw$Assault.type.2 == "historic abuse"] <- NA

adults_raw$Assault.type.3[adults_raw$Assault.type.3 == "non penetrative assault"] <- "non-penetrative assault"
adults_raw$Assault.type.3[adults_raw$Assault.type.3 == "forced to perform a sexual act"] <- "non-penetrative assault"
adults_raw$Assault.type.3[adults_raw$Assault.type.3 == "object penetration"] <- "non-penile penetration"
adults_raw$Assault.type.3[adults_raw$Assault.type.3 == "digital penetration"] <- "non-penile penetration"
adults_raw$Assault.type.3[adults_raw$Assault.type.3 == "historic abuse"] <- NA

adults_raw$Assault.type.4[adults_raw$Assault.type.4 == "non penetrative assault"] <- "non-penetrative assault"
adults_raw$Assault.type.4[adults_raw$Assault.type.4 == "forced to perform a sexual act"] <- "non-penetrative assault"
adults_raw$Assault.type.4[adults_raw$Assault.type.4 == "object penetration"] <- "non-penile penetration"
adults_raw$Assault.type.4[adults_raw$Assault.type.4 == "digital penetration"] <- "non-penile penetration"
adults_raw$Assault.type.4[adults_raw$Assault.type.4 == "historic abuse"] <- NA


# 4. Descriptive stats ----------------------------------------------------
# Subset DF to keep variables of interest
adults_sub <- adults_raw[, -c(15, 16, 17, 18, 19, 20, 21, 22, 33,34, 35, 38, 39, 40, 42, 43, 44)]
adults_sub <- adults_raw[-which(adults_raw$Age < 18),]
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

grid_plot(adults_sub, plotHist, ii= 2, ncol = 1, nrows = 1, name = "Referrer_hist", formatt = "pdf")  # Referrer
grid_plot(adults_sub, plotHist, ii= c(3,4), ncol = 1, nrows = 2, name = "Path_age_hist", formatt = "pdf") # Pathway and age
grid_plot(adults_sub, plotHist, ii= c(5,8), ncol = 1, nrows = 2, name = "Gend_rel_hist", formatt = "pdf")  # Gender + religion
grid_plot(adults_sub, plotHist, ii= 9:12, ncol = 2, nrows = 2, name = "Uni_dis_hist", formatt = "pdf")  # Uni + physical disability + learning disability + mental health
grid_plot(adults_sub, plotHist, ii= 13:14, ncol = 1, nrows = 2, name = "Inter_DV_hist", formatt = "pdf")  # Interperter used + DV history
grid_plot(adults_sub, plotHist, ii= c(15,16,21,22), ncol = 2, nrows = 2, name = "FME_time_strang_nperp_hist", formatt = "pdf")  # FME context + time since assault + strangulation + n of perps
grid_plot(adults_sub, plotHist, ii= 24:27, ncol = 2, nrows = 2, name = "int_harm_subst_sex_hist", formatt = "pdf")  # Met on internet + self harm + substance misuse + sex worker

grid_plot(adults_sub, plotHist, ii= 6, ncol = 1, nrows = 1, name = "Areaofresidence_hist", formatt = "pdf")
grid_plot(adults_sub, plotHist, ii= 7, ncol = 1, nrows = 1, name = "Ethnicity_hist", formatt = "pdf")
grid_plot(adults_sub, plotHist, ii= 23, ncol = 1, nrows = 1, name = "Relationship_hist", formatt = "pdf")
# Assault types
assault_types_hist <- adults_raw %>% select(Assault.type.1:Assault.type.4, LD_diag) %>% gather(key, value, -LD_diag) %>% select(LD_diag, value) %>% na.omit() %>% 
        ggplot(., aes(x=factor(value), fill = factor(LD_diag))) + stat_count() + 
                theme_fivethirtyeight() + 
                theme(axis.text.x = element_text(angle = 70, hjust = 1), legend.position = "top") + 
                scale_fill_brewer(palette = "Set3", na.value = "grey") +
                labs(title = "Assault types", fill = "Likely to have LD") + 
                coord_flip()
ggsave(filename = "Assault_types_hist.pdf", path = "./plots", 
               plot = assault_types_hist, device = "pdf")

# 4.3. Descriptives of population -----------------------------------
# General structure to print cross tabs:
referrer_table <- adults_sub %>% 
        count(LD_diag, Referrer) %>% 
        mutate(Percentage = round(n/sum(n) * 100, digits = 2)) %>% 
        rename(LD = LD_diag) %>% 
        select(-n) %>% spread(Referrer, Percentage) %>% select(-`<NA>`) %>% 
        tableGrob(rows = NULL)

pathway_table <- adults_sub %>% 
        count(LD_diag, Pathway) %>% 
        mutate(Percentage = round(n/sum(n) * 100, digits = 2)) %>% 
        rename(LD = LD_diag) %>% 
        select(-n) %>% spread(Pathway, Percentage) %>% select(-`<NA>`) %>% 
        tableGrob(rows = NULL)

age_table <- adults_sub %>% 
        count(LD_diag, Age) %>% 
        mutate(Percentage = round(n/sum(n) * 100, digits = 2)) %>% 
        rename(LD = LD_diag) %>% 
        select(-n) %>% spread(Age, Percentage) %>%
        tableGrob(rows = NULL)
gender_table <- adults_sub %>% 
        count(LD_diag, Gender) %>% 
        mutate(Percentage = round(n/sum(n) * 100, digits = 2)) %>% 
        rename(LD = LD_diag) %>% 
        select(-n) %>% spread(Gender, Percentage) %>%
        tableGrob(rows = NULL)

grid.arrange(textGrob("Age", gp=gpar(fontsize=15, fontface=3L)), age_table, 
             textGrob("Gender", gp=gpar(fontsize=15, fontface=3L)), gender_table, textGrob("Referrer", gp=gpar(fontsize=15, fontface=3L)), referrer_table, 
             textGrob("Pathway", gp=gpar(fontsize=15, fontface=3L)), pathway_table, ncol= 4, nrow = 2) %>% 
        ggsave(filename = "age_gender_ref_path.pdf", path = "./plots/tables", 
               plot = . , device = "pdf")

area_table <- adults_sub %>% 
        count(LD_diag, Area.of.residence) %>% 
        mutate(Percentage = round(n/sum(n) * 100, digits = 2)) %>% 
        rename(LD = LD_diag) %>% 
        select(-n) %>% spread(Area.of.residence, Percentage) %>%
        tableGrob(rows = NULL)
grid.arrange(textGrob("Area of residence", gp=gpar(fontsize=15, fontface=3L)), area_table, ncol = 1) %>% 
        ggsave(filename = "area.pdf", path = "./plots/tables", 
               plot = . , device = "pdf")

eth_table <- adults_sub %>% 
        count(LD_diag, Ethnicity) %>% 
        mutate(Percentage = round(n/sum(n) * 100, digits = 2)) %>% 
        rename(LD = LD_diag) %>% 
        select(-n) %>% spread(Ethnicity, Percentage) %>%
        tableGrob(rows = NULL)
grid.arrange(textGrob("Ethnicity", gp=gpar(fontsize=15, fontface=3L)), eth_table) %>% 
        ggsave(filename = "eth.pdf", path = "./plots/tables", 
                plot = . , device = "pdf")

rel_table <- adults_sub %>% 
        count(LD_diag, Religion) %>% 
        mutate(Percentage = round(n/sum(n) * 100, digits = 2)) %>% 
        rename(LD = LD_diag) %>% 
        select(-n) %>% spread(Religion, Percentage) %>% select(-`<NA>`) %>% 
        tableGrob(rows = NULL)

uni_table <- adults_sub %>% 
        count(LD_diag, Uni..Student) %>% 
        mutate(Percentage = round(n/sum(n) * 100, digits = 2)) %>% 
        rename(LD = LD_diag) %>% 
        select(-n) %>% spread(Uni..Student, Percentage) %>% select(-`<NA>`) %>% 
        tableGrob(rows = NULL)
grid.arrange(textGrob("Religion", gp=gpar(fontsize=15, fontface=3L)), rel_table, 
             textGrob("University student", gp=gpar(fontsize=15, fontface=3L)), uni_table, 
             ncol= 1, nrow = 4) %>% 
        ggsave(filename = "rel_uni.pdf", path = "./plots/tables", 
               plot = . , device = "pdf")

phys_table <- adults_sub %>% 
        count(LD_diag, Physical.disability) %>% 
        mutate(Percentage = round(n/sum(n) * 100, digits = 2)) %>% 
        rename(LD = LD_diag) %>% 
        select(-n) %>% spread(Physical.disability, Percentage) %>% select(-`<NA>`) %>% 
        tableGrob(rows = NULL)

ld_table <- adults_sub %>% 
        count(LD_diag, Learning.disability) %>% 
        mutate(Percentage = round(n/sum(n) * 100, digits = 2)) %>% 
        rename(LD = LD_diag) %>% 
        select(-n) %>% spread(Learning.disability, Percentage) %>% select(-`<NA>`) %>% 
        tableGrob(rows = NULL)

mental_table <- adults_sub %>% 
        count(LD_diag, Mental.health) %>% 
        mutate(Percentage = round(n/sum(n) * 100, digits = 2)) %>% 
        rename(LD = LD_diag) %>% 
        select(-n) %>% spread(Mental.health, Percentage) %>% select(-`<NA>`) %>% 
        tableGrob(rows = NULL)

grid.arrange(textGrob("Physical disability", gp=gpar(fontsize=15, fontface=3L)), phys_table, 
             textGrob("Learning disability (self-reported)", gp=gpar(fontsize=15, fontface=3L)), ld_table, 
             textGrob("Mental health", gp=gpar(fontsize=15, fontface=3L)), mental_table,
             ncol= 2, nrow = 3) %>% 
        ggsave(filename = "phys_ld_mental_.pdf", path = "./plots/tables", 
               plot = . , device = "pdf")

inter_table <- adults_sub %>% 
        count(LD_diag, Interpreter.used) %>% 
        mutate(Percentage = round(n/sum(n) * 100, digits = 2)) %>% 
        rename(LD = LD_diag) %>% 
        select(-n) %>% spread(Interpreter.used, Percentage) %>% 
        tableGrob(rows = NULL)

dv_table <- adults_sub %>% 
        count(LD_diag, DV.history) %>% 
        mutate(Percentage = round(n/sum(n) * 100, digits = 2)) %>% 
        rename(LD = LD_diag) %>% 
        select(-n) %>% spread(DV.history, Percentage) %>% 
        tableGrob(rows = NULL)
fme_table <- adults_sub %>% 
        count(LD_diag, FME.context) %>% 
        mutate(Percentage = round(n/sum(n) * 100, digits = 2)) %>% 
        rename(LD = LD_diag) %>% 
        select(-n) %>% spread(FME.context, Percentage) %>% 
        tableGrob(rows = NULL)
grid.arrange(textGrob("Interpreter used", gp=gpar(fontsize=15, fontface=3L)), inter_table, 
             textGrob("DV history", gp=gpar(fontsize=15, fontface=3L)), dv_table, 
             textGrob("FME context", gp=gpar(fontsize=15, fontface=3L)), fme_table,
             ncol= 2, nrow = 3) %>% 
        ggsave(filename = "inter_dv_fme.pdf", path = "./plots/tables", 
               plot = . , device = "pdf")

timesince_table <- adults_sub %>% 
        count(LD_diag, Time.since.assault) %>% 
        mutate(Percentage = round(n/sum(n) * 100, digits = 2)) %>% 
        rename(LD = LD_diag) %>% 
        select(-n) %>% spread(Time.since.assault, Percentage) %>% select(-`<NA>`) %>% 
        tableGrob(rows = NULL)

stran_table <- adults_sub %>% 
        count(LD_diag, Strangulation) %>% 
        mutate(Percentage = round(n/sum(n) * 100, digits = 2)) %>% 
        rename(LD = LD_diag) %>% 
        select(-n) %>% spread(Strangulation, Percentage) %>% select(-`<NA>`) %>% 
        tableGrob(rows = NULL)

nperps_table <- adults_sub %>% 
        count(LD_diag, No..of.perps.) %>% 
        mutate(Percentage = round(n/sum(n) * 100, digits = 2)) %>% 
        rename(LD = LD_diag) %>% 
        select(-n) %>% spread(No..of.perps., Percentage) %>% select(-`<NA>`) %>% 
        tableGrob(rows = NULL)

grid.arrange(textGrob("Time since assault", gp=gpar(fontsize=15, fontface=3L)), timesince_table, 
             textGrob("Strangulation", gp=gpar(fontsize=15, fontface=3L)), stran_table, 
             textGrob("N. of perpetrators", gp=gpar(fontsize=15, fontface=3L)), nperps_table,
             ncol= 2, nrow = 3) %>% 
        ggsave(filename = "time_stran_nperps.pdf", path = "./plots/tables", 
               plot = . , device = "pdf")

relat_table <- adults_sub %>% 
        count(LD_diag, Relationship.to.alleged.perp.) %>% 
        mutate(Percentage = round(n/sum(n) * 100, digits = 2)) %>% 
        rename(LD = LD_diag) %>% 
        select(-n) %>% spread(Relationship.to.alleged.perp., Percentage) %>% 
        tableGrob(rows = NULL)
grid.arrange(textGrob("Relationship to alleged perpetrator", gp=gpar(fontsize=15, fontface=3L)), relat_table,
             ncol= 1, nrow = 2) %>% 
        ggsave(filename = "relat.pdf", path = "./plots/tables", 
               plot = . , device = "pdf")

internet_table <- adults_sub %>% 
        count(LD_diag, Met.on.internet) %>% 
        mutate(Percentage = round(n/sum(n) * 100, digits = 2)) %>% 
        rename(LD = LD_diag) %>% 
        select(-n) %>% spread(Met.on.internet, Percentage) %>% select(-`<NA>`) %>% 
        tableGrob(rows = NULL)

harm_table <- adults_sub %>% 
        count(LD_diag, Self.harm) %>% 
        mutate(Percentage = round(n/sum(n) * 100, digits = 2)) %>% 
        rename(LD = LD_diag) %>% 
        select(-n) %>% spread(Self.harm, Percentage) %>% select(-`<NA>`) %>% 
        tableGrob(rows = NULL)

subst_table <- adults_sub %>% 
        count(LD_diag, Substance.misuse) %>% 
        mutate(Percentage = round(n/sum(n) * 100, digits = 2)) %>% 
        rename(LD = LD_diag) %>% 
        select(-n) %>% spread(Substance.misuse, Percentage) %>% select(-`<NA>`) %>% 
        tableGrob(rows = NULL)
grid.arrange(textGrob("Met on internet", gp=gpar(fontsize=15, fontface=3L)), internet_table, 
             textGrob("Self harm", gp=gpar(fontsize=15, fontface=3L)), harm_table, 
             textGrob("Substance misuse", gp=gpar(fontsize=15, fontface=3L)), subst_table,
             ncol= 2, nrow = 3) %>% 
        ggsave(filename = "int_harm_subst.pdf", path = "./plots/tables", 
               plot = . , device = "pdf")

ldsq_table <- adults_sub %>% 
        count(LD_diag, LDSQ..) %>% 
        mutate(Percentage = round(n/sum(n) * 100, digits = 2)) %>% 
        rename(LD = LD_diag) %>% 
        select(-n) %>% spread(LDSQ.., Percentage) %>% 
        tableGrob(rows = NULL)

grid.arrange(textGrob("LDSQ scores", gp=gpar(fontsize=15, fontface=3L)), ldsq_table, 
             ncol= 1, nrow = 2) %>% 
        ggsave(filename = "ldsq_score.pdf", path = "./plots/tables", 
               plot = . , device = "pdf")






# 4.4. Date plots -----------------------------------------------
Sys.setlocale("LC_ALL","English")

child_dates <- ymd(child_raw$Date.attended)  # children dates
adults_dates <- ymd(adults_raw$Date.attended)  # adults dates
dates_all <-  c(child_dates, adults_dates)
dates_all <- data.frame(Date = as.Date(dates_all, "%Y-%m-%d"))
# all dates

all_wdays <- dates_all %>% 
                mutate(Day_of_week = lubridate::wday(dates_all$Date, label = TRUE)) %>% 
                select(-Date) %>% group_by(Day_of_week) %>% summarize(Number = n())

# Might be better to create DF with columns: all dates along, LD diagnosed, Age
# Then just use select and filter functions
# RColorBrewer::brewer.pal(n = 6, name = "Set3") gives hex codes for the palette used for histograms. Use for visual consistency.

wdays_all <- ggplot(data=dates_all, aes(x=lubridate::wday(Date, label = TRUE))) + stat_count(fill="#80B1D3") + 
        theme_fivethirtyeight() + 
        theme(axis.text.x = element_text(hjust = 0.5), legend.position = "none") + 
        labs(x= "", y= "", fill = "", title = "Clients per day of week") 
ggsave(plot = wdays_all, filename = "daysweek_hist.pdf", path = "./plots/time", device = "pdf", height = 20, width = 15, units = "cm")

wdays_child <- ggplot(data = tibble(Date = child_dates), aes(x=lubridate::wday(Date, label = TRUE))) +
        stat_count(fill="#80B1D3") + 
        theme_fivethirtyeight() + 
        theme(axis.text.x = element_text(hjust = 0.5), legend.position = "none") + 
        labs(x= "", y= "", fill = "", subtitle = "Children", title = "Clients per day of week")
wdays_adult <- ggplot(data = tibble(Date = adults_dates), aes(x=lubridate::wday(Date, label = TRUE))) +
        stat_count(fill="#80B1D3") + 
        theme_fivethirtyeight() + 
        theme(axis.text.x = element_text(hjust = 0.5), legend.position = "none") + 
        labs(x= "", y= "", fill = "", subtitle = "Adults")
ggsave(plot = wdays_adult, filename = "wdays_adult.pdf", path = "./plots/time", device = "pdf", height = 15, width = 10, units = "cm")

ggsave(grid.arrange(wdays_child, wdays_adult, ncol = 1, nrow = 2), filename = "wdays_adult_ch.pdf", path = "./plots/time", device = "pdf", height = 18, width = 12, units = "cm")

ggplot(data = tibble(Date = adults_dates), aes(x=lubridate::wday(Date, label = TRUE))) +
        stat_count(fill="#80B1D3") + 
        theme_fivethirtyeight() + 
        theme(axis.text.x = element_text(hjust = 0.5), legend.position = "none") + 
        labs(x= "", y= "", fill = "", subtitle = "Adults")

adults_month <- tibble(Date = adults_dates) %>% group_by(Month = lubridate::month(Date, label = TRUE)) %>% summarise(Count = n())
child_month <- tibble(Date = child_dates) %>% group_by(Month = lubridate::month(Date, label = TRUE)) %>% summarise(Count = n())

months_line <- ggplot(data = adults_month, aes(x=Month, y = Count, group = 1)) +
        geom_line(colour="#80B1D3") + 
        geom_line(data = child_month, colour = "#FDB462") +
        theme_fivethirtyeight() + 
        labs(x= "", y= "")
ggsave(months_line, filename = "month_line.pdf", path = "./plots/time", device = "pdf", width = 250, height = 100, units = "mm")


days_month_all <- ggplot(data = dates_all, aes(x = lubridate::day(Date))) + stat_count(fill="#FDB462") + 
                        scale_x_continuous(breaks = c(1, 31)) +
                        theme_fivethirtyeight() +
                        labs(title = "Aggregate number clients per day of month", subtitle = "Adults and children")
ggsave(filename = "days_month_all.tiff", plot = days_month_all, path = "./plots/time", height= 15, width= 20, units="cm", dpi=600, compression = "lzw")


days_all_line <- dates_all %>% arrange(Date) %>%
        ggplot(data = ., aes(x = Date)) + geom_line(aes(y = ..count..), stat = "bin", binwidth = 1, color="#FDB462") +
                theme_fivethirtyeight() +
                scale_x_date(date_breaks = "1 month", date_labels = "%b") +
                scale_y_continuous(breaks = c(0, 2, 4, 6, 8)) +
                labs(title = "June to November clients per day", subtitle = " Adults and children")
ggsave(filename = "days_all_line.tiff", plot = days_all_line, path = "./plots/time", height= 15, width= 25, units="cm", dpi=600, compression = "lzw")

temp_adld <- adults_sub %>% select(Date.attended, LD_diag) %>% arrange(Date.attended) %>% slice(., 1:368)
temp_adld_p <- ggplot(data = temp_adld, aes(x = Date.attended)) + geom_line(aes(y = ..count..), stat = "bin", binwidth = 1, colour = "#FDB462") +
        scale_x_date(date_labels = "%b", date_breaks  ="1 month") +
        theme_fivethirtyeight() +
        labs(title = "June to November clients per day", subtitle = " Adults")

ggsave(filename = "days_adults_line.tiff", plot = temp_adld_p, path = "./plots/time", height= 15, width= 25, units="cm", dpi=600, compression = "lzw")


temp_adld_both_p <- ggplot(data = temp_adld[which(temp_adld$LD_diag == "no"), ], aes(x = Date.attended)) +
        geom_line(aes(y = ..count..), stat = "bin", binwidth = 1, colour = "#FDB462") +
        geom_line(data = temp_adld[which(temp_adld$LD_diag == "yes"), ], 
                  aes(x = Date.attended, y = ..count..), stat = "bin", binwidth = 1, colour = "#80B1D3") +
        scale_x_date(date_labels = "%b", date_breaks  ="1 month") +
        theme_fivethirtyeight() +
        labs(title = "June to November clients per day", subtitle = " Adults", caption = "Orange: Likely to have LD \nBlue: not likely")
ggsave(filename = "adults_ld_both_line.tiff", plot = temp_adld_both_p, path = "./plots/time", height= 15, width= 25, units="cm", dpi=600, compression = "lzw")

# Adults per day faceted by month
temp_month_fac <- temp_adld %>% mutate(m = lubridate::month(Date.attended, label = TRUE), d = lubridate::day(Date.attended)) %>% 
        ggplot(data = ., aes(x = d)) + stat_count(fill = "#FDB462") + 
                scale_x_continuous(breaks = c(1,31)) +
                facet_wrap(~m, ncol = 1) +
                theme_fivethirtyeight() +
                labs(title = "Clients per day by month", subtitle = " Adults \n LD yes or no")
        ggsave(filename = "adults_month_facet.tiff", plot = temp_month_fac, path = "./plots/time", height= 25, width= 15, units="cm", dpi=600, compression = "lzw")

# Adults and children faceted by month
date_all_facet <- tbl_df(dates_all) %>% arrange(desc(Date)) %>% slice(5:n()) %>% arrange(Date) %>% 
        mutate(m = lubridate::month(Date, label = TRUE), d = lubridate::day(Date)) %>% 
        
        ggplot(data = ., aes(x = d)) + stat_count(fill = "#FDB462") + 
                scale_x_continuous(breaks = c(1,31)) +
                scale_y_continuous(breaks = c(0, 2, 4, 6, 8)) +
                facet_wrap(~m, ncol = 1) +
                theme_fivethirtyeight() +
                labs(title = "Clients per day by month", subtitle = " Adults and children", caption = "Note: Jun, Sept and Nov had 30 days")

ggsave(filename = "date_all_facet.tiff", plot = date_all_facet, path = "./plots/time", height= 25, width= 15, units="cm", dpi=600, compression = "lzw")

        
# 4.5. Maps ---------------------------------------------------------------
# Methodology from https://medium.com/@traffordDataLab/lets-make-a-map-in-r-7bd1d9366098
# Download geojson of the boroughs of Greater Manchester
gb_bound <- st_read("https://opendata.arcgis.com/datasets/0b09996863af4b5db78058225bac5d1b_2.geojson", quiet = TRUE, stringsAsFactors = FALSE)
gm_bound <- gb_bound %>% filter(ctyua15nm %in% c("Bolton","Bury","Manchester","Oldham","Rochdale","Salford","Stockport","Tameside","Trafford","Wigan"))
plot(st_geometry(gm_bound))
gm_bound <- gm_bound %>% select(area_code = ctyua15cd, area_name = ctyua15nm)

# Rename areas of residence, filter to GM boroughs and add geographic code
areas_adult <- tibble(area_name = adults_sub$Area.of.residence, LD_diag = adults_sub$LD_diag)
areas_adult$area_name[which(areas_adult$area_name == "gm-stockport")] <- "Stockport"
areas_adult$area_name[which(areas_adult$area_name == "gm-salford")] <- "Salford"
areas_adult$area_name[which(areas_adult$area_name == "gm-rochdale")] <- "Rochdale"
areas_adult$area_name[which(areas_adult$area_name == "gm-bolton")] <- "Bolton"
areas_adult$area_name[which(areas_adult$area_name == "gm-manchester")] <- "Manchester"
areas_adult$area_name[which(areas_adult$area_name == "gm-oldham")] <- "Oldham"
areas_adult$area_name[which(areas_adult$area_name == "gm-trafford")] <- "Trafford"
areas_adult$area_name[which(areas_adult$area_name == "gm-bury")] <- "Bury"
areas_adult$area_name[which(areas_adult$area_name == "gm-wigan")] <- "Wigan"
areas_adult$area_name[which(areas_adult$area_name == "gm-tameside")] <- "Tameside"

areas_adult <- areas_adult %>% 
        filter(area_name %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale","Salford","Stockport","Tameside","Trafford","Wigan")) %>% 
        mutate(area_code = "code")

areas_adult$area_code[which(areas_adult$area_name == "Bolton")] <- "E08000001"              
areas_adult$area_code[which(areas_adult$area_name == "Bury")] <- "E08000002"  
areas_adult$area_code[which(areas_adult$area_name == "Manchester")] <- "E08000003"  
areas_adult$area_code[which(areas_adult$area_name == "Oldham")] <- "E08000004"  
areas_adult$area_code[which(areas_adult$area_name == "Rochdale")] <- "E08000005"  
areas_adult$area_code[which(areas_adult$area_name == "Stockport")] <- "E08000007"  
areas_adult$area_code[which(areas_adult$area_name == "Tameside")] <- "E08000008"  
areas_adult$area_code[which(areas_adult$area_name == "Trafford")] <- "E08000009"  
areas_adult$area_code[which(areas_adult$area_name == "Wigan")] <- "E08000010"  
areas_adult$area_code[which(areas_adult$area_name == "Salford")] <- "E08000006"  

# population estimates
population <- c(283115, 188669, 541263, 232724, 216165, 248726, 290557, 223189, 234673, 323060)
        
# Join gm_bound and areas_adult
sf_gm_adult <- left_join(gm_bound, areas_adult[, 3], by = "area_code")  # change to areas_adult[which(areas_adult$LD_diag == "yes"), 2:3]

# Prepare DF for map
gm_bound <- gm_bound %>% arrange(area_code)  # area_codes must be ordered in the same manner in both DFs for the left_join
sf_gm_adult <- sf_gm_adult %>% arrange(area_code) %>% group_by(area_code) %>% summarise(Count = n()) %>% mutate(Prevalence = round((Count/population)*100, digits = 3)) %>%
        mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]), 
               lat=map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
        bind_cols(gm_bound, .)

# Map of GM borough. Adults with and without LD
gm_ad_prev <- ggplot(data = na.omit(sf_gm_adult)) +
                geom_sf(aes(fill = as.factor(Prevalence)),
                        alpha = 0.8,
                        colour = "grey",                
                        size = 0.3) +
                scale_fill_brewer(labels = c(as.character(min(sf_gm_adult$Prevalence)), "", "", "","","","",as.character(max(sf_gm_adult$Prevalence))),
                                  palette = "Oranges",
                                  name = "Prevalence in %") +
                geom_text(aes(x = lon, y = lat, label = area_name), size = 3) +
                labs(x = NULL, y = NULL,                                                          
                     title = "Prevalence per borough of GM",      
                     subtitle = "Adults with and without LD",                             
                     caption = "Areas excluded: Cheshire, Merseyside") +  
                theme_fivethirtyeight() +
                theme(legend.position = "right",
                     legend.direction = "vertical",
                     panel.background = element_blank(),                                       
                     line = element_blank(),                                                     
                     axis.text = element_blank(),                                                
                     axis.title = element_blank()) +
                coord_sf(datum = NA)
ggsave(filename = "gm_ad_prev.tiff", plot = gm_ad_prev, path = "./plots/maps", height= 19, width= 22, units="cm", dpi=600, compression = "lzw")

# Map of GM borough. Adults and children
areas_all <- tibble(area_name = c(adults_sub$Area.of.residence, child_raw$Area.of.residence))  # select area from child and adult data sets

areas_all$area_name[which(areas_all$area_name == "gm-stockport")] <- "Stockport"
areas_all$area_name[which(areas_all$area_name == "gm-salford")] <- "Salford"
areas_all$area_name[which(areas_all$area_name == "gm-rochdale")] <- "Rochdale"
areas_all$area_name[which(areas_all$area_name == "gm-bolton")] <- "Bolton"
areas_all$area_name[which(areas_all$area_name == "gm-manchester")] <- "Manchester"
areas_all$area_name[which(areas_all$area_name == "gm-oldham")] <- "Oldham"
areas_all$area_name[which(areas_all$area_name == "gm-trafford")] <- "Trafford"
areas_all$area_name[which(areas_all$area_name == "gm-bury")] <- "Bury"
areas_all$area_name[which(areas_all$area_name == "gm-wigan")] <- "Wigan"
areas_all$area_name[which(areas_all$area_name == "gm-tameside")] <- "Tameside"

areas_all <- areas_all %>% 
        filter(area_name %in% c("Bolton","Bury","Manchester","Oldham", "Rochdale","Salford","Stockport","Tameside","Trafford","Wigan")) %>% 
        mutate(area_code = "code")  

areas_all$area_code[which(areas_all$area_name == "Bolton")] <- "E08000001"              
areas_all$area_code[which(areas_all$area_name == "Bury")] <- "E08000002"  
areas_all$area_code[which(areas_all$area_name == "Manchester")] <- "E08000003"  
areas_all$area_code[which(areas_all$area_name == "Oldham")] <- "E08000004"  
areas_all$area_code[which(areas_all$area_name == "Rochdale")] <- "E08000005"  
areas_all$area_code[which(areas_all$area_name == "Stockport")] <- "E08000007"  
areas_all$area_code[which(areas_all$area_name == "Tameside")] <- "E08000008"  
areas_all$area_code[which(areas_all$area_name == "Trafford")] <- "E08000009"  
areas_all$area_code[which(areas_all$area_name == "Wigan")] <- "E08000010"  
areas_all$area_code[which(areas_all$area_name == "Salford")] <- "E08000006"  # This step can be skipped, but it's desirable

sf_gm_all <- inner_join(areas_all[, 2], gm_bound, by = "area_code")  # join gm_bound and areas_adult

sf_gm_all <- sf_gm_all %>% arrange(area_code) %>% group_by(area_code) %>% summarise(Count = n()) %>% select(Count) %>%  # Prepare DF for map
        bind_cols(gm_bound, .)

gm_map_all <- ggplot() +
                geom_sf(data = sf_gm_all, aes(fill = as.factor(Count)),
                        alpha = 0.8,
                        colour = "grey",                
                        size = 0.3) +
                scale_fill_brewer(labels = c(as.character(min(sf_gm_all$Count)), "", "", "","","","","", as.character(max(sf_gm_all$Count))),
                                  palette = "Oranges",
                                  name = "N clients") +
                labs(x = NULL, y = NULL,                                                          
                     title = "Number of clients per Borough of GM",      
                     subtitle = "Adults and children",                             
                     caption = "Note: Not normalized to population") +  
                theme_fivethirtyeight() +
                theme(legend.position = "right",
                      legend.direction = "vertical",
                      panel.background = element_blank(),                                       
                      line = element_blank(),                                                     
                      axis.text = element_blank(),                                                
                      axis.title = element_blank()) +
                coord_sf(datum = NA)
ggsave(filename = "gm_map_all.tiff", plot = gm_map_all, path = "./plots/maps", height= 17, width= 20, units="cm", dpi=600, compression = "lzw")


      


