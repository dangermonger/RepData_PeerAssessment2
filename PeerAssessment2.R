setwd("C:/Users/Dangermonger/Documents/GitHub")

library(plyr)
library(dplyr) 
library(ggplot2)

##check that directory exists before creating

ifelse(!dir.exists(file.path("C:/Users/Dangermonger/Documents/GitHub", "./RepData_PeerAssessment2")), +
           dir.create(file.path("C:/Users/Dangermonger/Documents/GitHub", +
                                    "./RepData_PeerAssessment2")), FALSE)

setwd("C:/Users/Dangermonger/Documents/GitHub/Repdata_PeerAssessment2")

##check that files exists in directory before downloading. Use mode="wb" for pdf

if (!file.exists("StormData.csv.bz2")) { 
    fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2" 
    download.file(fileUrl, destfile = "./StormData.csv.bz2") ##download file
}

if (!file.exists("repdata-peer2_doc-pd01016005curr.pdf")) { 
    fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf" 
    download.file(fileUrl, destfile = "./repdata-peer2_doc-pd01016005curr.pdf", mode="wb") 
}

if (!file.exists("FAQ.pdf")) { 
    fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf" 
    download.file(fileUrl, destfile = "./FAQ.pdf", mode="wb") 
}

stormdata <- read.csv("StormData.csv.bz2") ##read file (don't need to unzip)

##Based on http://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype, all data prior to 1996 is insufficient for the purposes of this analysis. This is because prior to 1996, only 3 event types were cataloged. So, the first step is to remove all events prior to 1996.

convertbl <- tbl_df(stormdata) %>% ## convert table to tbl
    mutate(BGN_DATE = as.Date(BGN_DATE, format = "%m/%d/%Y")) %>%
    filter(BGN_DATE >= "1996-01-01") %>%
    mutate(EVTYPE = toupper(as.character(EVTYPE))) %>% ##change to capitals
    select(EVTYPE, FATALITIES, INJURIES, PROPDMGEXP, PROPDMG, +
               CROPDMGEXP, CROPDMG)

idx <- agrep(pattern = "FLOOD", convertbl$EVTYPE, max.distance = 1)
convertbl$EVTYPE[idx] <- "FLOOD"

idx <- agrep(pattern = "HAIL", convertbl$EVTYPE, max.distance = 1)
convertbl$EVTYPE[idx] <- "HAIL"

idx <- grep(pattern = "(STORM SURGE|STORM SURGE/TIDE)", convertbl$EVTYPE)
convertbl$EVTYPE[idx] <- "STORM SURGE"

idx <- grep(pattern = "(HURRICANE|TYPHOON)", convertbl$EVTYPE)
convertbl$EVTYPE[idx] <- "HURRICANE"

idx <- grep(pattern = "(HEAT|EXCESSIVE HEAT)", convertbl$EVTYPE)
convertbl$EVTYPE[idx] <- "HEAT"

harmtbl <- convertbl %>%
    select(EVTYPE, INJURIES, FATALITIES) %>%
    filter(FATALITIES & INJURIES > 0) ##remove rows where damage = 0

fatrem <-  harmtbl %>% 
    ddply("EVTYPE", numcolwise(sum)) %>%
    select(EVTYPE, FATALITIES) %>%
    arrange(desc(FATALITIES)) %>% ##order by fatalities
    top_n(5)

injrem <-  harmtbl %>% 
    ddply("EVTYPE", numcolwise(sum)) %>%
    select(EVTYPE, INJURIES) %>%
    arrange(desc(INJURIES)) %>% ##order by injuries
    top_n(5)

ggplot(data=injrem, 
       aes(x = reorder(EVTYPE, -INJURIES), y = INJURIES)) + 
    labs(title="Weather Event Injuries 1996 - 2011") +
    labs(x="Weather Event Type", y="Injuries") + 
    geom_bar(binwidth=1, fill="white", colour="darkblue", stat = "identity")


ggplot(data=fatrem, 
    aes(x = reorder(EVTYPE, -FATALITIES), y = FATALITIES)) + 
    labs(title="Weather Event Fatalities 1996 - 2011") +
    labs(x="Weather Event Type", y="Fatalities") + 
    geom_bar(binwidth=1, fill="white", colour="darkred", stat = "identity")

================================================================================

unique(convertbl$PROPDMGEXP) ##find unique cost values for property damage

##B h H K m M

unique(convertbl$CROPDMGEXP) ##find unique cost values for crop damage

##B k K m M

convertbl$PROPDMGEXP <- gsub("B", 10^9, convertbl$PROPDMGEXP)
convertbl$PROPDMGEXP <- gsub("M", 10^6, convertbl$PROPDMGEXP)
convertbl$PROPDMGEXP <- gsub("m", 10^6, convertbl$PROPDMGEXP)
convertbl$PROPDMGEXP <- gsub("K", 10^3, convertbl$PROPDMGEXP)
convertbl$PROPDMGEXP <- gsub("H", 10^2, convertbl$PROPDMGEXP)
convertbl$PROPDMGEXP <- gsub("h", 10^2, convertbl$PROPDMGEXP)

convertbl$CROPDMGEXP <- gsub("B", 10^9, convertbl$CROPDMGEXP)
convertbl$CROPDMGEXP <- gsub("m", 10^6, convertbl$CROPDMGEXP)
convertbl$CROPDMGEXP <- gsub("M", 10^6, convertbl$CROPDMGEXP)
convertbl$CROPDMGEXP <- gsub("k", 10^3, convertbl$CROPDMGEXP)
convertbl$CROPDMGEXP <- gsub("K", 10^3, convertbl$CROPDMGEXP)


multbl <- convertbl %>%
    mutate(PropDamageTotal = PROPDMG * as.numeric(PROPDMGEXP)) %>% 
    mutate(CropDamageTotal = CROPDMG * as.numeric(CROPDMGEXP)) %>% 
    mutate(DamageTotal = rowSums(.[8:9], na.rm = TRUE)) %>%
    select(EVTYPE, DamageTotal) %>%
    filter(DamageTotal > 0) ##remove rows where damage = 0

duprem <-  multbl %>% 
    ddply("EVTYPE", numcolwise(sum)) %>% ##remove duplicates
    mutate(DamageTotal = DamageTotal/10^6) %>%
    arrange(desc(DamageTotal)) %>% ##order by damage amount
    top_n(5)

ggplot(data=duprem, 
    aes(x = reorder(EVTYPE, -DamageTotal), y = DamageTotal)) + 
    labs(title="Economic Costs of Weather Events 1996 - 2011") +
    labs(x="Weather Event Type", y="Cost ($ millions)") + 
    geom_bar(binwidth=1, fill="white", colour="darkgreen", stat = "identity")

