---
output:
  html_document:
    fig_height: 8
    fig_width: 15
---
# Reproducible Research - Exploring & Analyzing NOAA Storm Database

- [Synopsis](#synopsis)
- [Data Processing](#data-processing)
    - [FATALITIES Processing](#fatalities-processing)
    - [INJURIES Processing](#injuries-processing)
    - [ECONOMIC Processing](#economic-processing)
- [Results](#results)

***********



## Synopsis

The basic goal of this assignment is to explore NOAA Storm Database for the severity of the weather events. We will also answer below questions:

* Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
* Across the United States, which types of events have the greatest economic consequences?

[Storm Data](http://d396qusza40orc.cloudfront.net/repDF%2FDF%2FStormDF.csv.bz2)

## Data Processing

I am assuminng data has been downloaded and extracted via above link and configured correct current working directory.  
Load neccessary packages.

```r
library(package = "plyr", warn.conflicts = FALSE, quietly = TRUE)
```

```
## -------------------------------------------------------------------------
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
## -------------------------------------------------------------------------
```

```r
library(package = "dplyr", warn.conflicts = FALSE, quietly = TRUE)
library(package = "ggplot2", warn.conflicts = FALSE, quietly = TRUE)
```
Simple read of CSV file as we need to see all the variables. I will limit to 10 rows.

```r
storm.data <- read.csv(file ="repdata_data_StormData.csv", nrow = 10)
str(storm.data)
```

```
## 'data.frame':	10 obs. of  37 variables:
##  $ STATE__   : num  1 1 1 1 1 1 1 1 1 1
##  $ BGN_DATE  : Factor w/ 7 levels "1/22/1952 0:00:00",..: 6 6 5 7 2 2 3 1 4 4
##  $ BGN_TIME  : int  130 145 1600 900 1500 2000 100 900 2000 2000
##  $ TIME_ZONE : Factor w/ 1 level "CST": 1 1 1 1 1 1 1 1 1 1
##  $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57
##  $ COUNTYNAME: Factor w/ 9 levels "BALDWIN","BLOUNT",..: 7 1 4 6 3 5 2 8 9 4
##  $ STATE     : Factor w/ 1 level "AL": 1 1 1 1 1 1 1 1 1 1
##  $ EVTYPE    : Factor w/ 1 level "TORNADO": 1 1 1 1 1 1 1 1 1 1
##  $ BGN_RANGE : num  0 0 0 0 0 0 0 0 0 0
##  $ BGN_AZI   : logi  NA NA NA NA NA NA ...
##  $ BGN_LOCATI: logi  NA NA NA NA NA NA ...
##  $ END_DATE  : logi  NA NA NA NA NA NA ...
##  $ END_TIME  : logi  NA NA NA NA NA NA ...
##  $ COUNTY_END: num  0 0 0 0 0 0 0 0 0 0
##  $ COUNTYENDN: logi  NA NA NA NA NA NA ...
##  $ END_RANGE : num  0 0 0 0 0 0 0 0 0 0
##  $ END_AZI   : logi  NA NA NA NA NA NA ...
##  $ END_LOCATI: logi  NA NA NA NA NA NA ...
##  $ LENGTH    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3
##  $ WIDTH     : num  100 150 123 100 150 177 33 33 100 100
##  $ F         : int  3 2 2 2 2 2 2 1 3 3
##  $ MAG       : num  0 0 0 0 0 0 0 0 0 0
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25
##  $ PROPDMGEXP: Factor w/ 1 level "K": 1 1 1 1 1 1 1 1 1 1
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0
##  $ CROPDMGEXP: logi  NA NA NA NA NA NA ...
##  $ WFO       : logi  NA NA NA NA NA NA ...
##  $ STATEOFFIC: logi  NA NA NA NA NA NA ...
##  $ ZONENAMES : logi  NA NA NA NA NA NA ...
##  $ LATITUDE  : num  3040 3042 3340 3458 3412 ...
##  $ LONGITUDE : num  8812 8755 8742 8626 8642 ...
##  $ LATITUDE_E: num  3051 0 0 0 0 ...
##  $ LONGITUDE_: num  8806 0 0 0 0 ...
##  $ REMARKS   : logi  NA NA NA NA NA NA ...
##  $ REFNUM    : num  1 2 3 4 5 6 7 8 9 10
```
Our raw storm data consists of <span style="color:red;">`37`</span> columns but we are interested in <span style="color:red;">`8`</span> columns i.e.

Variale name    |   Description
----------      |   ------------
STATE           |   Two letter code
EVTYPE          |   Weather event source type (tornado,avalanche,ice-storm etc.)    
FATALITIES      |   Count of weather event related fatalities
INJURIES        |   Count of weather event related injuries
PROPDMG         |   Property damage (scaled value)
PROPDMGEXP      |   Unit of measure for the PROPDMG
CROPDMG         |   Crop damage (scaled value)
CROPDMGEXP      |   Unit of measure for CROPDMG

Lets read only above columns

```r
columns.to.read <- c(rep("NULL",6), rep("character",2) , 
                        rep("NULL",14),rep("numeric",3),"character","numeric","character", rep("NULL",9))

storm.data <- read.csv(file ="repdata_data_StormData.csv", colClasses = columns.to.read , nrow = 1232705)
```
### Pre process the data for simplifications

```r
# for every state, get the total counts of each event type
new.storm <- storm.data %>% group_by(STATE,EVTYPE) %>% summarise(fatal.count = sum(FATALITIES), injur.count = sum(INJURIES), total.count = sum(FATALITIES + INJURIES))
```
Harmful events across the states w.r.t population health can be divided into two parts

1. FATALITIES
2. INJURIES

Economic consequences will use below columns

1. PROPDMG
2. PROPDMGEXP
3. CROPDMG
4. CROPDMGEXP

### <span style="color:red;">`FATALITIES`</span> Processing

```r
# get the event have max count and remove duplicates
fatal <- new.storm %>% top_n(n = 1, wt = fatal.count) %>% distinct()
# summarise the total count of each event. This will give us unqiue fatalities.
fatal.new <- fatal %>% group_by(EVTYPE) %>% summarise(total.count = sum (fatal.count))
```

```
## Error in eval(expr, envir, enclos): unknown column 'EVTYPE'
```

```r
# sort them in decreasing order
fatal.new <- fatal.new[order(fatal.new$total.count, decreasing = TRUE),]
```

```
## Error in eval(expr, envir, enclos): object 'fatal.new' not found
```

```r
fatal.new
```

```
## Error in eval(expr, envir, enclos): object 'fatal.new' not found
```
### <span style="color:red;">`FATALITIES`</span> plot

```r
g <- ggplot(data = fatal.new, aes(x = fatal.new$EVTYPE, y = fatal.new$total.count, fill=fatal.new$EVTYPE)) 
```

```
## Error in ggplot(data = fatal.new, aes(x = fatal.new$EVTYPE, y = fatal.new$total.count, : object 'fatal.new' not found
```

```r
g <- g + geom_bar(stat = "identity") + scale_fill_hue(c=45, l=80)
```

```
## Error in eval(expr, envir, enclos): object 'g' not found
```

```r
g <- g + geom_text(aes(label = fatal.new$total.count, y = fatal.new$total.count))
```

```
## Error in eval(expr, envir, enclos): object 'g' not found
```

```r
g <- g + coord_flip()
```

```
## Error in eval(expr, envir, enclos): object 'g' not found
```

```r
g <- g + scale_fill_discrete(name="Event")
```

```
## Error in eval(expr, envir, enclos): object 'g' not found
```

```r
g <- g + xlab("Event Type") +  ylab("Fatalities Count") 
```

```
## Error in eval(expr, envir, enclos): object 'g' not found
```

```r
g <- g + theme_bw()
```

```
## Error in eval(expr, envir, enclos): object 'g' not found
```

```r
g
```

```
## Error in eval(expr, envir, enclos): object 'g' not found
```

### <span style="color:red;">`INJURIES`</span> Processing


```r
# get the event have max count and remove duplicates
injur <- new.storm %>% top_n(n = 1, wt = injur.count) %>% distinct()
# summarise the total count of each event. This will give us unqiue injuries.
injur.new <- injur %>% group_by(EVTYPE) %>% summarise(total.count = sum (injur.count))
```

```
## Error in eval(expr, envir, enclos): unknown column 'EVTYPE'
```

```r
# sort them in decreasing order
injur.new <- injur.new[order(injur.new$total.count, decreasing = TRUE),]
```

```
## Error in eval(expr, envir, enclos): object 'injur.new' not found
```

```r
injur.new
```

```
## Error in eval(expr, envir, enclos): object 'injur.new' not found
```
### <span style="color:red;">`INJURIES`</span> plot

```r
g <- ggplot(data = injur.new, aes(x = injur.new$EVTYPE, y = injur.new$total.count, fill=injur.new$EVTYPE)) 
```

```
## Error in ggplot(data = injur.new, aes(x = injur.new$EVTYPE, y = injur.new$total.count, : object 'injur.new' not found
```

```r
g <- g + geom_bar(stat = "identity") + scale_fill_hue(c=45, l=80)
```

```
## Error in eval(expr, envir, enclos): object 'g' not found
```

```r
g <- g + geom_text(aes(label = injur.new$total.count, y = injur.new$total.count))
```

```
## Error in eval(expr, envir, enclos): object 'g' not found
```

```r
g <- g + coord_flip()
```

```
## Error in eval(expr, envir, enclos): object 'g' not found
```

```r
g <- g + scale_fill_discrete(name="Event")
```

```
## Error in eval(expr, envir, enclos): object 'g' not found
```

```r
g <- g + xlab("Event Type") +  ylab("Injuries Count") 
```

```
## Error in eval(expr, envir, enclos): object 'g' not found
```

```r
g <- g + theme_bw()
```

```
## Error in eval(expr, envir, enclos): object 'g' not found
```

```r
g
```

```
## Error in eval(expr, envir, enclos): object 'g' not found
```
Both plots clearly shows that the most harmful event to the human population health is <span style="color:red;">`TORNADO`</span>.

### <span style="color:red;">`ECONOMIC`</span> Processing

For each event, I will sum individual calculation of property and crop damages.

```r
eco.data <- storm.data %>% select(EVTYPE,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP)

# change the values as per below
# "","+","-","?" = 1 (for consistency purpose)
# "0","1","2","3","4","5","6","7","8" = 10 (for consistency purpose))
# H|h = 100
# K|k = 1000
# M|m = 1000000
# B|b = 1000000000

eco.data$PROPDMGEXP <- mapvalues(eco.data$PROPDMGEXP, from = c("","+","-","0","1","2","3","4","5","6","7","8","?","B","H","K","M","h","m"), to = c("1","1","1","10","10","10","10","10","10","10","10","10","1","1000000000","100","1000","1000000","100","1000000"))

eco.data$CROPDMGEXP <- mapvalues(eco.data$CROPDMGEXP, from = c("","M","K","m","B","?","0","k","2"), to = c("1","1000000","1000","1000000","1000000000","1","10","1000","10"))

eco.data <- mutate(eco.data, prop.damage = PROPDMG * as.numeric(PROPDMGEXP), crop.damage = CROPDMG * as.numeric(CROPDMGEXP), total.damage = prop.damage + crop.damage)

eco.damage <- aggregate(total.damage ~ EVTYPE, data = eco.data, FUN = sum )
eco.damage <- arrange(df = eco.damage, desc(eco.damage$total.damage))

top.20 <- eco.damage[1:20,]
top.20
```

```
##                       EVTYPE total.damage
## 1                      FLOOD 150319678257
## 2          HURRICANE/TYPHOON  71913712800
## 3                    TORNADO  57352117610
## 4                STORM SURGE  43323541000
## 5                       HAIL  18758224587
## 6                FLASH FLOOD  17562132318
## 7                    DROUGHT  15018672000
## 8                  HURRICANE  14610229010
## 9                RIVER FLOOD  10148404500
## 10                 ICE STORM   8967041810
## 11            TROPICAL STORM   8382236550
## 12              WINTER STORM   6715441260
## 13                 HIGH WIND   5908617595
## 14                  WILDFIRE   5060586800
## 15                 TSTM WIND   5038936340
## 16          STORM SURGE/TIDE   4642038000
## 17         THUNDERSTORM WIND   3897965612
## 18            HURRICANE OPAL   3191846000
## 19          WILD/FOREST FIRE   3108626330
## 20 HEAVY RAIN/SEVERE WEATHER   2500000000
```
### <span style="color:red;">`Economic`</span> plot 

```r
g <- ggplot(data = top.20, aes(x = top.20$EVTYPE, y = top.20$total.damage/1000000000, fill=top.20$EVTYPE)) 
g <- g + geom_bar(stat = "identity") + scale_fill_hue(c=45, l=80)
g <- g + coord_flip()
g <- g + scale_fill_discrete(name="Event")
```

```
## Scale for 'fill' is already present. Adding another scale for 'fill', which will replace the existing scale.
```

```r
g <- g + xlab("Event Type") +  ylab("Total Damage Billion ($)") 
g <- g + theme_bw()
g
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

### Results

1. Events <span style="color:red;">`Tornado, Excessive Heat, Heat`</span> are most harmful w.r.t population health 
2. Events <span style="color:red;">`Flood, Hurricane/Typhoon, Tornado, Storm Surge`</span> cause greatest economic consequences.

*******
