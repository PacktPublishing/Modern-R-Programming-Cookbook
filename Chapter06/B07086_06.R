# Using the pipe operator for data processing

library(magrittr)
library(dplyr)
library(ggplot2)

USAairlineData2016 <- read.csv("USAairlineData2016.csv", as.is = T)

USAairlineData2016 %>%
  select(MONTH, DEP_DELAY) %>%
  filter(DEP_DELAY>=0) %>%
  group_by(MONTH) %>%
  summarize(avgDelay=mean(DEP_DELAY, na.rm=T)) %>%
  qplot(factor(MONTH), avgDelay, data=., group=1,geom=c("line","point")) %>%
  add(xlab("Month")) %>%
  add(ylab("Mean delay (in min)")) %>%
  add(ggtitle("Mean delay in departure over months of 2016")) %>%
  add(theme_bw()) %>%
  print
  
# Efficient and fast summarization using the dplyr verbs

USAairlineData2016 <- read.csv("USAairlineData2016.csv", as.is = T)

desStat <- USAairlineData2016 %>%
  select(MONTH, ORIGIN, DEP_DELAY) %>%
  group_by(ORIGIN, MONTH) %>%
  summarize(
    MIN_DELAY = min(DEP_DELAY, na.rm=T),
    MEAN_DELAY = mean(DEP_DELAY, na.rm=T),
    MEDIAN_DELAY = median(DEP_DELAY, na.rm=T),
    MAX_DELAY = max(DEP_DELAY, na.rm=T)
  )


fourNumSum <- function(x){
  MIN_DELAY = min(x, na.rm=T)
  MEAN_DELAY = mean(x, na.rm=T)
  MEDIAN_DELAY = median(x, na.rm=T)
  MAX_DELAY = max(x, na.rm=T)
  return(data.frame(MIN_DELAY=MIN_DELAY,
                    MEAN_DELAY=MEAN_DELAY,
                    MEDIAN_DELAY=MEDIAN_DELAY,
                    MAX_DELAY=MAX_DELAY))
}

desStat2 <- with(USAairlineData2016, aggregate(DEP_DELAY, list(ORIGIN,MONTH), fourNumSum))

funDPLYR <- function(data){
  desStat <- data %>%
    select(MONTH, ORIGIN, DEP_DELAY) %>%
    group_by(ORIGIN, MONTH) %>%
    summarize(
      MIN_DELAY = min(DEP_DELAY, na.rm=T),
      MEAN_DELAY = mean(DEP_DELAY, na.rm=T),
      MEDIAN_DELAY = median(DEP_DELAY, na.rm=T),
      MAX_DELAY = max(DEP_DELAY, na.rm=T)
    )
 return(desStat)
}

funBASE <- function(data){
  fourNumSum <- function(x){
    MIN_DELAY = min(x, na.rm=T)
    MEAN_DELAY = mean(x, na.rm=T)
    MEDIAN_DELAY = median(x, na.rm=T)
    MAX_DELAY = max(x, na.rm=T)
    return(c(MIN_DELAY=MIN_DELAY,MEAN_DELAY=MEAN_DELAY,MEDIAN_DELAY=MEDIAN_DELAY,MAX_DELAY=MAX_DELAY))
  }

  desStat <- with(data, aggregate(DEP_DELAY, list(ORIGIN,MONTH), fourNumSum))
 return(desStat)
}

library(microbenchmark)
microbenchmark(funDPLYR(data = USAairlineData2016), funBASE(data = USAairlineData2016), times = 10)

# Using the customized function within the dplyr verbs

USAairlineData2016 <- read.csv("USAairlineData2016.csv", as.is= T)


# the new customized function to calculate summary statistics
fourNumSum <- function(x){
  MIN_DELAY = min(x, na.rm=T)
  MEAN_DELAY = mean(x, na.rm=T)
  MEDIAN_DELAY = median(x, na.rm=T)
  MAX_DELAY = max(x, na.rm=T)
  return(data.frame(MIN_DELAY=MIN_DELAY, MEAN_DELAY=MEAN_DELAY,
                    MEDIAN_DELAY=MEDIAN_DELAY, MAX_DELAY=MAX_DELAY))
}

desStat <- USAairlineData2016 %>%
  select(MONTH, ORIGIN, DEP_DELAY) %>%
  group_by(ORIGIN, MONTH) %>%
  do(fourNumSum(.$DEP_DELAY))

# ....the folloiwng code will produce an error
desStat <- USAairlineData2016 %>%
  select(MONTH, ORIGIN, DEP_DELAY) %>%
  group_by(ORIGIN, MONTH) %>%
  summarise(
    fourNumSum(DEP_DELAY)
  )

# Using the select verb for data processing

USAairlineData2016 <- read.csv("USAairlineData2016.csv", as.is = T)

selectExample <- USAairlineData2016 %>%
  select(QUARTER, MONTH, ORIGIN, DEST, DEP_DELAY, ARR_DELAY) %>%
  group_by(QUARTER) %>%
  do(regModel = lm(ARR_DELAY ~ DEP_DELAY, data=.)) %>%
  summarise(intercept = coef(regModel)[1], regCoef =
              coef(regModel)[2])

selectExample2 <- USAairlineData2016[c("QUARTER", "MONTH", "ORIGIN", "DEST", "DEP_DELAY", "ARR_DELAY")]

# Using the filter verb for data processing = T)
filterExample <- USAairlineData2016 %>%
  filter(DEP_DELAY>30)

filterExample2 <- filter(USAairlineData2016, DEP_DELAY>30)

filterExample3 <- USAairlineData2016[USAairlineData2016$DEP_DELAY>30,]

# Using the arrange verb for data processing
arrangeExample <- arrange(USAairlineData2016, ORIGIN, DEST)
arrangeExample2 <- arrange(USAairlineData2016, ORIGIN,desc(DEST))

sortRows <- USAairlineData2016[order(USAairlineData2016$ORIGIN, USAairlineData2016$DEST),]


# Using mutate for data processing
USAairlineData2016 <- read.csv("USAairlineData2016.csv", as.is= T)
USAairlineData2016 <- mutate(USAairlineData2016, delayIndicator = DEP_DELAY>30)

USAairlineData2016$delayIndicator <- USAairlineData2016$DEP_DELAY>30
USAairlineData2016$delayIndicator <- ifelse(USAairlineData2016$DEP_DELAY>30,1,0)

USAairlineData2016 <- mutate(USAairlineData2016,
                             diffDepArr = (ARR_DELAY - DEP_DELAY),
                             diffDepArrCat = diffDepArr>0)

# Using summarise to summarize dataset

USAairlineData2016 <- read.csv("USAairlineData2016.csv", as.is= T)

meanDelay <- USAairlineData2016 %>%
  select(ORIGIN, DEST, DEP_DELAY) %>%
  group_by(ORIGIN,DEST) %>%
  summarise(meanDelay = mean(DEP_DELAY, na.rm=T))

meanDelay2 <- USAairlineData2016 %>%
  select(ORIGIN, DEST, DEP_DELAY, ARR_DELAY) %>%
  group_by(ORIGIN,DEST) %>%
  summarise(meanDelay = mean(DEP_DELAY, na.rm=T),
            meanArrival = mean(ARR_DELAY, na.rm=T))

summaryStat <- USAairlineData2016 %>%
  select(ORIGIN, DEST, DEP_DELAY, ARR_DELAY) %>%
  group_by(ORIGIN,DEST) %>%
  summarise(meanDelay = mean(DEP_DELAY, na.rm=T),
            sdDelay = sd(DEP_DELAY, na.rm=T))

summaryStat2 <- USAairlineData2016 %>%
  select(ORIGIN, DEST, DEP_DELAY, ARR_DELAY) %>%
  group_by(ORIGIN,DEST) %>%
  summarise(meanDelay = mean(DEP_DELAY, na.rm=T),
            sdDelay = sd(DEP_DELAY, na.rm=T),
            meanArrival = mean(ARR_DELAY, na.rm=T),
            sdArrival = sd(ARR_DELAY, na.rm=T))

