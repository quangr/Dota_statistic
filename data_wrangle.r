library(jsonlite)
library(tidyverse)

data_wrangle<-function(raw){data=raw%>%
  select(c(1:8,10,11,12))

  data%>%
  mutate(radiant=player_slot<5)%>%
  mutate(win=!xor(radiant,radiant_win))%>%
  select(-1,-2,-3)%>%
  mutate(time=as.POSIXct(start_time,origin='1970-01-01',tz='Asia/Shanghai'))%>%
  mutate(year=format(time,'%Y'),month=format(time,'%m'),day=format(time,'%d'),hour=format(time,'%H'),minute=format(time,'%M'),second=format(time,'%S'),wday=weekdays(time,abbreviate=T))
}

data=data_wrangle(rbind(fromJSON("data.json"),fromJSON("data1.json"),fromJSON("data2.json")))

data=data%>%
  arrange(start_time)
