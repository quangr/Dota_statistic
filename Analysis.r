source(file='data_wrangle.r')


data%>%
  mutate(weekend=(wday %in% c("周六","周日"))|(wday=="周五"&hour>16))%>%
  group_by(weekend,month,year)%>%
  summarize(totaltime=sum(duration)/3600,)%>%
  ggplot()+
  geom_bar(aes(x=month,y=totaltime,fill=weekend),stat = "sum")+
  facet_wrap(~year)
  


data%>%
  group_by(year)%>%
  summarize(totaltime=sum(duration)/3600)%>%
  ggplot()+
  geom_bar(aes(x=year,y=totaltime),stat = "identity")



sum(data$duration)/3600

ndata=data%>%
  filter(game_mode %in% c(1,22,3))

ndata%>%
  mutate(tcut=cut(start_time,breaks = 20))%>%
  group_by(tcut)%>%
  summarize(winrate=mean(win))%>%
  ggplot(aes(x=tcut,y=winrate))+
  geom_point()+
  geom_line(aes(group=1))





data%>%
  mutate(kcut=cut(kills,breaks = 6),tcut=cut(data$start_time,breaks = 4))%>%
  group_by(tcut,kcut)%>%
  summarize(winrate=mean(win))%>%
  ggplot(aes(x=kcut,y=winrate,color=tcut))+
  geom_point()+
  geom_line(aes(group=tcut))

data%>%
  mutate(dcut=cut(deaths,breaks = 6),tcut=cut(data$start_time,breaks = 4))%>%
  group_by(tcut,dcut)%>%
  summarize(winrate=mean(win))%>%
  ggplot(aes(x=dcut,y=winrate,color=tcut))+
  geom_point()+
  geom_line(aes(group=tcut))

data%>%
  group_by(game_mode)%>%
  filter(length(duration)>30)%>%
  ungroup%>%
  ggplot(aes(x=factor(game_mode),y=duration/60))+
  geom_boxplot()

data%>%
  ggplot()+
  geom_bar(aes(x=hour))

m=1
s=c()
for(i in 1:(nrow(data)-1)){
  print(i)
  if((data$start_time[i+1]-(data$duration[i]+data$start_time[i]))<3600)
  {s[i]=m}
  else{s[i]=m;m=m+1}
}
s[nrow(data)]=m
data$s=s

data%>%
  group_by(s)%>%
  summarize(totalgame=length(duration))%>%
  ggplot(aes(x=totalgame))+
  geom_histogram()

data%>%
  group_by(year,month,day)%>%
  summarize(sqelen=length(unique(s)))%>%
  ggplot(aes(x=sqelen))+
  geom_histogram()

data%>%
  group_by(year,month,day)%>%
  summarize(sqelen=length(unique(s)))%>%
  ggplot(aes(x=sqelen))+
  geom_histogram()

nndata=data%>%
  group_by(s)%>%
  mutate(is_first_game=(start_time==min(start_time)),continue=(!start_time==max(start_time)))%>%
  ungroup()%>%
  group_by(year,month,day)%>%
  mutate(have_played=!(s==min(s)))%>%
  ungroup()
  
fgd=nndata%>%
  mutate(weekend=(wday %in% c("周六","周日"))|(wday=="周五"&hour>16))%>%
  filter(is_first_game==T,game_mode%in%c(1,3,22))%>%
  mutate(kda=(kills+assists)/(max(1,deaths)))%>%
  select(1,3,10,12,13,15,21:24)
  
fgd%>%
  filter(year>=2017)%>%
  ggplot(aes(x=continue))+
  geom_bar()+
  facet_wrap(~month)

fgd%>%
  filter(year>=2017)%>%
  ggplot(aes(x=duration,y=continue))+
  geom_point()+
  facet_wrap(~lobby_type)

fgd2017=fgd%>%
  mutate(holiday=(month %in% c('02','07','08')),mealtime=(hour%in%c('11','17')),late=hour%in%c('22','23','00','01'))%>%
  filter(year>=2017)%>%
  select(-month,-year,-hour)


fgm=glm(continue ~., data = fgd2017, family = "binomial")

summary(fgm)

fgm=glm(continue ~duration+lobby_type+late, data = fgd2017, family = "binomial")

ogd=nndata%>%
  mutate(weekend=(wday %in% c("周六","周日"))|(wday=="周五"&hour>16))%>%
  filter(is_first_game==F,game_mode%in%c(1,3,22))%>%
  mutate(kda=(kills+assists)/(max(1,deaths)))%>%
  select(1,3,10,12,13,15,21:24)


ogd2017=ogd%>%
  mutate(holiday=(month %in% c('02','07','08')),mealtime=(hour%in%c('11','17')),late=hour%in%c('22','23','00','01'))%>%
  filter(year>=2017)%>%
  select(-month,-year,-hour)

ogm=glm(continue ~., data = ogd2017, family = "binomial")

summary(ogm)

ogm=glm(continue ~duration+kda, data = ogd2017, family = "binomial")


ndata%>%
  group_by(year,month,day)%>%  
  summarize(start=min(hour[!hour%in%c('00','01','02')]),sqelen=length(unique(s)))%>%
  ggplot()+
  geom_point(aes(x=start,y=sqelen))

sd=data%>%
  group_by(year,month,day)%>%  
  summarize(start=min(hour[!hour%in%c('00','01','02')]),sqelen=length(unique(s)))%>%
  ungroup()

table(sd$sqelen,sd$start)
  
sqd=data%>%
  group_by(year,month,day)%>%  
  mutate(sqelen=length(unique(s)))%>%
  ungroup()%>%
  filter(sqelen>1)%>%
  group_by(s)%>%  
  summarize(start=min(start_time),end=max(start_time)+duration[start_time==max(start_time)])%>%
  ungroup()
  
int=(sqd$start[-1]-sqd$end[-nrow(sqd)])/3600
int=int[int<8]
hist(int)

ggplot(data.frame(int=int)) + 
  geom_histogram(aes(x=int,y=..density..), position="identity",bins=30) + 
  geom_density(aes(x=int,y=..density..))


kda= ndata%>%
  mutate(kda=(kills+assists)/(max(1,deaths)))%>%
  select(kda)

duration=ndata$duration

lobby_type=ndata$lobby_type