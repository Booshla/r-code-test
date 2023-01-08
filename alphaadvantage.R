library(tidyquant)
library(alphavantager)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
setDefaults("getSymbols.av",api.key="IRW102XORY3TYSVU")
av_api_key("IRW102XORY3TYSVU")
outputsize = "full"
Sector<-av_get(av_fun = "SECTOR")
Sector<-separate(Sector,rank.group,into=c("rank","performance"),sep=":")
Sector<- Sector %>%
  mutate(percent=value/100) %>%
  mutate(sign=ifelse(value>=0,"positive","negative"))
Sector$sector[Sector$sector=='Information Technology'] <- 'Technology'
Sector$sector[Sector$sector=='Consumer Discretionary'] <- 'Discretionary'
Sector$sector[Sector$sector=='Consumer Staples'] <- 'Staples'
Sector$sector[Sector$sector=='Telecommunication Services'] <- 'Telecom'
?av_get

x<-Sector %>%
  filter(rank=="Rank D") %>%
  ggplot(aes(sector,percent,fill=sign)) +
  geom_bar(stat="identity",position="identity") +
  scale_fill_manual(values = c("positive" = "darkblue", "negative" = "red"))+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal()+
  theme(legend.position = "none",plot.caption = element_text(hjust=0,size=9))+
  labs(x="",y="",caption="@TrevirN")

today <- Sys.Date()
today %m+% months(-3)
today()
threemonths <- today()-months(3)
SPY=av_get("SPY",av_fun = "TIME_SERIES_DAILY_ADJUSTED",outputsize="full")
y<- SPY %>%
  filter(timestamp >= threemonths) %>%
  ggplot(aes(timestamp,adjusted_close)) +
  geom_line(col="darkblue")+
  geom_text(aes(x = today(), y = tail(adjusted_close, n=1), label = "SPY",col="darkblue"))+
  theme_minimal()+
  theme(legend.position = "none",
        plot.title=element_text(size=14,face="bold"),
        plot.subtitle = element_text(size=10,face="italic"))+
   labs(x="",y="",title="Sector Performance S&P 500",subtitle="3 months")
ggarrange(y,x, heights = c(2, 1),
          ncol = 1, nrow = 2, align = "v")



