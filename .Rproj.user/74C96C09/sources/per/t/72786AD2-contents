
library(tidyverse)
library(rvest)

url <- "https://www.worldometers.info/coronavirus/"

all <-url%>%read_html()%>%html_table()%>%.[[1]]

continents <- all[1:6,]

coronavirus <- all[9:230,-1]

# Remove symbols from formatting of numbers
coronavirus[]<-lapply(coronavirus, function(x) (gsub("\\,|\\+", "", (x))))



# convert all but the first and last column to numeric

coronavirus[,c(2:14, 16:21)] <- sapply(coronavirus[c(2:14, 16:21)],as.numeric)

colnames(coronavirus) <- c("Country", "TotalCases", "NewCases", "TotalDeaths",       
                           "NewDeaths", "TotalRecovered", "NewRecovered", "ActiveCases",       
                           "CriticalCases", "Cases_Per1M", "Deaths_per1M", "TotalTests",         
                           "Tests_per1M", "Population", "Continent", "1CaseeveryXppl", 
                           "1DeatheveryXppl",  "1TesteveryXppl",   "NewCases_per1M",
                           "NewDeaths_per1M", "ActiveCases_per1M")


# deaths per million
a <- coronavirus[!is.na(coronavirus$Deaths_per1M),]

topdeathrate <- a[a$Deaths_per1M>=1500, ]


ggplot(topdeathrate, aes(x=reorder(Country, Deaths_per1M), y=Deaths_per1M, fill=Continent))+
      geom_bar(stat = "identity")+
      xlab("Country")+
      ylab("Deaths per 1,000,000")+
      theme(axis.text.x = element_text(angle = 90))+
      coord_flip()

# New Cases
b <- coronavirus[!is.na(coronavirus$NewCases_per1M),]

surge <- b[b$NewCases_per1M>=100,]    

ggplot(surge, aes(x=reorder(Country, NewCases_per1M), y=NewCases_per1M, fill=Continent))+
      geom_bar(stat = "identity")+
      xlab("Country")+
      ylab("New cases per 1,000,000")+
      theme(axis.text.x = element_text(angle = 90))+
      coord_flip()

# Islands are always kind of weird because of a variety of reasons:
# Small populations make it easier to achieve 100% testing, so cases often appear higher
# compared to populous countries where testing % is much much lower. If you have a very high 
# rate of testing, you catch all the asymptomatic people, whereas low testing rates tend to 
# prioritise symptomatic patients. Also,some asymptomatic testing in populous countries may rely
# on self-testing and self-reporting, which means some people simply won't bother.

# Low population countries
lowpop <- coronavirus[coronavirus$Population<=400000,]

