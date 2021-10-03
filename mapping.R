library(tidyverse)
library(rvest)

# define the url where the data is 
url <- "https://www.worldometers.info/coronavirus/"

# read the data from the table
all <-url%>%read_html()%>%html_table()%>%.[[1]]

# extract the continents
continents <- all[1:6,]

# exclude the continents and world variables
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

# Examine the variable we are going to use
any(is.na(coronavirus$NewCases_per1M))
str(coronavirus$NewCases_per1M)
testrange <- na.omit(coronavirus$NewCases_per1M)
range(testrange) 

# Play around with how many bins looks good
coronavirus %>%
      ggplot(aes(x=NewCases_per1M))+
      geom_histogram(bins = 10, fill='#8055A4', color='white')

##########################################################
library(rgdal)

my_spdf <- readOGR("C:/Users/georg/Documents/R_Maps/TM_WORLD_BORDERS_SIMPL-0.3.shp")
     
euro_map <- my_spdf[my_spdf@data$REGION==150, ]

plot(worldmap, xlim=c(-20,60) , ylim=c(30,40))

#####################################################

# Linking my data to the map
# https://stackoverflow.com/questions/32215031/merge-data-frame-with-spatialpolygonsdataframe


library(RColorBrewer)

par(mar=c(3,4,2,2))
display.brewer.all()


col_theme <- brewer.pal(p)