setwd("D://Study//Diploma")

##Parsing data on nominees


#Sundance 1984-2020

#Because there is no ready dataset available, I will scrap the data from the IMDB webpage myself - I will use Docker for that, as page is using JavaScript
library(rvest)
library(xml2)
library(XML)
library(httr)
library(RSelenium)
library(dplyr)
library(stringr)

remDr <- remoteDriver$new(remoteServerAddr = "localhost",port=4445L) #setting the virtual machine

#Finding the number under which category "Best feature" is listed on the page of each festival year
remDr$open()


sundance <- data.frame(matrix(ncol = 2, nrow = 0)) #initializing the dataframe
colnames(sundance) <- c('year_ceremony','awards')

for (year in 1986:2021) #looping through the pages of each year and parsing the nominees
{ 
  url<-paste('https://www.imdb.com/event/ev0000631/',as.character(year),'/1',sep='')
  remDr$navigate(url)
  webpage<-read_html(remDr$getPageSource()[[1]])
  awards_html <- html_nodes(webpage,'.event-widgets__award+ .event-widgets__award .event-widgets__award-name , .event-widgets__award-category-name')
  awards <- html_text(awards_html)
  n<-length(awards)
  year_ceremony <- rep(year, n)
  this_film<-cbind(year_ceremony,awards)
  sundance<-rbind(sundance,this_film)
}
View(sundance)
sundance$year_ceremony<-as.integer(sundance$year_ceremony)

layout_sundance <- data.frame(matrix(ncol = 2, nrow = 0)) #Finding the number under which the cat
for  (year in 1986:2021)
{ 
  number<- min(which(sundance[sundance$year_ceremony==year,'awards']=='Dramatic'))
  line<-cbind(year,number)
  layout_sundance<-rbind(layout_sundance,line)
}
View(layout_sundance) #same layout for all years, which makes everything simpler

remDr$open()

sundance <- data.frame(matrix(ncol = 4, nrow = 0)) #initializing the dataframe
colnames(sundance) <- c('year_ceremony','film','film_id','winner')
sundance

for (year in 1986:2021) #looping through the pages of each year and parsing the nominees
  { 
  url<-paste('https://www.imdb.com/event/ev0000631/',as.character(year),'/1',sep='')
  remDr$navigate(url)
  webpage<-read_html(remDr$getPageSource()[[1]])
  css_code_nominees<-'.event-widgets__award:nth-child(1) .event-widgets__award-category:nth-child(2) .event-widgets__primary-nominees a'
  css_code_backgrounds<-'.event-widgets__award:nth-child(1) .event-widgets__award-category:nth-child(2) .event-widgets__award-nomination'
  nominees_html <- html_nodes(webpage,css_code_nominees)
  nominees <- html_text(nominees_html)
  backgrounds_html <- html_nodes(webpage,css_code_backgrounds)
  strings<-toString(backgrounds_html,width=1000000000000)
  id_locations<-str_locate_all(strings,'"event-widgets__nominee-image\"><a href="/title/tt')[[1]]
  id_locations<-id_locations[,2]-1
  ids<-substring(strings,id_locations,id_locations+8)
  n<-length(nominees)
  year_ceremony <- rep(year, n)
  winner<-rep(FALSE, n)
  winner[1]=TRUE
  this_fest<-cbind(year_ceremony=year_ceremony,film=nominees,film_id=ids,winner=winner)
  sundance<-rbind(sundance,this_fest)
  }
View(sundance)


sundance$year_ceremony<-as.integer(sundance$year_ceremony)
sundance$winner<-as.logical(sundance$winner)
head(sundance)
hist(sundance$year_ceremony,breaks=1984:2021)
sundance %>% count(year_ceremony)

write.csv(sundance,"sundance.csv", row.names = FALSE)
sundance<-read.csv2("sundance.csv",header=T,sep=',')

# Independent spirit awards 1984-2021

#Finding the number under which category "Best feature" is listed on the page of each festival year
remDr$open()

spirit <- data.frame(matrix(ncol = 2, nrow = 0)) #initializing the dataframe
colnames(spirit) <- c('year_ceremony','awards')

for (year in 1986:2021) #looping through the pages of each year and parsing the nominees
{ 
  url<-paste('https://www.imdb.com/event/ev0000349/',as.character(year),'/1',sep='')
  remDr$navigate(url)
  webpage<-read_html(remDr$getPageSource()[[1]])
  awards_html <- html_nodes(webpage,'.event-widgets__award+ .event-widgets__award .event-widgets__award-name , .event-widgets__award-category-name')
  awards <- html_text(awards_html)
  n<-length(awards)
  year_ceremony <- rep(year, n)
  this_film<-cbind(year_ceremony,awards)
  spirit<-rbind(spirit,this_film)
}
View(spirit)
spirit$year_ceremony<-as.integer(spirit$year_ceremony)

layout_spirit <- data.frame(matrix(ncol = 2, nrow = 0))
for  (year in 1986:2021)
{ 
  number<- which(spirit[spirit$year_ceremony==year,'awards']=='Best Feature')
  line<-cbind(year,number)
  layout_spirit<-rbind(layout_spirit,line)
}
layout_spirit

#With the layout type known, parsing the rest of the data

remDr$open()

spirit <- data.frame(matrix(ncol = 4, nrow = 0)) #initializing the dataframe
colnames(spirit) <- c('year_ceremony','film','film_id','winner')

for (year in 1986:2021) #looping through the pages of each year and parsing the nominees
{ 
  url<-paste('https://www.imdb.com/event/ev0000349/',as.character(year),'/1',sep='')
  remDr$navigate(url)
  webpage<-read_html(remDr$getPageSource()[[1]])
  if (layout_spirit[layout_spirit$year==year,]$number==1) {
    css_code_nominees<-'.event-widgets__award-category:nth-child(1) .event-widgets__award-category-name+ .event-widgets__award-category-nominations .event-widgets__primary-nominees a'
    css_code_directors<-'.event-widgets__award-category:nth-child(1) .event-widgets__award-category-name+ .event-widgets__award-category-nominations .event-widgets__secondary-nominees a'
    css_code_backgrounds<-'.event-widgets__award-category:nth-child(1) .event-widgets__award-category-name+ .event-widgets__award-category-nominations .event-widgets__award-nomination'
  } else if (layout_spirit[layout_spirit$year==year,]$number==3) {
    css_code_nominees<-'.event-widgets__award-category:nth-child(3) .event-widgets__primary-nominees a'
    css_code_directors<-'.event-widgets__award-category:nth-child(3) .event-widgets__secondary-nominees a'
    css_code_backgrounds<-'.event-widgets__award-category:nth-child(3) .event-widgets__award-nomination'
  } else if (layout_spirit[layout_spirit$year==year,]$number==4) {
    css_code_nominees<-'.event-widgets__award-category:nth-child(4) .event-widgets__primary-nominees a'
    css_code_directors<-'.event-widgets__award-category:nth-child(4) .event-widgets__secondary-nominees a'
    css_code_backgrounds<-'.event-widgets__award-category:nth-child(4) .event-widgets__award-nomination'
  } else {
    css_code_nominees<-'.event-widgets__award-category:nth-child(5) .event-widgets__primary-nominees a'
    css_code_directors<-'.event-widgets__award-category:nth-child(5) .event-widgets__secondary-nominees a'
    css_code_backgrounds<-'.event-widgets__award-category:nth-child(5) .event-widgets__award-nomination'
  }
  if (year==2002) {css_code_nominees<-css_code_directors} #for some reason this year has a weird layout
  nominees_html <- html_nodes(webpage,css_code_nominees)
  nominees <- html_text(nominees_html)
  backgrounds_html <- html_nodes(webpage,css_code_backgrounds)
  strings<-toString(backgrounds_html,width=1000000000000)
  id_locations<-str_locate_all(strings,'"event-widgets__nominee-image\"><a href="/title/tt')[[1]]
  str_locate_all(strings,'"event-widgets__nominee-image\"><a href="/title/tt')[[1]]
  str_locate_all(strings,'event-widgets__nominee-name\"><a href=\"/title/tt')[[1]]
  id_locations<-ifelse(year==2002,str_locate_all(strings,'event-widgets__nominee-name\"><a href=\"/title/tt'),str_locate_all(strings,'"event-widgets__nominee-image\"><a href="/title/tt'))[[1]]
  id_locations<-id_locations[,2]-1
  ids<-substring(strings,id_locations,id_locations+8)
  n<-length(nominees)
  year_ceremony <- rep(year, n)
  winner<-nominees[1]
  num_winner<-which(nominees==winner)
  winner<-rep(FALSE, n)
  winner[num_winner]<-TRUE
  this_fest<-cbind(year_ceremony=year_ceremony,film=nominees,film_id=ids,winner=winner)
  spirit<-rbind(spirit,this_fest)
}
View(spirit)


spirit$year_ceremony<-as.integer(spirit$year_ceremony)
spirit$winner<-as.logical(spirit$winner)
head(spirit)
hist(spirit$year_ceremony,breaks=1984:2021) #mostly 5 movies for a ceremony, some with 4 or 6 movies
spirit %>% count(year_ceremony)


write.csv(spirit,"spirit.csv", row.names = FALSE)
spirit<-read.csv2("spirit.csv",header=T,sep=',')


head(spirit)
head(sundance)

spirit$festival<-'spirit'
sundance$festival<-'sundance'

names(spirit)<-names(sundance)

independent_nominees<-rbind(sundance,spirit)
head(independent_nominees)


hist(independent_nominees$year_ceremony,breaks=1984:2021) 
mean((independent_nominees %>% count(year_ceremony))$n) #average of 21 movies in a year for two fests

write.csv(independent_nominees,"independent_nominees.csv", row.names = FALSE)
independent_nominees<-read.csv2("independent_nominees.csv",header=T,sep=',')


# Parsing Oscars 1986-2020

remDr <- remoteDriver$new(remoteServerAddr = "localhost",port=4445L) #setting the virtual machine

#Finding the number under which category "Best feature" is listed on the page of each festival year
remDr$open()


oscars <- data.frame(matrix(ncol = 2, nrow = 0)) #initializing the dataframe
colnames(oscars) <- c('year_ceremony','awards')

for (year in 1986:2021) #looping through the pages of each year and parsing the nominees
{ 
  url<-paste('https://www.imdb.com/event/ev0000003/',as.character(year),'/1',sep='')
  remDr$navigate(url)
  webpage<-read_html(remDr$getPageSource()[[1]])
  awards_html <- html_nodes(webpage,'.event-widgets__award+ .event-widgets__award .event-widgets__award-name , .event-widgets__award-category-name')
  awards <- html_text(awards_html)
  n<-length(awards)
  year_ceremony <- rep(year, n)
  this_film<-cbind(year_ceremony,awards)
  oscars<-rbind(oscars,this_film)
}
View(oscars)
oscars$year_ceremony<-as.integer(oscars$year_ceremony)
unique(oscars$awards)

layout_oscars <- data.frame(matrix(ncol = 2, nrow = 0)) #Finding the number under which the cat
for  (year in 1986:2021)
{ 
  number<- min(which(oscars[oscars$year_ceremony==year,'awards']=="Best Motion Picture of the Year"|oscars[oscars$year_ceremony==year,'awards']=="Best Picture"))
  line<-cbind(year,number)
  layout_oscars<-rbind(layout_oscars,line)
}
View(layout_oscars) #same layout for all years, which makes everything simpler

remDr$open()

oscars <- data.frame(matrix(ncol = 4, nrow = 0)) #initializing the dataframe
colnames(oscars) <- c('year_ceremony','film','film_id','winner')


for (year in 1986:2021) #looping through the pages of each year and parsing the nominees
{ 
  url<-paste('https://www.imdb.com/event/ev0000003/',as.character(year),'/1',sep='')
  remDr$navigate(url)
  webpage<-read_html(remDr$getPageSource()[[1]])
  if (layout_oscars[layout_oscars$year==year,]$number==1) {
    css_code_nominees<-'.event-widgets__award-category:nth-child(1) .event-widgets__award-category-name+ .event-widgets__award-category-nominations .event-widgets__primary-nominees a'
    css_code_backgrounds<-'.event-widgets__award-category:nth-child(1) .event-widgets__award-category-name+ .event-widgets__award-category-nominations .event-widgets__award-nomination'
  } else if (layout_oscars[layout_oscars$year==year,]$number==17) {
    css_code_nominees<-'.event-widgets__award-category:nth-child(17) .event-widgets__primary-nominees a'
    css_code_backgrounds<-'.event-widgets__award-category:nth-child(17) .event-widgets__award-nomination'
  } else if (layout_oscars[layout_oscars$year==year,]$number==18) {
    css_code_nominees<-'.event-widgets__award-category:nth-child(18) .event-widgets__primary-nominees a'
    css_code_backgrounds<-'.event-widgets__award-category:nth-child(18) .event-widgets__award-nomination'
  } else {
    css_code_nominees<-'.event-widgets__award-category:nth-child(19) .event-widgets__primary-nominees a'
    css_code_backgrounds<-'.event-widgets__award-category:nth-child(19) .event-widgets__award-nomination'
  }
  #if (year==2002) {css_code_nominees<-css_code_directors} #for some reason this year has a weird layout
  nominees_html <- html_nodes(webpage,css_code_nominees)
  nominees <- html_text(nominees_html)
  backgrounds_html <- html_nodes(webpage,css_code_backgrounds)
  strings<-toString(backgrounds_html,width=1000000000000)
  id_locations<-str_locate_all(strings,'"event-widgets__nominee-image\"><a href="/title/tt')[[1]]
  str_locate_all(strings,'"event-widgets__nominee-image\"><a href="/title/tt')[[1]]
  str_locate_all(strings,'event-widgets__nominee-name\"><a href=\"/title/tt')[[1]]
  id_locations<-ifelse(year==2002,str_locate_all(strings,'event-widgets__nominee-name\"><a href=\"/title/tt'),str_locate_all(strings,'"event-widgets__nominee-image\"><a href="/title/tt'))[[1]]
  id_locations<-id_locations[,2]-1
  ids<-substring(strings,id_locations,id_locations+8)
  n<-length(nominees)
  year_ceremony <- rep(year, n)
  winner<-nominees[1]
  num_winner<-which(nominees==winner)
  winner<-rep(FALSE, n)
  winner[num_winner]<-TRUE
  this_fest<-cbind(year_ceremony=year_ceremony,film=nominees,film_id=ids,winner=winner)
  oscars<-rbind(oscars,this_fest)
}
View(oscars)
#dir.create(tempdir())

oscars$year_ceremony<-as.integer(oscars$year_ceremony)
oscars$winner<-as.logical(oscars$winner)
head(oscars)
hist(oscars$year_ceremony,breaks=1984:2021)
oscars %>% count(year_ceremony)

write.csv(oscars,"oscars.csv", row.names = FALSE)
oscars<-read.csv2("oscars.csv",header=T,sep=',')

# Parsing Golden Globe nominees

remDr <- remoteDriver$new(remoteServerAddr = "localhost",port=4445L) #setting the virtual machine

#Finding the number under which category "Best feature" is listed on the page of each festival year
remDr$open()

goldenglobe <- data.frame(matrix(ncol = 2, nrow = 0)) #initializing the dataframe
colnames(goldenglobe) <- c('year_ceremony','awards')

for (year in 1986:2021) #looping through the pages of each year and parsing the nominees
{ 
  url<-paste('https://www.imdb.com/event/ev0000292/',as.character(year),'/1',sep='')
  remDr$navigate(url)
  webpage<-read_html(remDr$getPageSource()[[1]])
  awards_html <- html_nodes(webpage,'.event-widgets__award+ .event-widgets__award .event-widgets__award-name , .event-widgets__award-category-name')
  awards <- html_text(awards_html)
  n<-length(awards)
  year_ceremony <- rep(year, n)
  this_film<-cbind(year_ceremony,awards)
  goldenglobe<-rbind(goldenglobe,this_film)
}
View(goldenglobe)
goldenglobe$year_ceremony<-as.integer(goldenglobe$year_ceremony)
unique(goldenglobe$awards)

layout_goldenglobe <- data.frame(matrix(ncol = 2, nrow = 0)) #Finding the number under which the cat
for  (year in 1986:2021)
{ 
  number<- min(which(goldenglobe[goldenglobe$year_ceremony==year,'awards']=="Best Motion Picture - Drama"))
  line<-cbind(year,number)
  layout_goldenglobe<-rbind(layout_goldenglobe,line)
}
View(layout_goldenglobe)

remDr$open()

goldenglobe <- data.frame(matrix(ncol = 4, nrow = 0)) #initializing the dataframe
colnames(goldenglobe) <- c('year_ceremony','film','film_id','winner')

for (year in 1986:2021) #looping through the pages of each year and parsing the nominees
{ 
  url<-paste('https://www.imdb.com/event/ev0000292/',as.character(year),'/1',sep='')
  remDr$navigate(url)
  webpage<-read_html(remDr$getPageSource()[[1]])
  if (layout_goldenglobe[layout_goldenglobe$year==year,]$number==1) {
    css_code_nominees<-'.event-widgets__award-category:nth-child(1) .event-widgets__award-category-name+ .event-widgets__award-category-nominations .event-widgets__primary-nominees a'
    css_code_backgrounds<-'.event-widgets__award-category:nth-child(1) .event-widgets__award-category-name+ .event-widgets__award-category-nominations .event-widgets__award-nomination'
  } else {
    css_code_nominees<-'.event-widgets__award-category:nth-child(5) .event-widgets__primary-nominees a'
    css_code_backgrounds<-'.event-widgets__award-category:nth-child(5) .event-widgets__award-nomination'
  }
  #if (year==2002) {css_code_nominees<-css_code_directors} #for some reason this year has a weird layout
  nominees_html <- html_nodes(webpage,css_code_nominees)
  nominees <- html_text(nominees_html)
  backgrounds_html <- html_nodes(webpage,css_code_backgrounds)
  strings<-toString(backgrounds_html,width=1000000000000)
  id_locations<-str_locate_all(strings,'"event-widgets__nominee-image\"><a href="/title/tt')[[1]]
  str_locate_all(strings,'"event-widgets__nominee-image\"><a href="/title/tt')[[1]]
  str_locate_all(strings,'event-widgets__nominee-name\"><a href=\"/title/tt')[[1]]
  id_locations<-str_locate_all(strings,'"event-widgets__nominee-image\"><a href="/title/tt')[[1]]
  #id_locations<-ifelse(year==2002,str_locate_all(strings,'event-widgets__nominee-name\"><a href=\"/title/tt'),str_locate_all(strings,'"event-widgets__nominee-image\"><a href="/title/tt'))[[1]]
  id_locations<-id_locations[,2]-1
  ids<-substring(strings,id_locations,id_locations+8)
  n<-length(nominees)
  year_ceremony <- rep(year, n)
  winner<-nominees[1]
  num_winner<-which(nominees==winner)
  winner<-rep(FALSE, n)
  winner[num_winner]<-TRUE
  this_fest<-cbind(year_ceremony=year_ceremony,film=nominees,film_id=ids,winner=winner)
  goldenglobe<-rbind(goldenglobe,this_fest)
}
View(goldenglobe)


goldenglobe$year_ceremony<-as.integer(goldenglobe$year_ceremony)
goldenglobe$winner<-as.logical(goldenglobe$winner)
head(goldenglobe)
hist(goldenglobe$year_ceremony,breaks=1984:2021)
goldenglobe %>% count(year_ceremony)

write.csv(goldenglobe,"goldenglobe.csv", row.names = FALSE)
goldenglobe<-read.csv2("goldenglobe.csv",header=T,sep=',')



# Combining the data on Oscars and Golden Globe
goldenglobe$festival<-'goldenglobe'
oscars$festival<-'oscars'
mainstream_nominees<-rbind(oscars,goldenglobe)
hist(mainstream_nominees$year_ceremony,breaks=1984:2021) #10-15 nominees on average

#Checking the percentage of intersection between two datasets

#Combining the nominees into 5-year groups:

tail(independent_nominees)
tail(mainstream_nominees)


independent_nominees$year_group<-   case_when(independent_nominees$year_ceremony>=2016~'2016+',
                                              independent_nominees$year_ceremony>=2011~'2011+',
                                              independent_nominees$year_ceremony>=2006~'2006+',
                                              independent_nominees$year_ceremony>=2001~'2001+',
                                              independent_nominees$year_ceremony>=1996~'1996+',
                                              independent_nominees$year_ceremony>=1991~'1991+',
                                              independent_nominees$year_ceremony>=1986~'1986+')

mainstream_nominees$year_group<-   case_when(mainstream_nominees$year_ceremony>=2016~'2016+',
                                             mainstream_nominees$year_ceremony>=2011~'2011+',
                                             mainstream_nominees$year_ceremony>=2006~'2006+',
                                             mainstream_nominees$year_ceremony>=2001~'2001+',
                                             mainstream_nominees$year_ceremony>=1996~'1996+',
                                             mainstream_nominees$year_ceremony>=1991~'1991+',
                                             mainstream_nominees$year_ceremony>=1986~'1986+')

#cutting out 2021 year for similarity in chunks
mainstream_nominees<-mainstream_nominees[mainstream_nominees$year_ceremony<2021,]
independent_nominees<-independent_nominees[independent_nominees$year_ceremony<2021,]

write.csv(mainstream_nominees,"mainstream_nominees.csv", row.names = FALSE)
write.csv(independent_nominees,"independent_nominees.csv", row.names = FALSE)