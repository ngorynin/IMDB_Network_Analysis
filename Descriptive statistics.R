setwd("D://Study//Diploma")

## Analysis on the ready data


##Merging the nominees with imdb data
library(ggplot2)
library(data.table)
library(dplyr)
library(gridExtra)
library(igraph)
library(stringr)
library(readxl)
library(haven)
library(DT)
library(sjPlot)

professions<-c('composer','director','writer','cinematographer','producer','editor','production_designer') #leaving only info on professions from the list

df_crew<-fread(file = 'data_principals.tsv',header = TRUE) #information on all the crews
df_titles<-fread(file = 'data_title_basics.tsv',header = TRUE) #reading the file

mainstream_nominees<-read.csv2("mainstream_nominees.csv",header=T,sep=',')
independent_nominees<-read.csv2("independent_nominees.csv",header=T,sep=',')

df_titles$startYear<-as.numeric(df_titles$startYear)
df_titles<-df_titles[(df_titles$startYear>=1986)&(df_titles$startYear<=2020),] #filtering to get only timeperiod needed
df_titles<-df_titles[df_titles$titleType=='movie',] #leaving only movies

dim(df_titles)
dim(df_crew)

df_crew<-df_crew[(df_crew$tconst%in%df_titles$tconst)&(df_crew$category%in%professions),]


# Checking the countries
df_country<-read_excel("country_via_id.xlsx",col_names=c('tconst','country'))
df_country<-as.data.frame(df_country)
length(unique(df_country$tconst))

length(df_titles$tconst)
length(unique(df_titles$tconst))
df_titles<-merge(df_titles, df_country, by.x='tconst', by.y='tconst',all.x=TRUE,sort = TRUE)
head(df_titles)

# Dataframe with properties of each movie
df_vertice_prop<-df_titles[,c(1,3,5,6,8,9,10)]
df_vertice_prop$isAdult<-as.logical(df_vertice_prop$isAdult)
df_vertice_prop$startYear<-as.integer(df_vertice_prop$startYear)
df_vertice_prop$runtimeMinutes<-as.integer(df_vertice_prop$runtimeMinutes)
df_vertice_prop[df_vertice_prop$genres=='\\N',]$genres<-NA
df_vertice_prop[df_vertice_prop$country=='<NA>',]$country<-NA
df_vertice_prop$nomination<-case_when((df_vertice_prop$tconst%in%mainstream_nominees$film_id)& (df_vertice_prop$tconst%in%independent_nominees$film_id)~'both',
                                      (df_vertice_prop$tconst%in%mainstream_nominees$film_id)&(!df_vertice_prop$tconst%in%independent_nominees$film_id)~'mainstream',
                                     (!df_vertice_prop$tconst%in%mainstream_nominees$film_id)& (df_vertice_prop$tconst%in%independent_nominees$film_id)~'independent',
                                     (!df_vertice_prop$tconst%in%mainstream_nominees$film_id)&(!df_vertice_prop$tconst%in%independent_nominees$film_id)~'none')


df_vertice_prop<-df_vertice_prop[df_vertice_prop$tconst%in%df_crew$tconst,]
dim(df_vertice_prop)





rm(df_titles)



round(addmargins(prop.table(table(droplevels(as_factor(df_vertice_prop$nomination)), useNA = "ifany"))),3)*100 # percentages rounded up
head(df_vertice_prop)
head(df_crew)

length(unique(df_crew$tconst))==length(unique(df_vertice_prop$tconst)) #checking if the number of movies is same in relevant datasets

## Checking the country distribution

df_countries<-df_vertice_prop[,c(4,7,8)]
df_countries<-df_countries %>% group_by(startYear,country,nomination) %>% summarise(n = n())
df_countries<-as.data.frame(df_countries)

topcountries<-as.data.frame(df_countries %>% group_by(country) %>% summarise(n = sum(n)) %>% arrange(-n))
topcountries<-topcountries[,1] #country names
topcountries<-topcountries[0:5] #first 5 countries

`%!in%` <- Negate(`%in%`) #negate operator
df_countries1<-df_countries                                                                                                                                                                                                                                              
df_countries1$country<-case_when(df_countries1$country%!in%topcountries~'Other',
                                 TRUE ~ df_countries1$country)
df_countries1$country <- factor(df_countries1$country,levels=append(topcountries,'Other'))
View(df_countries1) 

as.data.frame(df_countries1 %>% group_by(country,startYear) %>% summarise(n = sum(n)) %>% arrange(-n))
df_countries %>% group_by(country) %>% summarise(n = sum(n)) %>% arrange(-n)

par(mfrow = c(1, 1))
ggplot(df_countries1, aes(fill=country,x=startYear,y=n,label = n)) + 
  geom_bar(width=0.75, stat="identity")+#,position="fill")+
  geom_col(position = position_stack(reverse = TRUE))+
  theme_bw()+
  #geom_text(stat="identity", size =4, vjust=-0.5)+
  ggtitle('Total movies produced by country')+
  xlab('Year')+ylab('Number of movies')

ggplot(df_countries1[df_countries1$nomination=='mainstream',], aes(fill=country,x=startYear,y=n,label = n)) + 
  geom_bar(width=0.75, stat="identity")+#,position="fill")+
  geom_col(position = position_stack(reverse = TRUE))+
  theme_bw()+
  #geom_text(stat="identity", size =4, vjust=-0.5)+
  ggtitle('Mainstream nominees by country')+
  xlab('Year')+ylab('Number of movies')

ggplot(df_countries1[df_countries1$nomination=='independent',], aes(fill=country,x=startYear,y=n,label = n)) + 
  geom_bar(width=0.75, stat="identity")+#,position="fill")+
  geom_col(position = position_stack(reverse = TRUE))+
  theme_bw()+
  #geom_text(stat="identity", size =4, vjust=-0.5)+
  ggtitle('Independent nominees by country')+
  xlab('Year')+ylab('Number of movies')


## Statistics on nominees

dim(mainstream_nominees) #402 nominee initially
dim(independent_nominees) #740 nominees initially

all_nominees<-rbind(mainstream_nominees,independent_nominees)
all_nominees<-all_nominees[,-6]


dim(all_nominees) #1142 nominee total
df_vertice_prop
all_nominees<-merge(all_nominees,df_vertice_prop[,c(1,4,6,7,8)],by.x='film_id',by.y='tconst',all.x=T)
all_nominees<-all_nominees[is.na(all_nominees$country)!=T,]
all_nominees<-all_nominees[is.na(all_nominees$startYear)!=T,] #1105 nominees produced within 1986-2020 

summary(all_nominees)
dim(all_nominees[all_nominees$country=='United States',]) #919 nominees out of United states

length()



df_cm<-all_nominees[(all_nominees$festival=='oscars')|(all_nominees$festival=='goldenglobe'),] %>% group_by(country) %>% summarize(total = n()) %>% arrange(-total)
df_ci<-all_nominees[(all_nominees$festival=='sundance')|(all_nominees$festival=='spirit'),] %>% group_by(country) %>% summarize(total = n()) %>% arrange(-total)

names(df_cm)<-names(df_ci)<-c('Country','Total nominees')
list(df_cm,df_ci)

tab_dfs(list(df_cm,df_ci),title = c("Mainstream nominees by country","Independent nominees by country"),file = "nominees_country.doc",use.viewer=TRUE)



length(unique(all_nominees$film_id)) # 907 unique titles for all countries
length(unique(all_nominees[all_nominees$country=='United States',]$film_id)) # 907 unique titles for all countries


dim(all_nominees[(all_nominees$festival=='oscars')|(all_nominees$festival=='goldenglobe'),])/35 #11 nominees on mainstream festivals annually
dim(all_nominees[(all_nominees$festival=='sundance')|(all_nominees$festival=='spirit'),])/35 #20 nominees on independent festivals annually


head(all_nominees)
ggplot(all_nominees[all_nominees$country=='United States',], aes(startYear, fill=festival)) + 
  geom_bar(width=0.75,color='black',position = position_dodge(1))+
  theme_bw()+
  geom_text(aes(label=after_stat(count)),
            stat='count', size =3, vjust=-0.5,position = position_dodge(0.8),)+
  ggtitle('Total number of festival nominees over years')+
  xlab('Year')+ylab('Number of nominees')+
  guides(fill=guide_legend(title="Type"))


all_nominees
print_nominees<-all_nominees[all_nominees$country=='United States',c(1,6,2,3,7,5,4,10)] #list with all nominees to print
names(print_nominees)<-c('Id','Year_production',"Year_award",'Title','Genre','Award','Winner','Type')
print_nominees<-print_nominees %>% arrange(Year_production)
print_nominees<-print_nominees[,c(4,2,3,1,5,6,7)]

print_nominees$Award <- recode_factor(print_nominees$Award, congruent = "Con", 
                                incongruent = "InCon")

print_nominees$Award<-as.factor(print_nominees$Award)
levels(print_nominees$Award)
levels(print_nominees$Award) <- c("Golden Globes", "Oscars", "Independent Spirit","Sundance")
head(print_nominees)

tab_df(print_nominees[,-1],title = "All nominees under study",file = "all_nominees.doc",use.viewer=TRUE)



all_nominees$type<-ifelse(all_nominees$festival=='oscars'|all_nominees$festival=='goldenglobe','mainstream','independent')
head(all_nominees)

df_nominees<-df_vertice_prop
df_nominees$Nomination<-df_nominees


length(df_vertice_prop[df_vertice_prop$Nomination!='none',]$Id)

length(all_nominees$film_id)
length(unique(all_nominees$film_id))
length(all_nominees[all_nominees$type=='mainstream',]$film_id)/35
length(all_nominees[all_nominees$type=='independent',]$film_id)/35

print_nominees<-df_vertice_prop[df_vertice_prop$Nomination!='none',c(4,1,2,6,7)] %>% arrange(Year)

length(print_nominees$Id)
length(unique(df_vertice_prop$Id))

tab_df(print_nominees,title = "All nominees analyzed",file = "titles_sample.doc",use.viewer=TRUE)

tab_df(head(df_vertice_prop,20),title = "Sample of titles analyzed",file = "titles_sample.doc",use.viewer=TRUE)
tab_df(df_usa,title = "Number of movies produced by year",file = "movies_by_year.doc",use.viewer=TRUE)

head(df_vertice_prop,10)


##Comparing the amount of intersections between categories

head(df_vertice_prop)
head(df_crew)

dim(df_vertice_prop)
dim(df_crew)

summary(df_vertice_prop$startYear)

head(df_vertice_prop)
df_vertice_prop<-df_vertice_prop[df_vertice_prop$country=='United States',] #Leaving only US movies
df_vertice_prop<-df_vertice_prop[,-7] #removing the country column
names(df_vertice_prop)<-c('Id','Title','Adult','Year','Runtime','Genre','Nomination') #renaming the columns


dim(df_vertice_prop)
window<-5 #setting the window size

intersection_full <- data.frame(matrix(ncol = 5, nrow = 0)) #initializing the dataframe
colnames(intersection_full) <- c('year','n_independent','n_mainstream','n_intersect','percentage')
for (year in (1986+window-1):2020) {
  sundance<-unique(independent_nominees[(independent_nominees$year_ceremony<=year)&(independent_nominees$year_ceremony>(year-window))&(independent_nominees$festival=='sundance'),]$film_id) #all the sundance movies within the window
  spirit<-unique(independent_nominees[(independent_nominees$year_ceremony<=year)&(independent_nominees$year_ceremony>(year-window))&(independent_nominees$festival=='spirit'),]$film_id) #all the spirit movies within the window
  oscars<-unique(mainstream_nominees[(mainstream_nominees$year_ceremony<=year)&(mainstream_nominees$year_ceremony>(year-window))&(mainstream_nominees$festival=='oscars'),]$film_id) #all the oscars movies within the window
  goldenglobe<-unique(mainstream_nominees[(mainstream_nominees$year_ceremony<=year)&(mainstream_nominees$year_ceremony>(year-window))&(mainstream_nominees$festival=='goldenglobe'),]$film_id) #all the oscars movies within the window
  independent<-unique(append(sundance,spirit)) #all the independent movies within the window
  mainstream<-unique(append(oscars,goldenglobe)) #all the mainstream movies within the window
  intersect<-intersect(independent,mainstream) #all the common movies
  intersect_independent<-intersect(sundance,spirit) #all the common movies between independent festivals
  intersect_mainstream<-intersect(oscars,goldenglobe) #all the common movies between mainstream festivals
  n_independent<-length(independent) #total number of independent movies
  n_sundance<-length(sundance) #total number of sundance nominees
  n_spirit<-length(spirit) #total number of spirit nominees
  n_mainstream<-length(mainstream) #total number of mainstream movies
  n_oscars<-length(oscars) #total number of oscar nominees
  n_goldenglobe<-length(goldenglobe) #total number of goldenglobe nominees
  
  n_intersect<-length(intersect) #number of common movies between the two categories
  n_intersect_independent<-length(intersect_independent) #number of common movies between independent festivals
  n_intersect_mainstream<-length(intersect_mainstream) #number of common movies between mainstream festivals
  
  percentage<-n_intersect/min(n_independent,n_mainstream)
  percentage_independent<-n_intersect_independent/min(n_sundance,n_spirit)
  percentage_mainstream<-n_intersect_mainstream/min(n_oscars,n_goldenglobe)
  this_year<-cbind(year,n_independent,n_mainstream,n_intersect,percentage,percentage_independent,percentage_mainstream)
  intersection_full<-rbind(intersection_full,this_year)
}
intersection_full$percentage<-as.numeric(intersection_full$percentage)

# Same thing for individual years

intersection_full2 <- data.frame(matrix(ncol = 5, nrow = 0)) #initializing the dataframe
colnames(intersection_full2) <- c('year','n_independent','n_mainstream','n_intersect','percentage')
for (year in 1986:2020) {
  sundance<-unique(independent_nominees[(independent_nominees$year_ceremony==year)&(independent_nominees$festival=='sundance'),]$film_id) #all the sundance movies for the year
  spirit<-unique(independent_nominees[(independent_nominees$year_ceremony==year)&(independent_nominees$festival=='spirit'),]$film_id) #all the spirit movies for the year
  oscars<-unique(mainstream_nominees[(mainstream_nominees$year_ceremony==year)&(mainstream_nominees$festival=='oscars'),]$film_id) #all the oscars movies for the year
  goldenglobe<-unique(mainstream_nominees[(mainstream_nominees$year_ceremony==year)&(mainstream_nominees$festival=='goldenglobe'),]$film_id) #all the oscars movies for the year
  independent<-unique(append(sundance,spirit)) #all the independent movies within the window
  mainstream<-unique(append(oscars,goldenglobe)) #all the mainstream movies within the window
  intersect<-intersect(independent,mainstream) #all the common movies
  intersect_independent<-intersect(sundance,spirit) #all the common movies between independent festivals
  intersect_mainstream<-intersect(oscars,goldenglobe) #all the common movies between mainstream festivals
  n_independent<-length(independent) #total number of independent movies
  n_sundance<-length(sundance) #total number of sundance nominees
  n_spirit<-length(spirit) #total number of spirit nominees
  n_mainstream<-length(mainstream) #total number of mainstream movies
  n_oscars<-length(oscars) #total number of oscar nominees
  n_goldenglobe<-length(goldenglobe) #total number of goldenglobe nominees
  
  n_intersect<-length(intersect) #number of common movies between the two categories
  n_intersect_independent<-length(intersect_independent) #number of common movies between independent festivals
  n_intersect_mainstream<-length(intersect_mainstream) #number of common movies between mainstream festivals
  
  percentage<-n_intersect/min(n_independent,n_mainstream)
  percentage_independent<-n_intersect_independent/min(n_sundance,n_spirit)
  percentage_mainstream<-n_intersect_mainstream/min(n_oscars,n_goldenglobe)
  this_year<-cbind(year,n_independent,n_mainstream,n_intersect,percentage,percentage_independent,percentage_mainstream)
  intersection_full2<-rbind(intersection_full2,this_year)
}
intersection_full2$percentage<-as.numeric(intersection_full2$percentage)

mean(intersection_full2$percentage)

mean(intersection_full$percentage_independent)
mean(intersection_full2$percentage_mainstream)

head(intersection_full)
head(intersection_full2)


p1<-ggplot(intersection_full2, aes(x=year, y=percentage)) + 
  ylim(0, 1)+
  geom_bar(stat = "identity", width=0.75,color='black',fill='cornflowerblue')+
  theme_bw()+
  geom_text(aes(label=paste(100*round(percentage,2),'%',sep = "")), size =3.5, vjust=-0.5)+
  ggtitle('Percentage of intersection between mainstream and independent festival nominees over years')+
  xlab('Year')+ylab('Percentage')

p2<-ggplot(intersection_full, aes(x=year, y=percentage)) + 
  ylim(0, 1)+
  geom_bar(stat = "identity", width=0.75,color='black',fill='cornflowerblue')+
  theme_bw()+
  geom_text(aes(label=paste(100*round(percentage,2),'%',sep = "")), size =3.5, vjust=-0.5)+
  ggtitle('Percentage of intersection between mainstream and independent festival nominees within 5-year window')+
  xlab('Year')+ylab('Percentage')

grid.arrange(p1,p2)

p3<-ggplot(intersection_full2, aes(x=year, y=percentage_mainstream)) + 
  ylim(0, 1)+
  geom_bar(stat = "identity", width=0.75,color='black',fill='cornflowerblue')+
  theme_bw()+
  geom_text(aes(label=paste(100*round(percentage_mainstream,2),'%',sep = "")), size =3.5, vjust=-0.5)+
  ggtitle('Percentage of intersection within mainstream festival nominees over years')+
  xlab('Year')+ylab('Percentage')

p4<-ggplot(intersection_full, aes(x=year, y=percentage_mainstream)) + 
  ylim(0, 1)+
  geom_bar(stat = "identity", width=0.75,color='black',fill='cornflowerblue')+
  theme_bw()+
  geom_text(aes(label=paste(100*round(percentage_mainstream,2),'%',sep = "")), size =3.5, vjust=-0.5)+
  ggtitle('Percentage of intersection within mainstream festival nominees within 5-year window')+
  xlab('Year')+ylab('Percentage')

grid.arrange(p3,p4)

p5<-ggplot(intersection_full2, aes(x=year, y=percentage_independent)) + 
  ylim(0, 1)+
  geom_bar(stat = "identity", width=0.75,color='black',fill='cornflowerblue')+
  theme_bw()+
  geom_text(aes(label=paste(100*round(percentage_independent,2),'%',sep = "")), size =3.5, vjust=-0.5)+
  ggtitle('Percentage of intersection within independent festival nominees over years')+
  xlab('Year')+ylab('Percentage')

p6<-ggplot(intersection_full, aes(x=year, y=percentage_independent)) + 
  ylim(0, 1)+
  geom_bar(stat = "identity", width=0.75,color='black',fill='cornflowerblue')+
  theme_bw()+
  geom_text(aes(label=paste(100*round(percentage_independent,2),'%',sep = "")), size =3.5, vjust=-0.5)+
  ggtitle('Percentage of intersection within independent festival nominees within 5-year window')+
  xlab('Year')+ylab('Percentage')

grid.arrange(p5,p6)





# Finally filtering out the data to leave only US films

min(df_vertice_prop$Year)


df_vertice_prop<-df_vertice_prop[df_vertice_prop$Id%in%df_crew$tconst,]

head(df_crew)
head(df_vertice_prop)

#Average team size
length(unique(df_crew$nconst))
df_crew1<-df_crew[(df_crew$tconst%in%df_vertice_prop$Id)&(df_crew$category%in%professions),]
length(unique(df_crew1$nconst))/length(unique(df_crew1$tconst))

numhist<-as.data.frame(df_crew1 %>% group_by(tconst) %>% summarise(n = n())%>% arrange(-n))
names(numhist)<-c('Id','Team_size')
head(numhist)

df_crew1 %>% group_by(tconst) %>% summarise(n = n())%>% arrange(n)

mean(numhist$Team_size)
ggplot(numhist, aes(x=Team_size)) + 
  theme_bw()+
  ggtitle('Average team size for all the movies')+
  geom_histogram(color="black", fill="blue",alpha=0.5,bins=10)+
  scale_x_continuous(breaks = round(seq(1, 10, by = 1),1))+
  geom_vline(aes(xintercept=mean(Team_size)), color="red", linetype="dashed", size=1,show.legend=TRUE)

df_prof_mn1<-merge(df_crew,df_vertice_prop[df_vertice_prop$Nomination=='mainstream',],by.x='tconst',by.y='Id')%>%group_by(tconst) %>% summarise(Team_size = n())%>% arrange(-Team_size)
df_prof_in1<-merge(df_crew,df_vertice_prop[df_vertice_prop$Nomination=='independent',],by.x='tconst',by.y='Id')%>%group_by(tconst) %>% summarise(Team_size = n())%>% arrange(-Team_size)

df_prof_mn1
df_prof_in1
df_crew[df_crew$tconst=='tt0102740',]

min(df_prof_mn1$Team_size)
mean(df_prof_mn1$Team_size)

ggplot(df_prof_mn1, aes(x=Team_size)) + 
  theme_bw()+
  ggtitle('Average team size for mainstream nominees')+
  geom_histogram(color="black", fill="blue",alpha=0.5,bins=11)+
  scale_x_continuous(breaks = seq(1, 10, by = 1))+
  geom_vline(aes(xintercept=mean(Team_size)), color="red", linetype="dashed", size=1,show.legend=TRUE)

length(unique(df_prof_in1$Team_size))

mean(df_prof_in1$Team_size)
ggplot(df_prof_in1, aes(x=Team_size)) + 
  theme_bw()+
  ggtitle('Average team size for independent nominees')+
  geom_histogram(color="black", fill="blue",alpha=0.5,bins=10)+
  scale_x_continuous(breaks = seq(1, 10, by = 1))+
  geom_vline(aes(xintercept=mean(Team_size)), color="red", linetype="dashed", size=1,show.legend=TRUE)


df_crew<-df_crew[(df_crew$tconst%in%df_vertice_prop$Id)&(df_crew$category%in%professions),]

df_crew
# Summarizing the movie info under analyzes
df_usa<-df_vertice_prop
df_usa
df_usa<-df_usa %>% group_by(Year) %>% summarise(n = n())
df_usa<-as.data.frame(df_usa)
sum(df_usa$n)

ggplot(df_usa, aes(x=Year,y=n,label = n)) + 
  geom_bar(width=0.75, stat="identity")+#,position="fill")+
  geom_col(position = position_stack(reverse = TRUE))+
  theme_bw()+
  geom_text(stat="identity", size =2.5, vjust=-0.5)+
  ggtitle('Total movies under analysis')+
  xlab('Year')+ylab('Number of movies')


head(df_crew)
#df_crew<-df_crew[,c(1,3,4)] #leaving only essential data


df_prof_total<-df_crew%>%group_by(category) %>% summarise(n = n())%>% arrange(-n)
df_prof_total<-as.data.frame(df_prof_total)
df_prof_total
names(df_prof_total)<-c('Profession','N_prof')
df_prof_total$Prof_freq<-round(df_prof_total$N_prof/sum(df_prof_total$N_prof),2)
df_prof_total
tab_df(df_prof_total,title = "Total number of proffessionals involved in production teams",file = "prof_total.doc",use.viewer=TRUE)


#Professions for all the films
df_prof<-merge(df_crew,df_vertice_prop,by.x='tconst',by.y='Id')%>%group_by(Year,category) %>% summarise(n_prof = n())%>% arrange(Year,-n_prof)
df_prof
df_prof<-as.data.frame(df_prof)
df_prof

df_prof<-merge(df_prof,df_usa,by.x='Year',by.y='Year')
names(df_prof)<-c('Year','Profession','N_prof','N_movies')
df_prof$Prof_freq<-round(df_prof$N_prof/df_prof$N_movies,2)

#Professions for nominees
df_prof_mn<-merge(df_crew,df_vertice_prop[df_vertice_prop$Nomination=='mainstream',],by.x='tconst',by.y='Id')%>%group_by(Year,category) %>% summarise(n_prof = n())%>% arrange(Year,-n_prof)
df_prof_in<-merge(df_crew,df_vertice_prop[df_vertice_prop$Nomination=='independent',],by.x='tconst',by.y='Id')%>%group_by(Year,category) %>% summarise(n_prof = n())%>% arrange(Year,-n_prof)

df_prof_mn<-as.data.frame(df_prof_mn)
df_prof_in<-as.data.frame(df_prof_in)

df_prof_mn
df_prof_in

df_usa_mn<-df_vertice_prop[df_vertice_prop$Nomination=='mainstream',]
df_usa_in<-df_vertice_prop[df_vertice_prop$Nomination=='independent',]

df_usa_mn<-df_usa_mn %>% group_by(Year) %>% summarise(n = n())
df_usa_in<-df_usa_in %>% group_by(Year) %>% summarise(n = n())

df_usa_mn<-as.data.frame(df_usa_mn)
df_usa_in<-as.data.frame(df_usa_in)

df_prof_mn<-merge(df_prof_mn,df_usa_mn,by.x='Year',by.y='Year')
df_prof_in<-merge(df_prof_in,df_usa_in,by.x='Year',by.y='Year')

names(df_prof_mn)<-names(df_prof_in)<-c('Year','Profession','N_prof','N_movies')

df_prof_mn$Prof_freq<-round(df_prof_mn$N_prof/df_prof_mn$N_movies,2)
df_prof_in$Prof_freq<-round(df_prof_in$N_prof/df_prof_in$N_movies,2)

df_prof_mn
df_prof_in


ggplot(df_prof, aes(x=Year,y=Prof_freq,label = Prof_freq,fill=Profession)) + 
  geom_bar(width=0.75, stat="identity")+#,position="fill")+
  geom_col(position = position_stack(reverse = F))+
  theme_bw()+
  geom_text(stat="identity", size =2.5,position = position_stack(vjust = 0.5))+#, vjust=-0.5)+
  ggtitle('Composition of "core crew" of all the movies under study over years')+
  xlab('Year')+ylab('Average number of proffesionals involved')


ggplot(df_prof_mn, aes(x=Year,y=Prof_freq,label = Prof_freq,fill=Profession)) + 
  geom_bar(width=0.75, stat="identity")+#,position="fill")+
  geom_col(position = position_stack(reverse = F))+
  theme_bw()+
  geom_text(stat="identity", size =2.5,position = position_stack(vjust = 0.5))+#, vjust=-0.5)+
  ggtitle('Composition of "core crew" of mainstream nominees over years')+
  xlab('Year')+ylab('Average number of proffesionals involved')

ggplot(df_prof_in, aes(x=Year,y=Prof_freq,label = Prof_freq,fill=Profession)) + 
  geom_bar(width=0.75, stat="identity")+#,position="fill")+
  geom_col(position = position_stack(reverse = F))+
  theme_bw()+
  geom_text(stat="identity", size =2.5,position = position_stack(vjust = 0.5))+#, vjust=-0.5)+
  ggtitle('Composition of "core crew" of independent nominees over years')+
  xlab('Year')+ylab('Average number of proffesionals involved')


df_prof

dim(df_prof)
dim(df_prof_mn)
dim(df_prof_in)


tab_df(cbind(df_prof[0:35,],df_prof[36:70,]),title = "Number of proffessionals in a team over year",file = "prof_by_year1.doc",use.viewer=TRUE)
tab_df(cbind(df_prof[71:105,],df_prof[106:140,]),title = "Number of proffessionals in a team over year",file = "prof_by_year2.doc",use.viewer=TRUE)
tab_df(cbind(df_prof[141:175,],df_prof[176:210,]),title = "Number of proffessionals in a team over year",file = "prof_by_year3.doc",use.viewer=TRUE)
tab_df(df_prof[211:245,],title = "Number of proffessionals in a team over year",file = "prof_by_year4.doc",use.viewer=TRUE)

tab_df(cbind(df_prof_mn[0:35,],df_prof_mn[36:70,]),title = "Number of proffessionals in a mainstream nominee team over year",file = "prof_by_year_mn1.doc",use.viewer=TRUE)
tab_df(cbind(df_prof_mn[71:105,],df_prof_mn[106:140,]),title = "Number of proffessionals in a mainstream nominee team over year",file = "prof_by_year_mn2.doc",use.viewer=TRUE)
tab_df(cbind(df_prof_mn[141:165,],df_prof_mn[166:190,]),title = "Number of proffessionals in a mainstream nominee team over year",file = "prof_by_year_mn3.doc",use.viewer=TRUE)

tab_df(cbind(df_prof_in[0:35,],df_prof_in[36:70,]),title = "Number of proffessionals in an independent nominee team over year",file = "prof_by_year_in1.doc",use.viewer=TRUE)
tab_df(cbind(df_prof_in[71:105,],df_prof_in[106:140,]),title = "Number of proffessionals in an independent nominee team over year",file = "prof_by_year_in2.doc",use.viewer=TRUE)
tab_df(cbind(df_prof_in[141:175,],df_prof_in[176:210,]),title = "Number of proffessionals in an independent nominee team over year",file = "prof_by_year_in3.doc",use.viewer=TRUE)
tab_df(df_prof_in[211:242,],title = "Number of proffessionals in an independent nominee team over year",file = "prof_by_year_in4.doc",use.viewer=TRUE)

df_prof_in

df_vertice_prop<-df_vertice_prop[df_vertice_prop$film_id%in%df_crew$tconst]




##Analyzing the genres of movies for festival nominees and in general

#All the genres in the dataframe


all_genres<-df_vertice_prop$genres #vector of all the genres listed
all_genres<-paste(all_genres, sep=",", collapse=",") #collapsing all the genres into one string
all_genres<-str_split(all_genres, ",") #separating the string back to single genres
all_genres<-as.vector(all_genres[[1]]) #transforming the list to a vector
all_genres<-unique(all_genres) #unique genres


df_genres<-data.frame(matrix(ncol = 4, nrow = 0))
for (i in year_groups) {
  for (j in all_genres){
    num_movies_independent<-sum(as.numeric(str_detect(df_vertice_prop[(df_vertice_prop$nomination=='independent')&(df_vertice_prop$year_group==i),]$genres,j)),na.rm=T)
    num_movies_mainstream<-sum(as.numeric(str_detect(df_vertice_prop[(df_vertice_prop$nomination=='mainstream')&(df_vertice_prop$year_group==i),]$genres,j)),na.rm=T)
    num_movies_both<-sum(as.numeric(str_detect(df_vertice_prop[(df_vertice_prop$nomination=='both')&(df_vertice_prop$year_group==i),]$genres,j)),na.rm=T)
    num_movies_none<-sum(as.numeric(str_detect(df_vertice_prop[(df_vertice_prop$nomination=='none')&(df_vertice_prop$year_group==i),]$genres,j)),na.rm=T)
    df_genres_temp<-cbind(year_group=i,genre=j,num_movies=num_movies_independent,type='independent')
    df_genres<-rbind(df_genres,df_genres_temp)
    df_genres_temp<-cbind(year_group=i,genre=j,num_movies=num_movies_mainstream,type='mainstream')
    df_genres<-rbind(df_genres,df_genres_temp)
    df_genres_temp<-cbind(year_group=i,genre=j,num_movies=num_movies_both,type='both')
    df_genres<-rbind(df_genres,df_genres_temp)
    df_genres_temp<-cbind(year_group=i,genre=j,num_movies=num_movies_none,type='none')
    df_genres<-rbind(df_genres,df_genres_temp)
  }
}

df_genres$num_movies<-as.integer(df_genres$num_movies)
df_genres1<-df_genres
df_genres1$genre<-ifelse(df_genres1$genre=='Drama','Drama','Other')
df_genres1<-as.data.frame(df_genres1 %>% group_by(year_group,genre,type)%>% summarise(num_movies = sum(num_movies)))




#df_genres<-arrange(df_genres,num_movies)
head(df_genres)



ggplot(df_genres1[df_genres1$type=='mainstream',], aes(fill=genre,x=year_group,y=num_movies,label = num_movies)) + 
  geom_bar(width=0.75, stat="identity",position="fill")+
  theme_bw()+
  #geom_text(stat="identity", size =4, vjust=-0.5)+
  ggtitle('Genres among mainstream nominees over years')+
  xlab('Year')+ylab('Number of movies for genre')
  
ggplot(df_genres1[df_genres1$type=='independent',], aes(fill=genre,x=year_group,y=num_movies)) + 
  geom_bar(width=0.75, stat="identity",position="fill")+
  theme_bw()+
  ggtitle('Genres among independent nominees over years')+
  xlab('Year')+ylab('Number of movies for genre')

ggplot(df_genres1[df_genres1$type=='both',], aes(fill=genre,x=year_group,y=num_movies)) + 
  geom_bar(width=0.75, stat="identity",position="fill")+
  theme_bw()+
  ggtitle('Genres among nominees of both types over years')+
  xlab('Year')+ylab('Number of movies for genre')

ggplot(df_genres1[df_genres1$type=='none',], aes(fill=genre,x=year_group,y=num_movies)) + 
  geom_bar(width=0.75, stat="identity",position="fill")+
  theme_bw()+
  ggtitle('Genres among non-nominees over years')+
  xlab('Year')+ylab('Number of movies for genre')


##  Making networks out of crew dataframes


df_crew_2<-df_crew
df_crew<-df_crew[,c(1,3)]
head(df_crew)



write.csv(df_vertice_prop,"df_vertice_prop.csv", row.names = FALSE)
write.csv(df_crew,"df_crew_filtered.csv", row.names = FALSE)