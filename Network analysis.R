setwd("D://Study//Diploma")

library(ggplot2)
library(dplyr)
library(igraph)
library(stringr)
library(readxl)
library(haven)
library(DT)
library(sjPlot)
library(DescTools)
library(gridExtra)

df_vertice_prop<-read.csv2("df_vertice_prop.csv",header=T,sep=',')
df_crew<-read.csv2("df_crew_filtered.csv",header=T,sep=',')

proj_movie_mainstream<-list()
proj_movie_independent<-list()
proj_movie_full<-list()

proj_movie_full_no_iso<-list()
proj_movie_mainstream_no_iso<-list()
proj_movie_independent_no_iso<-list()

list_movies_mainstream<-list()
list_movies_independent<-list()
list_movies_both<-list()
list_movies_full<-list()

window<-5

#Networks for moving window
for (year in (1986+window-1):2020) {
  list_movies_mainstream[[year]] <-df_vertice_prop[(df_vertice_prop$Year<=year)&(df_vertice_prop$Year>(year-window))&(df_vertice_prop$Nomination=='mainstream'),]$Id
  list_movies_independent[[year]] <-df_vertice_prop[(df_vertice_prop$Year<=year)&(df_vertice_prop$Year>(year-window))&(df_vertice_prop$Nomination=='independent'),]$Id
  list_movies_both[[year]] <-df_vertice_prop[(df_vertice_prop$Year<=year)&(df_vertice_prop$Year>(year-window))&(df_vertice_prop$Nomination=='both'),]$Id
  list_movies_full[[year]] <- df_vertice_prop[(df_vertice_prop$Year<=year)&(df_vertice_prop$Year>(year-window)),]$Id
  df_crew_mainstream <-df_crew[(df_crew$tconst%in%list_movies_mainstream[[year]]),]
  df_crew_independent<-df_crew[(df_crew$tconst%in%list_movies_independent[[year]]),]
  df_crew_full       <-df_crew[(df_crew$tconst%in%list_movies_full[[year]]),]
  #df_vertice_prop_temp<-df_vertice_prop[]
  g_mainstream<-graph.data.frame(df_crew_mainstream, directed=FALSE)#,vertices = df_vertice_prop[(df_vertice_prop$nomination=='mainstream')|(df_vertice_prop$nomination=='both'),])
  g_independent<-graph.data.frame(df_crew_independent, directed=FALSE)
  g_full<-graph.data.frame(df_crew_full, directed=FALSE)
  V(g_mainstream)$type <- bipartite_mapping(g_mainstream)$type
  V(g_independent)$type <- bipartite_mapping(g_independent)$type
  V(g_full)$type <- bipartite_mapping(g_full)$type
  proj_movie_mainstream[[year]] <- bipartite_projection(g_mainstream,which=FALSE)
  proj_movie_independent[[year]] <- bipartite_projection(g_independent,which=FALSE)
  proj_movie_full[[year]] <- bipartite_projection(g_full,which=FALSE)
  
  proj_movie_full_no_iso[[year]]<-delete.vertices(proj_movie_full[[year]], which(degree(proj_movie_full[[year]])==0))
  proj_movie_independent_no_iso[[year]]<-delete.vertices(proj_movie_independent[[year]], which(degree(proj_movie_independent[[year]])==0))
  proj_movie_mainstream_no_iso[[year]]<-delete.vertices(proj_movie_mainstream[[year]], which(degree(proj_movie_mainstream[[year]])==0))
}


# Network-level properties
options(scipen=0)

degrees_full<-list()

df_degrees<-data.frame(matrix(ncol = 6, nrow = 0))
df_differences<-data.frame(matrix(ncol = 5, nrow = 0))

for (year in (1986+window-1):2020) {
  degrees_full[[year]] <- as.data.frame(igraph::degree(proj_movie_full_no_iso[[year]], mode="all",normalized=F))
  
  degrees_independent<-degrees_full[[year]][list_movies_independent[[year]],]
  degrees_mainstream<-degrees_full[[year]][list_movies_mainstream[[year]],]
  degrees_both<-degrees_full[[year]][list_movies_both[[year]],]
  
  mean_degree_full<-signif(mean(degrees_full[[year]][,1],na.rm=T),digits=4)
  mean_degree_independent<-signif(mean(degrees_independent,na.rm=T),digits=4)
  mean_degree_mainstream<-signif(mean(degrees_mainstream,na.rm=T),digits=4)
  mean_degree_both<-signif(mean(degrees_both,na.rm=T),digits=4)
  
  number_total<-gorder(proj_movie_full[[year]]) #total size,including isolates
  number_isolated <- length(which(degree(proj_movie_full[[year]])==0))
  
  number_ind<-gorder(proj_movie_independent_no_iso[[year]]) #size without isolates
  number_main<-gorder(proj_movie_mainstream_no_iso[[year]]) #size without isolates
  number_both<-ifelse(length(list_movies_both[[year]])==0,0,gorder(subgraph(proj_movie_full_no_iso[[year]],list_movies_both[[year]]))) #size without isolates
  
  number_is_ind<-length(which(degree(proj_movie_independent[[year]])==0))
  number_is_main<-length(which(degree(proj_movie_mainstream[[year]])==0)) 
  number_is_both<-length(intersect(which(degree(proj_movie_full[[year]])==0),list_movies_both[[year]])) 
  
  centralization<-centr_degree(proj_movie_full_no_iso[[year]],mode = "all",loops = TRUE,normalized = TRUE)$centralization
  
  this_year_full<-cbind(Year=year,Mean_degree=mean_degree_full,Isolates=number_isolated,Number_nodes=number_total,Centralization=centralization,type='full')
  this_year_mainstream<-cbind(Year=year,Mean_degree=mean_degree_mainstream,Isolates=number_is_main,Number_nodes=number_main,Centralization=NA,type='mainstream')
  this_year_independent<-cbind(Year=year,Mean_degree=mean_degree_independent,Isolates=number_is_ind,Number_nodes=number_ind,Centralization=NA,type='independent')
  this_year_both<-cbind(Year=year,Mean_degree=mean_degree_both,Isolates=number_is_both,Number_nodes=number_both,Centralization=NA,type='both')
  this_year_isolates<-cbind(Year=year,Mean_degree=NA,Isolates=number_isolated,Number_nodes=number_isolated,Centralization=NA,type='isolates')
  
  df_degrees<-rbind(df_degrees,this_year_mainstream,this_year_independent,this_year_full,this_year_both,this_year_isolates)
  
  emptyvec<-c(0,0,0)
  
  diff_mi<-if (min(length(degrees_mainstream),length(degrees_independent))<=2) {emptyvec} else {MeanDiffCI(degrees_mainstream,degrees_independent,na.rm=T)}
  diff_mf<-if (min(length(degrees_mainstream),length(degrees_full[[year]][,1]))<=2) {emptyvec} else {MeanDiffCI(degrees_mainstream,degrees_full[[year]][,1],na.rm=T)}
  diff_if<-if (min(length(degrees_independent),length(degrees_full[[year]][,1]))<=2) {emptyvec} else {MeanDiffCI(degrees_independent,degrees_full[[year]][,1],na.rm=T)}
  diff_bf<-if (min(length(degrees_both),length(degrees_full[[year]][,1]))<=2) {emptyvec} else {MeanDiffCI(degrees_both,degrees_full[[year]][,1],na.rm=T)}
  
  this_year_diff_mi<-cbind(Year=year,Mean_diff=diff_mi[1],Lwr.ci=diff_mi[2],Upr.ci=diff_mi[3],type='Mainstream vs independent')
  this_year_diff_mf<-cbind(Year=year,Mean_diff=diff_mf[1],Lwr.ci=diff_mf[2],Upr.ci=diff_mf[3],type='Mainstream vs average')
  this_year_diff_if<-cbind(Year=year,Mean_diff=diff_if[1],Lwr.ci=diff_if[2],Upr.ci=diff_if[3],type='Independent vs average')
  this_year_diff_bf<-cbind(Year=year,Mean_diff=diff_bf[1],Lwr.ci=diff_bf[2],Upr.ci=diff_bf[3],type='Both vs average')
  df_differences<-rbind(df_differences,this_year_diff_mi,this_year_diff_mf,this_year_diff_if,this_year_diff_bf)
  
}

df_degrees$Mean_degree<-as.numeric(df_degrees$Mean_degree)
df_degrees$Isolates<-as.numeric(df_degrees$Isolates)
df_degrees$Number_nodes<-as.numeric(df_degrees$Number_nodes)
df_degrees$Share_isolates<-df_degrees$Isolates/(df_degrees$Number_nodes+df_degrees$Isolates)
df_degrees$Centralization<-as.numeric(df_degrees$Centralization)

df_differences$Mean_diff<-as.numeric(df_differences$Mean_diff)
df_differences$Lwr.ci<-as.numeric(df_differences$Lwr.ci)
df_differences$Upr.ci<-as.numeric(df_differences$Upr.ci)
df_differences$Signif<-ifelse((sign(df_differences$Lwr.ci)==sign(df_differences$Upr.ci))&(df_differences$Mean_diff!=0),'Significant','Insignificant')

head(df_degrees)
head(df_differences)

df_iso<-cbind(df_degrees[df_degrees$type=='full',c(1,4)],df_degrees[df_degrees$type=='isolates',4]) #separate dataframe to plot number of isolates
names(df_iso)<-c('Year','Number_nodes','Number_isolates')
df_iso$Share_isolates<-df_iso$Number_isolates/df_iso$Number_nodes
df_iso

mean(df_degrees[df_degrees$type=='full',]$Centralization)
# Plot the average size of the whole network
ggplot(df_degrees[(df_degrees$type=='full')|(df_degrees$type=='isolates'),],aes(x=Year,y=Number_nodes))+
  theme_bw()+
  geom_line(aes(color=type,group=type))+
  ggtitle('Size of network and share of isolates over years')+
  geom_text(data=df_iso,  aes(y=Number_isolates+10,label=paste(100*round(Share_isolates,2),'%',sep = "")), size =3.5, vjust=-0.5)

# Plot the average share of isolates among nominees
ggplot(df_degrees[(df_degrees$type=='mainstream')|(df_degrees$type=='independent'),],aes(x=Year,y=Share_isolates,group=type))+
  theme_bw()+
  geom_line()+
  ggtitle('Average share of isolates among nominees over years')+
  geom_line(aes(color=type))

MeanDiffCI(df_degrees[(df_degrees$type=='independent'),'Share_isolates'],df_degrees[(df_degrees$type=='mainstream'),'Share_isolates'],na.rm=T)


# Plot the average size of nominee sample
ggplot(df_degrees[(df_degrees$type!='full')&(df_degrees$type!='isolates'),],aes(x=Year,y=Number_nodes,fill=type))+
  theme_bw()+
  geom_bar(position="dodge",stat='identity', width=0.75)+
  #geom_line()+
  ggtitle('Size of nominee sample within the network over years')


# Plot of average centralization of the network
ggplot(df_degrees[df_degrees$type=='full',],aes(x=Year,y=Centralization,group=1))+
  theme_bw()+
  geom_line()+
  ggtitle('Normalized centralization in the network over years')
  #geom_line(aes(color=type))


# Plot of average node degree
ggplot(df_degrees[df_degrees$type!='isolates',],aes(x=Year,y=Mean_degree,group=type))+
  theme_bw()+
  geom_line()+
  ggtitle('Average node degree of nominees over years')+
  geom_line(aes(color=type))

# Differences between degrees
ggplot(data=df_differences[df_differences$type=='Mainstream vs independent',], aes(Year, Mean_diff,group=type)) +
  theme_bw()+
  ggtitle('Differences in mean degrees between mainstream and independent nominees')+
  geom_col(aes(fill = type), color = "black", width = 0.85,position = position_dodge(1)) +
  geom_errorbar(aes(ymin = Lwr.ci, ymax = Upr.ci),width = .1,position = position_dodge(1))+
  geom_point(mapping = aes(x = Year, y = Mean_diff,group=type,color=Signif),position = position_dodge(1),size=1.5)+
  scale_color_manual(values = c("Insignificant" = "grey80", "Significant" = "limegreen"))

# Differences between degrees
ggplot(data=df_differences[df_differences$type!='Mainstream vs independent',], aes(Year, Mean_diff,group=type)) +
  theme_bw()+
  ggtitle('Differences in mean degrees between groups of nominees')+
  geom_col(aes(fill = type), color = "black", width = 0.85,position = position_dodge(1)) +
  geom_errorbar(aes(ymin = Lwr.ci, ymax = Upr.ci),width = .1,position = position_dodge(1))+
  geom_point(mapping = aes(x = Year, y = Mean_diff,group=type,color=Signif),position = position_dodge(1),size=1.5)+
  scale_color_manual(values = c("Insignificant" = "grey80", "Significant" = "limegreen"))




##Core-periphery structure
cp_full <- readRDS("cp_full.RDS")
cp_corr <- readRDS("cp_corr.RDS")

df_cp<- data.frame(matrix(ncol = 6, nrow = 0)) #initializing the dataframe
for (year in (1986+window-1):2020) {
  number_total<-gorder(proj_movie_full[[year]])
  number_ind<-gorder(proj_movie_independent[[year]])
  number_main<-gorder(proj_movie_mainstream[[year]])
  number_both<-gorder(subgraph(proj_movie_full[[year]],list_movies_both[[year]]))
  
  size_core<-sum(cp_full[[year]],na.rm=T)
  main_core<-sum(cp_full[[year]][list_movies_mainstream[[year]]],na.rm=T)
  ind_core<-sum(cp_full[[year]][list_movies_independent[[year]]],na.rm=T)
  both_core<-sum(cp_full[[year]][list_movies_both[[year]]],na.rm=T)
  
  number_isolated<- length(which(degree(proj_movie_full[[year]])==0))
  number_is_ind<-length(which(degree(proj_movie_independent[[year]])==0))
  number_is_main<-length(which(degree(proj_movie_mainstream[[year]])==0)) 
  number_is_both<-length(intersect(which(degree(proj_movie_full[[year]])==0),list_movies_both[[year]])) 
  
  this_year_full<-cbind(Year=year,Corr_cp=cp_corr[[year]],Total=number_total,Size_core=size_core,Number_isolates=number_isolated,type='full')
  this_year_main<-cbind(Year=year,Corr_cp=NA,Total=number_main,Size_core=main_core,Number_isolates=number_is_main,type='mainstream')
  this_year_ind<-cbind(Year=year,Corr_cp=NA,Total=number_ind,Size_core=ind_core,Number_isolates=number_is_ind,type='independent')
  this_year_both<-cbind(Year=year,Corr_cp=NA,Total=number_both,Size_core=both_core,Number_isolates=number_is_both,type='both')
  
  this_year<-cbind(Year=year,Corr_cp=cp_corr[[year]],Size_core=size_core,Mainstream_core=mainstream_core,Independent_core=independent_core,Both_core=both_core)
  df_cp<-rbind(df_cp,this_year_full,this_year_main,this_year_ind,this_year_both)
}

df_cp$Corr_cp<-as.numeric(df_cp$Corr_cp)
df_cp$Total<-as.numeric(df_cp$Total)
df_cp$Size_core<-as.numeric(df_cp$Size_core)
df_cp$Number_isolates<-as.numeric(df_cp$Number_isolates)

df_cp$Share_core<-df_cp$Size_core/(df_cp$Total-df_cp$Number_isolates)

# Plot the correlation with the perfect core-periphery model
ggplot(df_cp[df_cp$type=='full',],aes(x=Year,y=Corr_cp))+
  theme_bw()+
  geom_line(group=1)+
  ggtitle('Correlation with perfect core-periphery structure over years')

# Share of nodes in the core among non-isolates
ggplot(df_cp,aes(x=Year,y=Share_core,))+
  theme_bw()+
  geom_line(aes(color=type,group=type))+
  ggtitle('Share of nodes in the core among non-isolates')

min(df_cp[df_cp$type=='full',]$Share_core)
max(df_cp[df_cp$type=='full',]$Share_core)
mean(df_cp[(df_cp$type=='full')&(df_cp$Corr_cp>=0.2),]$Share_core)
df_cp[df_cp$type=='full',]
df_cp

mean(df_cp[df_cp$type=='full',]$Corr_cp)
min(df_cp[df_cp$type=='full',]$Corr_cp)
max(df_cp[df_cp$type=='full',]$Corr_cp)

df_degrees[(df_degrees$type=='full')&(df_degrees$Year==1991),'Centralization']/df_degrees[(df_degrees$type=='full')&(df_degrees$Year==2006),'Centralization']
df_cp[(df_cp$type=='full')&(df_cp$Year==1991),'Corr_cp']/df_cp[(df_cp$type=='full')&(df_cp$Year==2006),'Corr_cp']

df_degrees[(df_degrees$type=='full')&(df_degrees$Year==2015),'Centralization']/df_degrees[(df_degrees$type=='full')&(df_degrees$Year==2007),'Centralization']
df_cp[(df_cp$type=='full')&(df_cp$Year==2015),'Corr_cp']/df_cp[(df_cp$type=='full')&(df_cp$Year==2007),'Corr_cp']

cent<-df_degrees[(df_degrees$type=='full'),'Centralization']
corr<-df_cp[(df_cp$type=='full'),'Corr_cp']
cor(cent,corr)
#Network plots
par(mfrow = c(1, 1))

list_movies_mainstream <-df_vertice_prop[(df_vertice_prop$Nomination=='mainstream'),]$Id
list_movies_independent <-df_vertice_prop[(df_vertice_prop$Nomination=='independent'),]$Id
list_movies_both <-df_vertice_prop[(df_vertice_prop$Nomination=='both'),]$Id

df_crew_mainstream <-df_crew[(df_crew$tconst%in%list_movies_mainstream),]
df_crew_independent<-df_crew[(df_crew$tconst%in%list_movies_independent),]
df_crew_both       <-df_crew[(df_crew$tconst%in%list_movies_both),]

g_mainstream<-graph.data.frame(df_crew_mainstream, directed=FALSE)
g_independent<-graph.data.frame(df_crew_independent, directed=FALSE)
g_both<-graph.data.frame(df_crew_both, directed=FALSE)

V(g_mainstream)$type <- bipartite_mapping(g_mainstream)$type
V(g_independent)$type <- bipartite_mapping(g_independent)$type
V(g_both)$type <- bipartite_mapping(g_both)$type

proj_movie_mainstream <- bipartite_projection(g_mainstream,which=FALSE)
proj_movie_independent <- bipartite_projection(g_independent,which=FALSE)
proj_movie_both <- bipartite_projection(g_both,which=FALSE)


par(mfrow = c(1, 2))
plot(proj_movie_mainstream,vertex.size=1,vertex.label=NA,layout=layout_with_fr) +title("All mainstream movies 1986-2020", line = 0)
plot(proj_movie_independent,vertex.size=1,vertex.label=NA,layout=layout_with_fr)+title("All independent movies 1986-2020", line = 0)
plot(proj_movie_both,vertex.size=1,vertex.label=NA,layout=layout_with_fr,main='All nominees of both type of festivals 1986-2020')

centr_main<-centr_degree(proj_movie_mainstream,mode = "all",loops = TRUE,normalized = TRUE)$centralization
centr_ind<-centr_degree(proj_movie_independent,mode = "all",loops = TRUE,normalized = TRUE)$centralization
centr_main/centr_ind
round(centr_main,2)
round(centr_ind,2)

CFG_main<-cluster_fast_greedy(proj_movie_mainstream) #communities in mainstream network
CFG_ind<-cluster_fast_greedy(proj_movie_independent) #communities in independent network
modularity(CFG_main)
modularity(CFG_ind)


# Histograms with degree distribution
deg_movie_independent_all <- as.data.frame(igraph::degree(proj_movie_independent, mode="all",normalized=TRUE))
deg_movie_mainstream_all <- as.data.frame(igraph::degree(proj_movie_mainstream, mode="all",normalized=TRUE))
colnames(deg_movie_independent_all)<-colnames(deg_movie_mainstream_all)<-'degree'
hist_main<-ggplot(deg_movie_mainstream_all,aes(x=degree))+ 
  theme_bw()+
  stat_ecdf(color="blue")+
  #geom_bar(aes(y = (..count..)/sum(..count..)),fill="lightblue") + 
  scale_y_continuous(labels=scales::percent) +
  xlab('Normalized degree')+ylab('Relative frequency')+expand_limits(x=c(0,0.15),y=c(0,0.2))+theme(aspect.ratio=0.5)+labs(title='Cumulative distribution of normalized degree within the network of mainstream nominees') 
hist_ind<-ggplot(deg_movie_independent_all,aes(x=degree))+
  theme_bw()+
  stat_ecdf(color="red")+
  #geom_bar(aes(y = (..count..)/sum(..count..)),fill="lightblue") + 
  scale_y_continuous(labels=scales::percent) +
  xlab('Normalized degree')+ylab('Relative frequency')+expand_limits(x=c(0,0.15),y=c(0,0.2)) +theme(aspect.ratio=0.5) +labs(title='Cumulative distribution of normalized degree within the network of independent nominees') 
grid.arrange(hist_main,hist_ind)

sum(deg_movie_mainstream_all$degree>max(deg_movie_independent_all$degree))/length(deg_movie_mainstream_all$degree)

mean(deg_movie_mainstream_all$degree)/mean(deg_movie_independent_all$degree)

# Non-normalized
deg_movie_independent_all_non <- as.data.frame(igraph::degree(proj_movie_independent, mode="all",normalized=FALSE))
deg_movie_mainstream_all_non <- as.data.frame(igraph::degree(proj_movie_mainstream, mode="all",normalized=FALSE))
colnames(deg_movie_independent_all_non)<-colnames(deg_movie_mainstream_all_non)<-'degree'
hist_main_non<-ggplot(deg_movie_mainstream_all_non,aes(x=degree))+ 
  theme_bw()+
  stat_ecdf(color="blue")+
  #geom_bar(aes(y = (..count..)/sum(..count..)),fill="lightblue") + 
  scale_y_continuous(labels=scales::percent) +
  xlab('Degree')+ylab('Relative frequency')+expand_limits(x=c(0,20),y=c(0,0.2))+theme(aspect.ratio=0.5)+labs(title='Cumulative distribution of degree within the network of mainstream nominees') 
hist_ind_non<-ggplot(deg_movie_independent_all_non,aes(x=degree))+
  theme_bw()+
  stat_ecdf(color="red")+
  #geom_bar(aes(y = (..count..)/sum(..count..)),fill="lightblue") + 
  scale_y_continuous(labels=scales::percent) +
  xlab('Degree')+ylab('Relative frequency')+expand_limits(x=c(0,20),y=c(0,0.2)) +theme(aspect.ratio=0.5) +labs(title='Cumulative distribution of degree within the network of independent nominees') 
grid.arrange(hist_main_non,hist_ind_non)

mean(deg_movie_mainstream_all_non$degree)/mean(deg_movie_independent_all_non$degree)