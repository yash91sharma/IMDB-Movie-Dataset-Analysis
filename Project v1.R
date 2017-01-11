
rm(list=ls())
getwd()

install.packages("plotly")

library(devtools)
library(tibble)
library(printr)
library(prettydoc)
library(ggplot2)
library(knitr)
library(dplyr)
library(fitdistrplus)
library(plotly)


#devtools::install_github("metrumresearchgroup/ggedit",subdir="ggedit")
#p0 <- ggplot(movie, aes(title_year, fill = color)) + geom_histogram(position = "dodge")
#library(ggedit)
#a = ggedit(p0)
#plot(p0)

rm(movie)
url <- "https://github.com/yash91sharma/projectX/raw/master/movie_metadata.csv"
movie <- as_data_frame(read.csv(url, stringsAsFactors = FALSE))
class(movie)
head(movie)
colnames(movie)
table(movie$movie_title)
#ggplot(movie, aes(title_year, fill = color)) + geom_histogram(position = "dodge")

#missing value analysis
sum(complete.cases(movie))

#cleaning
movie$movie_title <- (sapply(movie$movie_title,gsub,pattern="Â",replacement=""))
movie$genres_2 <- (sapply(movie$genres,gsub,pattern="\\|",replacement=" "))
movie$plot_keywords_2 <- (sapply(movie$plot_keywords,gsub,pattern="\\|",replacement=" "))

#remove duplicates
movie = movie[!duplicated(movie$movie_title),]

#clean revenue and budget
unique(movie$country)
summary(movie$gross)
movie <- transform(movie, budget = ifelse(country == "South Korea", budget/1173.49, budget))
movie <- transform(movie, budget = ifelse(country == "Japan", budget/115.33, budget))
movie <- transform(movie, budget = ifelse(country == "Turkey", budget/3.49, budget))
movie <- transform(movie, budget = ifelse(country == "Hungary", budget/298.17, budget))
movie <- transform(movie, budget = ifelse(country == "Thailand", budget/35.67, budget))

movie <- transform(movie, gross = ifelse(country == "South Korea", gross/1173.49, gross))
movie <- transform(movie, gross = ifelse(country == "Japan", gross/115.33, gross))
movie <- transform(movie, gross = ifelse(country == "Turkey", gross/3.49, gross))
movie <- transform(movie, gross = ifelse(country == "Hungary", gross/298.17, gross))
movie <- transform(movie, gross = ifelse(country == "Thailand", gross/35.67, gross))

#check missing data
print(paste(sum(complete.cases(movie)),"Complete cases!"))

#Data description
Variable_type <- lapply(movie,class)
Variable_desc <- c("Specifies if it was color/black & white movie",
"Name of movie director","Number of critics who reviewed",
"Duration of the movie (minutes)","Number of likes on director's FB page",
"Number of likes on 3rd actor's FB page","Name of second actor",
"Number of likes on 1st actor's FB page","Gross earning by the movie ($)",
"Genres of the movie","Name of the first actor",
"Title of the movie","Number of users voted on IMDB",
"Total facebook likes for all cast members","Name of the third actor",
"Number of the actor who featured in the movie poster",
"Keywords describing the movie plot","IMDB link of the movie",
"Number of users who gave a review","Language of the movie",
"Country the movie was produced in",
"Content rating of the movie","Budget of the movie ($)",
"Year the movie released in","Number of facebook likes for actor 2",
"IMDB score for the movie (out of 10)","Aspect ratio the movie was made in",
"Number of facebook likes")
Variable_name <- colnames(movie)
data_desc <- data.frame(cbind(Variable_name,Variable_type,Variable_desc))
colnames(data_desc) <- c("Variable Name","Data Type","Variable Description")
kable(data_desc)



#install packegs for text mining
Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
install.packages(Needed, dependencies=TRUE)   
install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")
install.packages("slam")

#genre
library(tm)
genre <- Corpus(VectorSource(movie$genres_2))
inspect(genre)
genre_dtm <- DocumentTermMatrix(genre)
genre_tdm <- TermDocumentMatrix(genre)
genre_freq <- colSums(as.matrix(genre_dtm))
freq <- sort(colSums(as.matrix(genre_dtm)), decreasing=TRUE) 
length(genre_freq)
genre_ord <- order(genre_freq)
genre_freq[head(genre_ord)]
head(table(genre_freq), 20)
tail(table(genre_freq), 20)
genre_wf <- data.frame(word=names(genre_freq), freq=genre_freq)

library(ggplot2)
library(dplyr)
ggplot(genre_wf, aes(x=reorder(word,-freq), y=freq))+ 
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=45, hjust=1))

#install.packages("wordcloud")
library(wordcloud)
set.seed(10)
pal2 <- brewer.pal(8,"Dark2")
wordcloud(genre_wf$word,genre_wf$freq,random.order=FALSE,
          rot.per=.15, colors=pal2,scale=c(4,.9))

#genre associations
word_assoc <- function(word)
{
  assoc_1 <- as.data.frame(findAssocs(genre_dtm,c(word),corlimit = 0.1))
  assoc_1$words <- rownames(assoc_1)
  colnames(assoc_1)[1] <- c("score")
  assoc_1$key <- c(word)
  rownames(assoc_1) <- NULL
  return(assoc_1)
}

drama_assoc <- word_assoc("drama")
comedy_assoc <- word_assoc("comedy")
thriller_assoc <- word_assoc("thriller")
action_assoc <- word_assoc("action")
romance_assoc <- word_assoc("romance")
adventure_assoc <- word_assoc("adventure")
crime_assoc <- word_assoc("crime")
assoc <- rbind(drama_assoc,comedy_assoc,thriller_assoc,action_assoc,
               romance_assoc,adventure_assoc,crime_assoc)

assoc$n = as.numeric(factor(assoc$key))
assoc = ddply(assoc,.(key,words),transform, x=paste(c(rep(' ',n-1), words), collapse=''))
assoc$x = factor(assoc$x, levels=assoc[order(assoc$score), 'x'])

ggplot(assoc,aes(x=x,y=score))+
  geom_bar(stat="identity")+
  facet_grid(~key,scales = 'free',space="free_x")+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  ylab("Association Score")+
  xlab("Genre")+
  ggtitle("Genre Associations")



#Country Analysis
unique(movie$country)
unique(movie$title_year)
unique(movie$language)

country_count <- movie %>%
  subset(country != "") %>%
  na.omit() %>%
  group_by(country,title_year) %>%
  summarise(count=n())

colnames(country_count)[3] <- "Movie_Count"
ggplot(country_count,aes(title_year,country))+
  geom_tile(aes(fill=log(Movie_Count)),colour="white")+
  scale_fill_gradient(low="light blue",high = "dark blue")+
  xlab("Year of movie release")+
  ylab("Country")+
  ggtitle("Heat Map: Country vs Movie Release Year")+
  guides(fill=FALSE)

library(dplyr)
country_summary <- movie %>%
  subset(country != "") %>%
  subset(country != "New Line") %>%
  group_by(country) %>%
  summarise(count=n(),
            avg_score=mean(imdb_score,na.rm="true"),
            avg_budget = mean(budget,na.rm="true"),
            avg_gross = mean(gross,na.rm="true"))
country_with_multiple_movies <- subset(country_summary,count>1)[1]

#budget
ggplot(country_summary[complete.cases(country_summary), ],
       aes(x=reorder(country,-avg_budget),avg_budget/1000000))+
  geom_bar(stat = "identity")+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  ylab("Average Movie Budget (Million $)")+
  xlab("")+
  ggtitle("Average Movie Budget by Country")

budget_rank <- country_summary[complete.cases(country_summary), c(1,4)]
budget_rank <- budget_rank[order(-budget_rank$avg_budget),]
budget_rank$rank <- seq.int(nrow(budget_rank))
movie_temp <- merge(x=movie,y=budget_rank, by = "country",all.x = TRUE)

ggplot(subset(movie_temp,country %in% country_with_multiple_movies$country),
       aes(x=reorder(country,rank),y=budget/1000000))+
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  ylab("Average Movie Budget (Million $)")+
  xlab("")+
  ggtitle("Movie Budget variation by Country")+
  ylim(0,100)
rm(movie_temp)

#revenue
ggplot(country_summary[complete.cases(country_summary), ],
       aes(x=reorder(country,-avg_gross),avg_gross/1000000))+
  geom_bar(stat = "identity")+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  ylab("Average Movie Revenue (Million $)")+
  xlab("")+
  ggtitle("Average Movie Revenue by Country")

revenue_rank <- country_summary[complete.cases(country_summary), c(1,5)]
revenue_rank <- revenue_rank[order(-revenue_rank$avg_gross),]
revenue_rank$rank <- seq.int(nrow(revenue_rank))
movie_temp <- merge(x=movie,y=revenue_rank, by = "country",all.x = TRUE)

ggplot(subset(movie_temp,country %in% country_with_multiple_movies$country),
       aes(x=reorder(country,rank),y=gross/1000000))+
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  ylab("Average Movie Revenue (Million $)")+
  xlab("")+
  ggtitle("Movie Revenue variation by Country")+
  ylim(0,50)
rm(movie_temp)

#Country vs Language
country_language <- movie %>%
  subset(country != "") %>%
  subset(language != "") %>%
  group_by(country,language) %>%
  summarise(count=n())

colnames(country_language)[3] <- "Movie_Count"
ggplot(country_language,aes(language,country))+
  geom_tile(aes(fill=log(Movie_Count)),colour="white")+
  scale_fill_gradient(low="light blue",high = "dark blue")+
  xlab("Language")+
  ylab("Country")+
  ggtitle("Heat Map: Country vs Language")+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  guides(fill=FALSE)

# director analysis
director_summary <- movie %>%
  group_by(director_name) %>%
  summarise(num_movie = n(),
            avg_score = mean(imdb_score)) %>%
  subset(director_name != "") %>%
  subset(num_movie > 10) %>%
  arrange(-num_movie)

ggplot(director_summary,aes(x=reorder(director_name,num_movie),y=num_movie))+
  geom_bar(stat = "identity")+
  coord_flip()+
  ylab("Number of Movies")+
  ggtitle("Directors with most number of movies")+
  xlab("")

ggplot(director_summary,aes(x=reorder(director_name,avg_score),y=avg_score))+
  geom_bar(stat = "identity")+
  coord_flip()+
  ylab("Average IMDB Score")+
  ggtitle("Directors with highest IMDB Score (more than 10 movies directed)")+
  xlab("")

#profit
movie$profit <- movie$gross - movie$budget
summary(movie$profit)
movie$profit_m <- movie$profit/1000000
movie$budget_m <- movie$budget/1000000
movie$gross_m <- movie$gross/1000000
movie$profitable[movie$gross>movie$budget] <- 1
movie$profitable[movie$gross<=movie$budget] <- 0
movie$profitable <- as.factor(movie$profitable)
table(movie$profitable)

summary(movie$profit_m)

ggplot(movie,
       aes(profit_m))+
  geom_histogram()+
  scale_x_continuous(limits=c(-200,600))+
  ylab("Count of Movies")+
  xlab("Profits - Million $")+
  ggtitle("Histogram: Movie profits")

ggplot(movie,aes(x=budget_m,y=gross_m,color=profitable))+
  geom_point(size=1)+
  scale_y_continuous(limits = c(0,500))+
  scale_x_continuous(limits = c(0,400))+
  geom_abline(intercept = 0, slope = 1,color="grey",size=1)+
  annotate("text", label = "2050 Profitable Movies",
           x = 320, y = 360, size = 4, colour = "black",angle=30)+
  annotate("text", label = "1841 Non-profitable Movies",
           x = 350, y = 320, size = 4, colour = "black",angle=30)+
  ggtitle("Scatter Plot: Budget vs Revenue")+
  xlab("Budget - Million $")+
  ylab("Gross Revenue - Million $")

#profit by directors
director_summary <- movie %>%
  group_by(director_name) %>%
  summarise(num_movie = n(),
            avg_score = mean(imdb_score),
            profit = sum(gross_m,na.rm=TRUE),
            budget = sum(budget_m,na.rm=TRUE)) %>%
  subset(director_name != "") %>%
  subset(num_movie > 10) %>%
  arrange(-num_movie)
director_summary$profitable <- "No"
director_summary$profitable[director_summary$profit>director_summary$budget] <- "Yes"
director_summary$profitable <- as.factor(director_summary$profitable)

ggplot(director_summary,aes(x=budget,y=profit,color=profitable))+
  geom_abline(intercept = 0, slope = 1,color="grey",size=1,alpha=0.5)+
  geom_point(aes(size=avg_score),alpha=0.8)+
  ggtitle("Scatter Plot depicting profitability of Directors (with 10+ movies)")+
  xlab("Total Budget - Million $")+
  ylab("Total Revenues - Million $")

# IMDB Score
unique(movie$imdb_score)

ggplot(movie,aes(imdb_score))+
  geom_histogram(bins=80)+
  geom_vline(xintercept = mean(movie$imdb_score,na.rm = TRUE),colour="steel blue")+
  geom_vline(xintercept = quantile(movie$imdb_score, prob = c(0.05)),colour="red",linetype = "longdash")+
  geom_vline(xintercept = quantile(movie$imdb_score, prob = c(0.95)),colour="red",linetype = "longdash")+
  annotate("text", label = "5th Percentile (4.3)",x = 4.2, y = 100, size = 4, colour = "red",angle=90)+
  annotate("text", label = "95th Percentile (8.1)",x = 8.2, y = 100, size = 4, colour = "red",angle=90)+
  annotate("text", label = "Mean (6.4)",x = 6.1, y = 100, size = 4, colour = "light blue",angle=90)+
  xlab("Count of Movies")+
  ylab("IMDB Score")+
  ggtitle("Histogram: IMDB Score")

descdist(movie$imdb_score, boot = 100)
ks.test(movie$imdb_score,"gamma")
ks.test(movie$imdb_score,"plnorm")

movie$profit_flag <- as.factor(ifelse((movie$gross > movie$budget),1,0))

plot_ly(movie, x = ~imdb_score, y = ~budget/1000000, z = ~gross/1000000, 
        color = ~profit_flag, colors = c('#BF382A', '#0C4B8E'),size = I(3)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'IMDB Score'),
                      yaxis = list(title = 'Budget (M$)'),
                      zaxis = list(title = 'Revenue (M$)')),
         title = "3D Scatter plot: IMDB Score vs Revenue vs Budget")

#profit by country
imdb_country <- movie %>%
  group_by(country) %>%
  summarise(num_movie = n(),
            avg_score = mean(imdb_score,na.rm=TRUE),
            profit = (sum(gross,na.rm=TRUE)-sum(budget,na.rm=TRUE))/1000000) %>%
  subset(country != "") %>%
  subset(num_movie > 5) %>%
  arrange(-num_movie)
imdb_country$profit_flag <- as.factor(ifelse(imdb_country$profit > 0 , 1,0))
imdb_country$profit_2 <- imdb_country$profit + 2009

plot_ly(imdb_country, x = ~profit_2, y = ~avg_score, 
        color = ~profit_flag, colors = c('red', 'green'),
        size = ~num_movie,text=~country) %>%
  add_markers() %>%
  layout(xaxis = list(type = "log",title="Profitability",
                      zeroline = TRUE,showline = TRUE,showticklabels = FALSE,showgrid = TRUE),
         yaxis = list(title="Average IMDB Score",
                      zeroline = TRUE,showline = TRUE,showticklabels = FALSE,showgrid = TRUE),
         title = "Interactive Scatter: IMDB Score vs Profitability",
         showlegend = FALSE)
  
  movie %>%
  group_by(country) %>%
  summarise(num = n_distinct(language)) %>%
  arrange(-num) %>%
  subset(num > 3) %>%
  ggplot(aes(y=num,x=reorder(country,-num)))+
    geom_bar(stat = "identity")+
    xlab("")+ylab("Number of Languages")+
    ggtitle("Top countries by number of languages of films produced")
