##### Data Import in R from CSV #####
##### We will use the data for Premier league for the complete exercise ###
library(magrittr)
library(dplyr)
summary(foot16)
summary(foot_17)
foot_16<-foot16[!is.na(foot16$matchID),]
foot_17<-foot17[!is.na(foot17$matchID),]
foot_16[sapply(foot_16, is.character)] <- lapply(foot_16[sapply(foot_16, is.character)],as.factor)
foot_17[sapply(foot_17, is.character)] <- lapply(foot_17[sapply(foot_17, is.character)],as.factor)
foot_16$PlayerRef<-as.character(foot_16$PlayerRef)
foot_17$PlayerRef<-as.character(foot_17$PlayerRef)
foot_16$name<-as.character(foot_16$name)
foot_17$name<-as.character(foot_17$name)

hist(foot_16$points,xlab = "Points",col = "yellow",border = "blue")
hist(foot_17$points,xlab = "Points",col = "yellow",border = "blue")

### cross tabulation by players on points ###
group_by(foot_16, name) %>% summarise(mean_points = mean(points, na.rm  = TRUE)) %>% arrange(desc(mean_points))

v<-data.frame(group_by(foot_16, name) %>% 
  summarise(mean_points = mean(points, na.rm  = TRUE)) %>% 
  arrange(desc(mean_points)) %>%
  top_n(n=10))
### Players performance away vs home games ###
foot_16 %>%
  filter(name %in% v$name) %>%
  group_by(name,home.away) %>%
  summarise(points=mean(points))

### All data for top players and their histogram/scatter plot ###

top_player_data<-foot_16 %>% filter(name %in% v$name)
for (i in unique(top_player_data$PlayerRef))
{
  player_data<-top_player_data[which(top_player_data$PlayerRef==i),]
  hist(player_data$points,xlab = "Points",col = "yellow",border = "blue",main = paste("Histogram of" ,unique(player_data$name)))
  plot(player_data$matchID,player_data$points)
}

### testing the time dependency of players (Do some of them play good at seasons first half or second) ###
perf<-NULL
for (i in unique(top_player_data$PlayerRef))
{
  player_data<-top_player_data[which(top_player_data$PlayerRef==i),]
  if (nrow(player_data)>20)
  {
    fit<-lm(points~matchID,data=player_data)
    temp<-data.frame(t(c(unique(player_data$name),fit$coefficients[2])))
    names(temp)<-c("player","coefficient")
    perf<-rbind(perf,temp)
  }
}

#### 
foot_16$opposition<-ifelse(foot_16$team==foot_16$home_team,as.character(foot_16$away_team),as.character(foot_16$home_team))

opp<- foot_16 %>%
      filter(name %in% top_player_data$name) %>%
      group_by(name,opposition) %>%
      summarise(points=mean(points)) 

