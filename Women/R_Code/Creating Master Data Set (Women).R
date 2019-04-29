# Creating the master data set to rank the beta's from Model 2 (Women)
# Original Date: 5/23/2017
# Version Date: 5/10/2018

# Update:
# 5/10/2018 - Add 2016-2017 and 2017-2018 seasons. Since some school names have changed in database
#             for 2017-2018 season, team id in "ncaa_divisions.csv" is used to translate old names 
#             to new ones. Don't create design matrix as part of dataset any more.

#--------------------------------------------------------------------------------
# if at home
Home_directory <- paste0("F:/Research Assistant/Prof. Zimmerman/",
                         "Sport Statistics (College W&M)/data/Women/")
# if on campus
Home_directory <- paste0("H:/Documents/Research Assistant/Prof. Zimmerman/",
                         "Sport Statistics (College W&M)/data/Women/")



#----------------------------------------------------------------------------------
# import WBB data

# 1. Pull the list of all WBB Division 1 teams, since 2001-2002 season
Div_1_WBB <- read.csv(paste0(Home_directory, "ncaa_divisions.csv"), header = F)
dim(Div_1_WBB)
head(Div_1_WBB)
# remove the fields that are not useful
Div_1_WBB <- Div_1_WBB[Div_1_WBB$V7==1,-c(1,4,5,9)]
dim(Div_1_WBB) # [1] 11407     6
head(Div_1_WBB)
str(Div_1_WBB)
table(Div_1_WBB$V10, useNA = "always")
Div_1_WBB <- as.data.frame(Div_1_WBB)
Div_1_WBB <- Div_1_WBB[,c(1,2,3,5)]
names(Div_1_WBB) <- c("team_name", "team_id", "year", "Season")
Div_1_WBB$opponent_id <- Div_1_WBB$team_id
Div_1_WBB$Season <- paste0(as.double(substring(Div_1_WBB$Season,1,4)), "-",
                           as.double(substring(Div_1_WBB$Season,1,4)) + 1)
head(Div_1_WBB)
dim(Div_1_WBB) # 11407     5
Div_1_WBB$Indicator <- 1

# save the list of teams as txt file
write.table(Div_1_WBB,"Div_1_WBB_list.txt", row.names = F)


# 2. Import the games year by year, then choose those between two D1 teams
WBB_tab <- data.frame()
for(i in 2:18){
    
    WBB_tab <- rbind(WBB_tab, 
                     read.csv(paste0(Home_directory, "ncaa_games_", 2000+i, ".csv")))
    
}

WBB_tab_raw <- na.omit(WBB_tab) # opponent_id field is NA for Non-Division 1 teams
WBB_tab <- WBB_tab_raw
dim(WBB_tab) # [1] 481147     12
head(WBB_tab)
tail(WBB_tab)

# Replace the team_name and opponent_name
WBB_tab$team_name <- Div_1_WBB$team_name[match(WBB_tab$team_id, 
                                               Div_1_WBB$team_id)]
WBB_tab$opponent_name <- Div_1_WBB$team_name[match(WBB_tab$opponent_id, 
                                                   Div_1_WBB$team_id)]
WBB_tab$team_name <- as.character(WBB_tab$team_name)
WBB_tab$opponent_name <- as.character(WBB_tab$opponent_name)
WBB_tab <- WBB_tab[!is.na(WBB_tab$team_name),]
WBB_tab <- WBB_tab[!is.na(WBB_tab$opponent_name),]
dim(WBB_tab) # [1] 175177     12
head(WBB_tab)
tail(WBB_tab)

# Check the number of unique teams
WBB_tab <- WBB_tab[WBB_tab$opponent_id!=788,]
dim(WBB_tab) # [1] 175173     12
length(unique(WBB_tab$team_name)) # [1] 364
length(unique(WBB_tab$opponent_name)) # [1] 364
length(unique(WBB_tab$team_id)) # [1] 364
length(unique(WBB_tab$opponent_id)) # [1] 364

# choose the Division 1 teams
WBB_tab <- merge(x = WBB_tab, y = Div_1_WBB[,c("team_id","year","Indicator")], 
                 by = c("team_id","year"), all.x = T)
dim(WBB_tab) # [1] 175173     13
WBB_tab <- merge(x = WBB_tab, y = Div_1_WBB[,c("opponent_id","year","Indicator")], 
                 by = c("opponent_id","year"), all.x = T)
dim(WBB_tab) # [1] 175173     14
table(WBB_tab[,c("Indicator.x", "Indicator.y")], useNA = "always")
WBB_tab <- WBB_tab[!is.na(WBB_tab$Indicator.x),]
WBB_tab <- WBB_tab[!is.na(WBB_tab$Indicator.y),]
dim(WBB_tab) # [1] 173610     14
head(WBB_tab)
tail(WBB_tab)
table(is.na(WBB_tab[,c("opponent_id")]))
WBB_tab <- WBB_tab[,c(2,4:9)]
dim(WBB_tab) # [1] 173610     7
head(WBB_tab)
tail(WBB_tab)

# Check the number of unique teams
length(unique(WBB_tab$team_name)) # [1] 353
length(unique(WBB_tab$opponent_name)) # [1] 353

# Save the raw data
write.table(WBB_tab, paste0(Home_directory,"WBB_tab_raw.txt"))

WBB_tab_raw <- read.table(paste0(Home_directory,"WBB_tab_raw.txt"))
head(WBB_tab_raw)

# 3. reorganize the raw table and remove duplicate games
WBB_tab_raw$team_name <- as.character(WBB_tab_raw$team_name)
WBB_tab_raw$opponent_name <- as.character(WBB_tab_raw$opponent_name)
WBB_tab_raw$location <- as.character(WBB_tab_raw$location)
WBB_tab_raw$game_date <- as.Date(WBB_tab_raw$game_date, format = "%m/%d/%Y")
str(WBB_tab_raw)
WBB_tab <- data.frame(Date = WBB_tab_raw$game_date)

# Append Home and Away team names
WBB_tab$Home <- WBB_tab_raw$team_name
WBB_tab$Home[WBB_tab_raw$location=="Away"] <- 
    WBB_tab_raw$opponent_name[WBB_tab_raw$location=="Away"]
WBB_tab$Home[WBB_tab_raw$location=="Neutral"&
                 (WBB_tab_raw$team_name>WBB_tab_raw$opponent_name)] <- 
    WBB_tab_raw$opponent_name[WBB_tab_raw$location=="Neutral"&
                                  (WBB_tab_raw$team_name>WBB_tab_raw$opponent_name)]

WBB_tab$Away <- WBB_tab_raw$opponent_name
WBB_tab$Away[WBB_tab_raw$location=="Away"] <- 
    WBB_tab_raw$team_name[WBB_tab_raw$location=="Away"]
WBB_tab$Away[WBB_tab_raw$location=="Neutral"&
                 (WBB_tab_raw$team_name>WBB_tab_raw$opponent_name)] <- 
    WBB_tab_raw$team_name[WBB_tab_raw$location=="Neutral"&
                                  (WBB_tab_raw$team_name>WBB_tab_raw$opponent_name)]
dim(WBB_tab) # [1] 173610      3
head(WBB_tab)

# Append Neutral indicator
WBB_tab$Neutral <- as.double(WBB_tab_raw$location=="Neutral")
dim(WBB_tab) # [1] 173610      4
head(WBB_tab)
tail(WBB_tab)

# Append Home and Away Points
WBB_tab$PT_H <- WBB_tab_raw$team_score
WBB_tab$PT_H[WBB_tab_raw$location=="Away"] <- 
    WBB_tab_raw$opponent_score[WBB_tab_raw$location=="Away"]
WBB_tab$PT_H[WBB_tab_raw$location=="Neutral"&
                 (WBB_tab_raw$team_name>WBB_tab_raw$opponent_name)] <- 
    WBB_tab_raw$opponent_score[WBB_tab_raw$location=="Neutral"&
                                  (WBB_tab_raw$team_name>WBB_tab_raw$opponent_name)]

WBB_tab$PT_A <- WBB_tab_raw$opponent_score
WBB_tab$PT_A[WBB_tab_raw$location=="Away"] <- 
    WBB_tab_raw$team_score[WBB_tab_raw$location=="Away"]
WBB_tab$PT_A[WBB_tab_raw$location=="Neutral"&
                 (WBB_tab_raw$team_name>WBB_tab_raw$opponent_name)] <- 
    WBB_tab_raw$team_score[WBB_tab_raw$location=="Neutral"&
                              (WBB_tab_raw$team_name>WBB_tab_raw$opponent_name)]
dim(WBB_tab) # [1] 173610      6
head(WBB_tab)

# Append Season
WBB_tab$Season <- paste0(WBB_tab_raw$year-1,"-",WBB_tab_raw$year)

# Rearrange the fields and remove duplicates
WBB_tab <- WBB_tab[,c(1,7,4,2,3,5,6)]
WBB_tab <- unique(WBB_tab)
dim(WBB_tab) # [1] 86806     7
head(WBB_tab)

# # Append the Design Matrix for regression
# length(unique(WBB_tab$Home))==length(unique(WBB_tab$Away))
# Design_Mat <- with(WBB_tab, model.matrix(~Home-1))
# colnames(Design_Mat) <- substring(colnames(Design_Mat), 5)
# Design_Mat[1:10,1:10]
# Design_Mat <- Design_Mat - with(WBB_tab, model.matrix(~Away-1))
# Design_Mat[1:10,1:10]
# dim(Design_Mat) # [1] 76171   353
# WBB_tab <- cbind(WBB_tab, Design_Mat)

# Save the data set
write.table(WBB_tab, paste0(Home_directory, "WBB_tab.txt"))




