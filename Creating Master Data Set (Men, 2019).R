# Creating the master data set to rank the beta's from Model 2 (Men)
# Original Date: 4/23/2019

#--------------------------------------------------------------------------------
# if at home
Home_directory <- paste0("F:/Research Assistant/Prof. Zimmerman/",
                         "Sport Statistics (College W&M)/data/Men/")
# if on campus
Home_directory <- paste0("H:/Documents/Research Assistant/Prof. Zimmerman/",
                         "Sport Statistics (College W&M)/data/Men/")


###################################################################################
# (1984-2019) MBB data
###################################################################################

# 1. Import the MBB data from 1984-2019 data set
MBB_8419_tab_raw <- read.csv(paste0(Home_directory, "Raw_MBB_1984_2019.csv"), 
                             header = T, stringsAsFactors = F)
dim(MBB_8419_tab_raw) # [1] 337846     16
dim(unique(MBB_8419_tab_raw)) # [1] 337846     16
names(MBB_8419_tab_raw)
head(MBB_8419_tab_raw)
tail(MBB_8419_tab_raw)

# Check existence of NA in X.2 field
table(MBB_8419_tab_raw$X.2, useNA = 'always')
# Replace NA with space
MBB_8419_tab_raw$X.2[is.na(MBB_8419_tab_raw$X.2)] <- ''
table(MBB_8419_tab_raw$X.2, useNA = 'always')
MBB_8419_tab_raw$X.2 <- as.factor(as.character(MBB_8419_tab_raw$X.2))

MBB_8419_tab_raw <- MBB_8419_tab_raw[, -1]
MBB_8419_tab_raw <- MBB_8419_tab_raw[,c(2,1,6,4,7,11,12)]
head(MBB_8419_tab_raw)

# 2. Split the table into Away and Home/Neutral tables based on X.2 field
MBB_away_8419 <- MBB_8419_tab_raw[MBB_8419_tab_raw$X.2=="@",]
dim(MBB_away_8419) # [1] 146242      7
MBB_away_8419 <- data.frame(MBB_away_8419, Indicator = 1)
dim(MBB_away_8419) # [1] 146242      8
MBB_away_8419$Home <- MBB_away_8419$Opp
MBB_away_8419$Away <- MBB_away_8419$Schl
dim(MBB_away_8419) # [1] 146242     10
head(MBB_away_8419)

MBB_H_N_8419 <- MBB_8419_tab_raw[MBB_8419_tab_raw$X.2=="",]
MBB_H_N_8419$Home <- MBB_H_N_8419$Schl
MBB_H_N_8419$Away <- MBB_H_N_8419$Opp
dim(MBB_H_N_8419) # [1] 191604      9
head(MBB_H_N_8419)

dim(MBB_H_N_8419)[1] + dim(MBB_away_8419)[1] # [1] 337846

# 3. Merge the tables to create master table with neutrality indicator
Merge_tab <- merge(MBB_H_N_8419, MBB_away_8419, by = c("Date", "Home", "Away"),
                   all.x = T)
dim(Merge_tab) # [1] 191604     16
head(Merge_tab)

MBB_8419_tab <- Merge_tab[,c(4,1,16,5,2,3,8,9)]
dim(MBB_8419_tab) # [1] 191604      8
head(MBB_8419_tab)
MBB_8419_tab$Neutral <- 0
MBB_8419_tab$Neutral[is.na(MBB_8419_tab$Indicator)] <- 1
dim(MBB_8419_tab) # [1] 191604      9
head(MBB_8419_tab)
MBB_8419_tab <- MBB_8419_tab[,c(2,1,9,5,6,7,8),]
names(MBB_8419_tab) <- c("Date", "Season", "Neutral", 
                         "Home", "Away", "PT_H", "PT_A")
MBB_8419_tab$Season <- paste0(as.double(substring(MBB_8419_tab$Season,1,4)), "-",
                              as.double(substring(MBB_8419_tab$Season,1,4)) + 1)
dim(MBB_8419_tab) # [1] 191604      7
head(MBB_8419_tab)
table(MBB_8419_tab$Season, useNA = 'always')


# 4. Import the team list in Division 1 by Season
Div_1_MBB_List <- read.csv(paste0(Home_directory,"Division1_by_year_1984_2019.csv"), 
                           header = T, stringsAsFactors = F)
dim(Div_1_MBB_List) # [1] 11171     3
head(Div_1_MBB_List)
Div_1_MBB_List <- unique(Div_1_MBB_List[,2:3])
names(Div_1_MBB_List)[2] <- "Home"
Div_1_MBB_List$Away <- Div_1_MBB_List$Home
dim(Div_1_MBB_List) # [1] 11171     3
head(Div_1_MBB_List)
Div_1_MBB_List <- data.frame(Div_1_MBB_List, Ind = 1)
dim(Div_1_MBB_List) # [1] 11171     4
head(Div_1_MBB_List)

# Change the school names
# From: VMI, Fort Wayne, SIU Edwardsville
# To: Virginia Military Institute, Purdue-Fort Wayne, Southern Illinois-Edwardsville
Div_1_MBB_List$Home[Div_1_MBB_List$Home == 'VMI'] <- 'Virginia Military Institute'
Div_1_MBB_List$Away[Div_1_MBB_List$Away == 'VMI'] <- 'Virginia Military Institute'
Div_1_MBB_List$Home[Div_1_MBB_List$Home == 'Fort Wayne'] <- 'Purdue-Fort Wayne'
Div_1_MBB_List$Away[Div_1_MBB_List$Away == 'Fort Wayne'] <- 'Purdue-Fort Wayne'
Div_1_MBB_List$Home[Div_1_MBB_List$Home == 'SIU Edwardsville'] <- 'Southern Illinois-Edwardsville'
Div_1_MBB_List$Away[Div_1_MBB_List$Away == 'SIU Edwardsville'] <- 'Southern Illinois-Edwardsville'


# 5. choose the games with Home and Away teams in Division 1
dim(MBB_8419_tab) # [1] 191604      7
table(MBB_8419_tab$Season, useNA = 'always')
table(Div_1_MBB_List$Season, useNA = 'always')

MBB_8419_tab <- merge(x = MBB_8419_tab, y = Div_1_MBB_List, 
                      by = c("Season", "Home"), all.x = T)
dim(MBB_8419_tab) # [1] 191604      9
table(MBB_8419_tab$Season, useNA = 'always')
head(MBB_8419_tab)
MBB_8419_tab <- MBB_8419_tab[,-8]
names(MBB_8419_tab)[5] <- "Away"
head(MBB_8419_tab)
MBB_8419_tab <- merge(x = MBB_8419_tab, y = Div_1_MBB_List, 
                      by = c("Season", "Away"), all.x = T)
dim(MBB_8419_tab) # [1] 191604     10
head(MBB_8419_tab)
MBB_8419_tab <- MBB_8419_tab[,-9]
names(MBB_8419_tab)[3] <- "Home"
dim(MBB_8419_tab) # [1] 191604      9
head(MBB_8419_tab)
table(MBB_8419_tab[,c("Ind.x","Ind.y")], useNA = "always")
MBB_8419_tab <- MBB_8419_tab[!is.na(MBB_8419_tab$Ind.x),]
MBB_8419_tab <- MBB_8419_tab[!is.na(MBB_8419_tab$Ind.y),]
dim(MBB_8419_tab) # [1] 182331      9
head(MBB_8419_tab)
MBB_8419_tab <- MBB_8419_tab[,-(8:9)]
MBB_8419_tab <- MBB_8419_tab[,c(1,4,5,3,2,6,7)]

head(MBB_8419_tab)
dim(MBB_8419_tab) # [1] 182331      7

# 6. Handle duplicate neutral court games
MBB_8419_tab$Home <- as.character(MBB_8419_tab$Home)
MBB_8419_tab$Away <- as.character(MBB_8419_tab$Away)
# Indicator of neutral game and home team after away team alphebatically
SwitchIndVector <- 
    (MBB_8419_tab$Home > MBB_8419_tab$Away)&(MBB_8419_tab$Neutral == 1)
# Switch Home and Away for those games
HomeTempVector <- MBB_8419_tab$Away[SwitchIndVector]
MBB_8419_tab$Away[SwitchIndVector] <- MBB_8419_tab$Home[SwitchIndVector]
MBB_8419_tab$Home[SwitchIndVector] <- HomeTempVector
# Switch Home Points and Away Points for those games
PT_HTempVector <- MBB_8419_tab$PT_A[SwitchIndVector]
MBB_8419_tab$PT_A[SwitchIndVector] <- MBB_8419_tab$PT_H[SwitchIndVector]
MBB_8419_tab$PT_H[SwitchIndVector] <- PT_HTempVector
table(MBB_8419_tab$Neutral, useNA = 'always')
MBB_8419_tab <- unique.data.frame(MBB_8419_tab)
table(MBB_8419_tab$Neutral, useNA = 'always')
dim(MBB_8419_tab) # [1] 163705      7
head(MBB_8419_tab)
remove(SwitchIndVector, HomeTempVector, PT_HTempVector)

# 7. Check the number of teams by season
table(MBB_8419_tab$Season, useNA = 'always')
UniqueHomeTeam <- unique.data.frame(MBB_8419_tab[,c('Season', 'Home')])
dim(UniqueHomeTeam)
table(UniqueHomeTeam$Season, useNA = 'always')
UniqueAwayTeam <- unique.data.frame(MBB_8419_tab[,c('Season', 'Away')])
dim(UniqueAwayTeam)
table(UniqueAwayTeam$Season, useNA = 'always')

table(Div_1_MBB_List$Season, useNA = 'always')

table(Div_1_MBB_List$Season, useNA = 'always') -
    table(UniqueHomeTeam$Season, useNA = 'always')
table(Div_1_MBB_List$Season, useNA = 'always') -
    table(UniqueAwayTeam$Season, useNA = 'always')

# Spot check for missing teams
UniqueHomeTeam$Ind_MBB_tab <- 1
dim(UniqueHomeTeam) # [1] 11113     3
MergeHomeTeam <- merge(Div_1_MBB_List, UniqueHomeTeam, 
                       by = c("Season", "Home"), all.x = T)
dim(MergeHomeTeam) # [1] 11171     5
head(MergeHomeTeam)
MergeHomeTeam[is.na(MergeHomeTeam$Ind_MBB_tab),]

# Found three schools: VMI, Fort Wayne, SIU Edwardsville
# Virginia Military Institute, Purdue-Fort Wayne, Southern Illinois-Edwardsville
    
# 8. Save (1984-2018) MBB data table
str(MBB_8419_tab)
write.table(MBB_8419_tab, paste0(Home_directory, "MBB_Final_tab_2019.txt"))












