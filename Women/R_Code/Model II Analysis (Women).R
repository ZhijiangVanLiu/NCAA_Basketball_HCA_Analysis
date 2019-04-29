# Generate the beta's from Model 2 and their rank (Women)
# Original Date: 6/28/2017
# Version Date: 5/10/2018

# Update:
# 5/10/2018 - Create Design Matrix that apply zero constraint on coefficients before
#             fitting linear model.
# 5/11/2018 - Remove the games after the starting date of NCAA tournament.

#--------------------------------------------------------------------------------
# if at home
Home_directory <- paste0("F:/Research Assistant/Prof. Zimmerman/",
                         "Sport Statistics (College W&M)/data/Women/")
# if on campus
Home_directory <- paste0("H:/Documents/Research Assistant/Prof. Zimmerman/",
                         "Sport Statistics (College W&M)/data/Women/")


#--------------------------------------------------------------------------------
# Run Model II regression to generate beta and their ranks by season

WBB_tab <- read.table(paste0(Home_directory, "WBB_tab.txt"), stringsAsFactors = F)
WBB_tab$Date <- as.Date(WBB_tab$Date, format = "%Y-%m-%d")
dim(WBB_tab) # [1] 86806     7
str(WBB_tab)
head(WBB_tab)
tail(WBB_tab)

# Table of starting date of NCAA tournament by season
NCAA_date_tab <- read.csv(paste0(Home_directory, "tournament_start_dateW.csv"), 
                          stringsAsFactors = F)
str(NCAA_date_tab)
NCAA_date_tab$Date <- as.Date(NCAA_date_tab$Date, format = "%Y-%m-%d")

out_tab <- data.frame()
for(i in 1:17){
    Season <- paste0(2000+i,"-",2001+i)
    
    anal_tab <- WBB_tab[WBB_tab$Season==Season,]
    # dim(anal_tab) # [1] 4615  7
    
    # Remove the games after the starting date of NCAA tournament
    NCAA_date <- NCAA_date_tab$Date[NCAA_date_tab$Season == Season]
    anal_tab <- anal_tab[anal_tab$Date < NCAA_date,]
    
    # Append PT_diff and Non_Neutral variables
    anal_tab$Non_Neutral <- 1 - anal_tab$Neutral
    anal_tab$PT_diff <- anal_tab$PT_H - anal_tab$PT_A
    
    # Regression Model
    anal_tab$Home <- as.factor(anal_tab$Home)
    anal_tab$Away <- as.factor(anal_tab$Away)
    Design_Mat <- with(anal_tab, model.matrix(~Home-1))
    Design_Mat <- Design_Mat - with(anal_tab, model.matrix(~Away-1))
    colnames(Design_Mat) <- substring(colnames(Design_Mat), 5)
    # dim(Design_Mat) # [1] 4615  353
    # table(rowSums(Design_Mat))
    
    # Code Design_Mat so that sum of coefficients are zero
    p <- ncol(Design_Mat)
    Design_Mat <- Design_Mat%*%rbind(diag(1,p-1),-1)
    colnames(Design_Mat) <- levels(anal_tab$Home)[-p]
    # Design_Mat[1:10,1:10]
    ml2 <- lm(anal_tab$PT_diff ~ anal_tab$Non_Neutral + Design_Mat - 1)
    out_tab_temp <- data.frame(Season = Season, Team = levels(anal_tab$Home),
                               beta = as.numeric(c(coef(ml2)[-1],-sum(na.omit(coef(ml2))[-1]))), 
                               HCA = coef(ml2)[1],
                               row.names = NULL)
    # dim(out_tab_temp) # [1] 321   4
    out_tab_temp$Rank_beta <- rank(-out_tab_temp$beta)
    out_tab_temp <- out_tab_temp[order(out_tab_temp$beta, decreasing = T),]
    # dim(out_tab_temp) # [1] 321   5
    out_tab <- rbind(out_tab, out_tab_temp)
    row.names(out_tab) <- NULL
}


dim(out_tab) # [1] 5759    5

write.csv(out_tab, paste0(Home_directory, "out_tab_WBB_2018.csv"))















