# Generate ANOVA table from Model 2 vs Model 1 by season (Men)
# Original Date: 9/19/2017
# Version Date: 4/24/2019

# Update:
# 5/12/2018 - Create Design Matrix that apply zero constraint on coefficients before
#             fitting linear model. Update to latest 2017-2018 season. Remove the games 
#             after the starting date of NCAA tournament

#--------------------------------------------------------------------------------
# if at home
Home_directory <- paste0("F:/Research Assistant/Prof. Zimmerman/",
                         "Sport Statistics (College W&M)/data/Men/")
# if on campus
Home_directory <- paste0("H:/Documents/Research Assistant/Prof. Zimmerman/",
                         "Sport Statistics (College W&M)/data/Men/")


#--------------------------------------------------------------------------------
# Run Model II regression to generate ANOVA table

MBB_tab <- read.table(paste0(Home_directory, "MBB_Final_tab_2019.txt"), 
                      stringsAsFactors = F)
MBB_tab <- MBB_tab[order(MBB_tab$Date, MBB_tab$Home),]
MBB_tab$Date <- as.Date(MBB_tab$Date, format = "%Y-%m-%d")
MBB_tab <- data.frame(MBB_tab, row.names = NULL)
dim(MBB_tab) # [1] 163705    7
head(MBB_tab)
tail(MBB_tab)
str(MBB_tab)

# Table of starting date of NCAA tournament by season
NCAA_date_tab <- read.csv(paste0(Home_directory, "tournament_start_dateM.csv"), 
                          stringsAsFactors = F)
str(NCAA_date_tab)
NCAA_date_tab$Date <- as.Date(NCAA_date_tab$Date, format = "%m/%d/%Y")


out_tab <- data.frame()
for(i in -16:18){
    Season <- paste0(2000+i,"-",2001+i)
    
    anal_tab <- MBB_tab[MBB_tab$Season==Season,]
    # dim(anal_tab) # [1] 3838  7
    
    # Remove the games after the starting date of NCAA tournament
    NCAA_date <- NCAA_date_tab$Date[NCAA_date_tab$Season == Season]
    anal_tab <- anal_tab[anal_tab$Date < NCAA_date,]
    
    # Append PT_diff and Non_Neutral variables
    anal_tab$Non_Neutral <- 1 - anal_tab$Neutral
    anal_tab$PT_diff <- anal_tab$PT_H - anal_tab$PT_A
    
    # Regression Model
    n <- nrow(anal_tab)
    Home_Away_Factor <- as.factor(c(anal_tab$Home, anal_tab$Away))
    anal_tab$Home <- Home_Away_Factor[1:n]
    anal_tab$Away <- Home_Away_Factor[(n+1):(2*n)]
    Design_Mat <- with(anal_tab, model.matrix(~Home-1))
    Design_Mat <- Design_Mat - with(anal_tab, model.matrix(~Away-1))
    colnames(Design_Mat) <- substring(colnames(Design_Mat), 5)
    # table(rowSums(Design_Mat))
    # dim(Design_Mat) # [1] 3838  363
    
    # Code Design_Mat so that sum of coefficients are zero
    p <- ncol(Design_Mat)
    Design_Mat <- Design_Mat%*%rbind(diag(1,p-1),-1)
    colnames(Design_Mat) <- levels(Home_Away_Factor)[-p]
    # Design_Mat[1:10,1:10]
    ml2 <- lm(anal_tab$PT_diff ~ anal_tab$Non_Neutral + Design_Mat - 1)
    anova_out <- anova(ml2)
    out_tab_temp <- data.frame(SS = rep(" ", 4), df = rep(" ", 4), 
                               MS = rep(" ", 4), F_stat = rep(" ", 4), 
                               p_value = rep(" ", 4),
                               row.names = paste0("(", Season, ") ",
                                                  c("M1", "M2|M1", "Residual",
                                                    "Total")), 
                               stringsAsFactors = F)
    out_tab_temp[1:3,1] <- round(as.numeric(anova_out$`Sum Sq`[c(2,1,3)]),2)
    out_tab_temp[4,1] <- sum(as.numeric(out_tab_temp[1:3,1]))
    out_tab_temp[1:3,2] <- as.numeric(anova_out$Df[c(2,1,3)])
    out_tab_temp[4,2] <- sum(as.numeric(out_tab_temp[1:3,2]))
    out_tab_temp[1:3,3] <- round(as.numeric(anova_out$`Mean Sq`[c(2,1,3)]),2)
    out_tab_temp[1:2,4] <- round(anova_out$`F value`[c(2,1)],2)
    out_tab_temp[1:2,5] <- round(anova_out$`Pr(>F)`[c(2,1)],4)
    # dim(out_tab_temp) # [1] 329   5
    out_tab <- rbind(out_tab, out_tab_temp)
}


dim(out_tab) # [1] 140    5

write.csv(out_tab, paste0(Home_directory, "ANOVA_tab_MBB_2019.csv"))















