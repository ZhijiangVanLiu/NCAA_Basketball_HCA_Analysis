# Date: 4/23/2019

library(rvest)


###########################################################################

# Link for basketball scoring summary

link <- paste0('https://www.sports-reference.com/cbb/play-index/',
               'matchup_finder.cgi?request=1&year_min=1985&year_max=2019&school_id=&opp_id=&',
               'game_type=A&game_month=&game_location=&game_result=&is_overtime=&',
               'comp_school=le&comp_opp=le&rank_school=ANY&rank_opp=ANY&',
               'order_by=date_game&order_by_asc=Y&offset=') # In ascending order by date
offset <- 0 # offset to loop through all pages

z <- data.frame() 

# tracking how long the loop takes
system.time(
      # max 337800 - found through "173211 Wins, 164635 Losses, .513 W%" on website
    while(offset <= 337800) {
        dd <- html_session(paste0(link, format(offset, scientific = F))) %>%
          html_node("table#stats") %>%
          html_table()
        dd <- dd[dd$Date != 'Date',]
        row.names(dd) <- NULL
        z <- rbind(z, dd)
        
        # Save the partial data frame every 10,000 pages
        if(round(offset/10000)*10000 == offset){
            print(offset)
            write.csv(z, "Raw_MBB_1984_2019.csv")
        }
        
        offset <- offset + 100 # increase by 100 for each iteration
    
    }
)

dim(dd)
dd <- unique.data.frame(dd)
dim(dd)

row.names(dd) <- NULL
dd$Year <- as.factor(as.character(dd$Year))
table(dd$Year)

# Save output as csv file
write.csv(z, "Raw_MBB_1984_2019.csv")