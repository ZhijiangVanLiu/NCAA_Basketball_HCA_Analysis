# Date: 4/23/2019

library(rvest)


###########################################################################
# Data frame of all MBB Division 1 ratings by season

z <- data.frame() 

# tracking how long the loop takes
system.time(
    for(year in 1985:2019) {
        dd <- html_session(paste0('https://www.sports-reference.com/cbb/seasons/',
                                  year, # Decide which season
                                  '-ratings.html')) %>%
          html_node("#ratings") %>%
          html_table()
        
        # Remove all columns except schools, append season column
        dd <- data.frame(Season = paste0(year-1,'-',year), School = dd[,2])
        dd <- dd[!dd$School%in%c('School', ''), ]
        dd <- dd[order(dd$School),]
        row.names(dd) <- NULL
        
        z <- rbind(z, dd)

    }
)

dim(z) # [1] 11171     2
head(z)

# Remove invalid rows
z <- z[!z$School%in%c('School', ''), ]
z$School <- as.factor(as.character(z$School))
dim(z) # [1] 11171     2
head(z)

# Remove duplicates
z <- unique.data.frame(z)
dim(z) # [1] 11171     2
head(z)
row.names(z) <- NULL

table(z$Season)

# Save output as csv file
write.csv(z, "Division1_by_year_1984_2019.csv")
