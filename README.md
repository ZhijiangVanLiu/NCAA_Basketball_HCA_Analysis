# NCAA_Basketball_HCA_Analysis
R programs and results from studying the home-court advantage on NCAA Men's and Women's basketball games

Author: Zhijiang (Van) Liu

Update Date: 4/24/2019

Data source
..* http://www.sports-reference.com/cbb/ (Men's)
..* https://github.com/octonion/basketball-w (Women's)

Description

Estimated home-court advantage and team strength parameters by season, using Model II from Harville and Smith, 1994, "The Home-Court Advantage: How Large Is It, and Does It Vary from Team to Team?". The model is applied to both Men's and Women's games, but limited to Division 1 teams and regular season games. The most updated results include Men's model output from 1984-1985 to 2018-2019 season and Women's model output from 2001-2002 to 2017-2018 season.

Datasets

    Raw_1984_2010.csv: raw dataset scraped from http://www.sports-reference.com/cbb/ by using web_scrape_cbb_2019.R.

    MBB_Final_tab_2019.txt: dataset formatted for analysis by using Creating Master Data Set (Men, 2019).R.

    Division1_by_year_1984_2019.csv: list of Division 1 Men's schools, scraped from http://www.sports-reference.com/cbb/ by using web_scrape_Div1_2019.R.

Outputs

    out_tab_MBB_2019.csv / out_tab_WBB_2018.csv: team strength and home-court advantage parameters estimated by season for Men's/Women's NCAA basketball games, by using Model II Analysis (Men).R / Model II Analysis (Women).R.

    ANOVA_tab_MBB_2019.csv / ANOVA_tab_WBB_2018.csv: ANOVA tables of Model I vs. Model II by season for Men's/Women's NCAA basketball games, by using Model I,II ANOVA (Men).R / Model I,II ANOVA (Women).R.

    Report_190424.pdf: a summary of home-court advantage yearly trend, Division I team counts and Top 10 teams with the most Top 10 strength measure in a season.
