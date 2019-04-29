# NCAA_Basketball_HCA_Analysis
R programs and results from studying the home-court advantage on NCAA Men's and Women's basketball games

**Author**: Zhijiang (Van) Liu

**Update Date**: 4/24/2019

**Data source**
* http://www.sports-reference.com/cbb/ (Men's)
* https://github.com/octonion/basketball-w (Women's)

**Description**

Estimated home-court advantage and team strength parameters by season, using Model II from *Harville and Smith, 1994*, *"The Home-Court Advantage: How Large Is It, and Does It Vary from Team to Team?"*. The model is applied to both Men's and Women's games, but limited to Division I teams and regular season games. The most updated results include Men's model output from 1984-1985 to 2018-2019 season and Women's model output from 2001-2002 to 2017-2018 season.

| Name of Output File | Description | Output from |
| ------------- |:-------------:|:-------------:|
| Report_190424.pdf | A summary of home-court advantage yearly trend, Division I team counts and Top 10 teams with the most Top 10 strength measure in a season | |

**/Men/Dataset**

| Name of Dataset | Description | Output from |
| ------------- |:-------------:|:-------------:|
| Raw_MBB_1984_2019.zip | Compressed raw dataset scraped from http://www.sports-reference.com/cbb/ | web_scrape_cbb_2019.R |
| MBB_Final_tab_2019.txt | Dataset for Men's Basketball games formatted properly to fit Model II | Creating Master Data Set (Men, 2019).R |
| Division1_by_year_1984_2019.csv | List of Division I Men's schools, scraped from http://www.sports-reference.com/cbb/ | web_scrape_Div1_2019.R |
| tournament_start_dateM.csv | Start date of Men's NCAA tournament by season, used to retain just regular season games | | 

**/Men/Output**

| Name of Output File | Description | Output from |
| ------------- |:-------------:|:-------------:|
| out_tab_MBB_2019.csv | Team strength and home-court advantage parameters estimated by season for Men's NCAA basketball games | Model II Analysis (Men).R |
| ANOVA_tab_MBB_2019.csv | ANOVA tables of Model I vs. Model II by season for Men's NCAA basketball games | Model I,II ANOVA (Men).R |

**/Women/Dataset**

| Name of Dataset | Description | Output from |
| ------------- |:-------------:|:-------------:|
| WBB_tab.txt | Dataset for Women's Basketball games formatted properly to fit Model II | Creating Master Data Set (Women).R |
| tournament_start_dateW.csv | Start date of Women's NCAA tournament by season, used to retain just regular season games | | 

**/Women/Output**

| Name of Output File | Description | Output from |
| ------------- |:-------------:|:-------------:|
| out_tab_WBB_2018.csv | Team strength and home-court advantage parameters estimated by season for Women's NCAA basketball games | Model II Analysis (Women).R.
| ANOVA_tab_WBB_2018.csv | ANOVA tables of Model I vs. Model II by season for Women's NCAA basketball games | Model I,II ANOVA (Women).R |
