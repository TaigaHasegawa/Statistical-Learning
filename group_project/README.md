---
title: "Applying Machine Learning to 2018 NCAA March Madness"
author: "Team Scorpion"
date: "3/24/2019"
output:
  pdf_document: default
  html_document: default
---

> **Team members**
June Kim (jkim654), Leo Franco Soto (francst2), Hoyin Lau (hlau4), Taiga Hasegawa(taigah2), Xiaomei Sun (xsun56)

> **Data Description**
The data is from Kaggle machine learning competition hosted by Google Cloud and NCAA 2018. It is a historical data collecting from year 1985 to 2018 (i.e. season 2017-2018, since this year's season is 2018-2019). The main goal of the project is to forecast the top 10 teams of March Madness during year 2018 for NCAA Division I Men Basketball Championship. Along the analysis, we can see which conferences have produced most championships since 1985 as well. We can validate our predictions with already existing result from season 2017-2018. 

For this project, we _can_ implement several models:

* K-Nearest Neighbors

* Random Forest

* SVM

The analysis can be accomplished by using various packages in R. Some of the examples would be: `dplyr` and `caret`. 

[Source](https://www.kaggle.com/c/mens-machine-learning-competition-2018/data)

*Variable Description*

(1) **PlayerName** - this is a text representation of the player's full name, in the format LAST_FIRST, with underscores substituted in for spaces.

(2) **CoachName** - this is a text representation of the coach's full name, in the format first_last, with underscores substituted in for spaces.

(3) **ConfAbbrev** - this is a short abbreviation for each conference; the abbreviation is used in some other files to indicate the parent conference of a team or of a conference tournament.

(4) **Description** - this is a longer text name for the conference.

(5) **ConfAbbrev** - this identifies the conference (as described in Conferences.csv).

(6) **Slot** - this uniquely identifies one of the tournament games. For play-in games, it is a three-character string identifying the seed fulfilled by the winning team, such as W16 or Z13. For regular tournament games, it is a four-character string, where the first two characters tell you which round the game is (R1, R2, R3, R4, R5, or R6) and the second two characters tell you the expected seed of the favored team. Thus the first row is R1W1, identifying the Round 1 game played in the W bracket, where the favored team is the 1 seed. As a further example, the R2W1 slot indicates the Round 2 game that would have the 1 seed from the W bracket, assuming that all favored teams have won up to that point. The slot names are different for the final two rounds, where R5WX identifies the national semifinal game between the winners of regions W and X, and R5YZ identifies the national semifinal game between the winners of regions Y and Z, and R6CH identifies the championship game. The "slot" value is used in other columns in order to represent the advancement and pairings of winners of previous games.

(7) **StrongSeed** - this indicates the expected stronger-seeded team that plays in this game. For Round 1 games, a team seed is identified in this column (as listed in the "Seed" column in the NCAATourneySeeds.csv file), whereas for subsequent games, a slot is identified in this column. In the first record of this file (slot R1W1), we see that seed W01 is the "StrongSeed", which during the 1985 tournament would have been Georgetown. Whereas for games from Round 2 or later, rather than a team seed, we will see a "slot" referenced in this column. So in the 33rd record of this file (slot R2W1), it tells us that the winners of slots R1W1 and R1W8 will face each other in Round 2. Of course, in the last few games of the tournament - the national semifinals and finals - it's not really meaningful to talk about a "strong seed" or "weak seed", since you would have #1 seeds favored to face each other, but those games are nevertheless represented in the same format for the sake of consistency.

(8) **WeakSeed** - this indicates the expected weaker-seeded team that plays in this game, assuming all favored teams have won so far. For Round 1 games, a team seed is identified in this column (as listed in the "Seed" column in the TourneySeeds.csv file), whereas for subsequent games, a slot is identified in this column.

(9) **TeamID** - A 4 digit id number, from 1000-1999, uniquely identifying each NCAA® men's team. (A school's TeamID does not change from one year to the next, so for instance the Duke men's TeamID is 1181 for all seasons.)

(10) **Team Name** - A compact spelling of the team's college name, 16 characters or fewer. There are no commas or double-quotes in the team names, but you will see some characters that are not letters or spaces, (e.g., Texas A&M, St Mary's CA, TAM C. Christi, and Bethune-Cookman).

(11) **FirstD1Season** - The first season in our dataset that the school was a Division-I school. (For instance, FL Gulf Coast (famously) was not a Division-I school until the 2008 season, despite their two wins just five years later in the 2013 NCAA® tourney. Of course, many schools were Division-I far earlier than 1985, but since we don't have any data included prior to 1985, all such teams are listed with a FirstD1Season of 1985.)

(12) **LastD1Season** - The last season in our dataset that the school was a Division-I school. For any teams that are currently Division-I, they will be listed with LastD1Season=2018, and you can confirm there are 351 such teams. It has been a few years since any teams stopped being Division-I; the last was Centenary whose final Division-I year was 2011.

(13) **Season** - Indicates the year in which the tournament was played.

(14) **DayZero** - the date corresponding to daynum=0 during that season. All game dates have been aligned upon a common scale so that the championship game of the final tournament is on daynum=154. Working backward, the national semifinals are always on daynum=152, the "play-in" games are on days 134/135, Selection Sunday is on day 132, and so on. All game data includes the day number in order to make it easier to perform date calculations. If you really want to know the exact date a game was played on, you can combine the game's "daynum" with the season's "dayzero". For instance, since day zero during the 2011-2012 season was 10/31/2011, if we know that the earliest regular season games that year were played on daynum=7, they were therefore played on 11/07/2011.

_By convention, the four regions in the final tournament are always named W, X, Y, and Z. Whichever region's name comes first alphabetically, that region will be Region W. And whichever Region plays against Region W in the national semifinals, that will be Region X. For the other two regions, whichever region's name comes first alphabetically, that region will be Region Y, and the other will be Region Z. For example, During the 2012 tournament, the four regions were East, Midwest, South, and West. Being the first alphabetically, East becomes W. Since the East regional champion (Ohio State) played against the Midwest regional champion (Kansas) in the national semifinals, that makes Midwest be region X. For the other two (South and West), since South comes first alphabetically, that makes South Y and therefore West is Z. So for that season, the W/X/Y/Z are East,Midwest,South,West._

(15) **Region W **
(16) **Region X**

(17) **Region Y**

(18) **Region Z**

(19) **Seed** - This is a 3/4-character identifier of the seed, where the first character is either W, X, Y, or Z (identifying the region the team was in) and the next two digits (either 01, 02, ..., 15, or 16) tells you the seed within the region. For play-in teams, there is a fourth character (a or b) to further distinguish the seeds, since teams that face each other in the play-in games will have seeds with the same first three characters. For example, the first record in the file is seed W01, which means we are looking at the #1 seed in the W region (which we can see from the "Seasons.csv" file was the East region).

(20) **WTeamID** - this identifies the id number of the team that won the game, as listed in the "Teams.csv" file. No matter whether the game was won by the home team or visiting team, or if it was a neutral-site game, the "WTeamID" always identifies the winning team.

(21) **WScore** - this identifies the number of points scored by the winning team.

(22) **LTeamID** - this identifies the id number of the team that lost the game.

(23) **LScore** - this identifies the number of points scored by the losing team. Thus you can be confident that WScore will be greater than LScore for all games listed.

(24) **NumOT** - this indicates the number of overtime periods in the game, an integer 0 or higher.

(25) **WLoc** - this identifies the "location" of the winning team. If the winning team was the home team, this value will be "H". If the winning team was the visiting team, this value will be "A". If it was played on a neutral court, then this value will be "N". Sometimes it is unclear whether the site should be considered neutral, since it is near one team's home court, or even on their court during a tournament, but for this determination we have simply used the Kenneth Massey data in its current state, where the "@" sign is either listed with the winning team, the losing team, or neither team. If you would like to investigate this factor more closely, we invite you to explore Data Section 3, which provides the city that each game was played in, irrespective of whether it was considered to be a neutral site.

(26) **DayNum** - 134 or 135 (Tue/Wed) - play-in games to get the tournament field down to the final 64 teams, 136 or 137 (Thu/Fri) - Round 1, to bring the tournament field from 64 teams to 32 teams, 138 or 139 (Sat/Sun) - Round 2, to bring the tournament field from 32 teams to 16 teams, 143 or 144 (Thu/Fri) - Round 3, otherwise known as "Sweet Sixteen", to bring the tournament field from 16 teams to 8 teams, 145 or 146 (Sat/Sun) - Round 4, otherwise known as "Elite Eight" or "regional finals", to bring the tournament field from 8 teams to 4 teams, 152 (Sat) - Round 5, otherwise known as "Final Four" or "national semifinals", to bring the tournament field from 4 teams to 2 teams, 154 (Mon) - Round 6, otherwise known as "national final" or "national championship", to bring the tournament field from 2 teams to 1 champion team

(27) **ID** - this is a 14-character string of the format SSSS_XXXX_YYYY, where SSSS is the four digit season number, XXXX is the four-digit TeamID of the lower-ID team, and YYYY is the four-digit TeamID of the higher-ID team

(28) **Pred** - this contains the predicted winning percentage for the first team identified in the ID field

(29) **WFGM** - field goals made (by the winning team)

(30) **WFGA** - field goals attempted (by the winning team)

(31) **WFGM3** - three pointers made (by the winning team)

(32) **WFGA3** - three pointers attempted (by the winning team)
    
(33) **WFTM** - free throws made (by the winning team)
    
(34) **WFTA** - free throws attempted (by the winning team)

(35) **WOR** - offensive rebounds (pulled by the winning team)
    
(36) **WDR** - defensive rebounds (pulled by the winning team)

(37) **WAst** - assists (by the winning team)

(38)**WTO** - turnovers committed (by the winning team)

(39) **WStl** - steals (accomplished by the winning team)

(40) **WBlk** - blocks (accomplished by the winning team)

(41) **WPF** - personal fouls committed (by the winning team)

(42) **CityID** - a four-digit ID number uniquely identifying a city.

(43) **City** - the text name of the city.

(44) **State** - the state abbreviation of the state that the city is in. In a few rare cases, the game location is not inside one of the 50 U.S. states and so other abbreviations are used, for instance Cancun, Mexico has a state abbreviation of MX.

(45) **RankingDayNum** - this integer always ranges from 0 to 133, and is expressed in the same terms as a game's DayNum (where DayZero is found in the Seasons.csv file). The RankingDayNum is intended to tell you the first day that it is appropriate to use the rankings for predicting games. For example, if RankingDayNum is 110, then the rankings ought to be based upon game outcomes up through DayNum=109, and so you can use the rankings to make predictions of games on DayNum=110 or later. The final pre-tournament rankings each year have a RankingDayNum of 133, and can thus be used to make predictions of the games from the NCAA® tournament, which start on DayNum=134 (the Tuesday after Selection Sunday).

(46) **SystemName** - this is the (usually) 3-letter abbreviation for each distinct ranking system. These systems may evolve from year to year, but as a general rule they retain their meaning across the years. Near the top of the Massey composite page, you can find slightly longer labels describing each system, along with links to the underlying pages where the latest rankings are provided (and sometimes the calculation is described).

(47) **OrdinalRank** - this is the overall ranking of the team in the underlying system. Most systems provide a complete ranking from #1 through #351 (currently), but sometimes there are ties and sometimes only a smaller set of rankings is provided, as with the AP's top 25.

(48) **EventID** - this is a unique ID for each logged event. The EventID's are different within each year, as are the PlayerID's. The events are sorted in approximate chronological order within each game, based on clock time, although when multiple events happen within the same clock time, the tiebreak for sorting is just the text EventType, so in some cases it may be impossible to determine the exact sequence of several tip-in attempts or free-throw-attempts that all happened at the same clock time.

(49) **Season, DayNum, WTeamID, LTeamID** - these four columns are sufficient to uniquely identify each game. The games are a mix of Regular Season, NCAA® Tourney, and Secondary Tourney games.

(50) **WPoints, LPoints** - whenever a scoring play happens (1 point, 2 points, or 3 points) the updated score is provided (from the perspective of the winning team (WPoints) and the losing team (LPoints), although of course during the game we didn't know yet that they were the winning team or losing team.

(51) **ElapsedSeconds**- this is the number of seconds that have elapsed from the start of the game until the event occurred. With a 20-minue half, that means that an ElapsedSeconds value from 0 to 1200 represents an event in the first half, a value from 1200 to 2400 represents and event in the second half, and a value above 2400 represents an event in overtime.

(51) **EventTeamID** - this is the ID of the team that the event is logged for, which will either be the WTeamID or the LTeamID.

(52) **EventPlayerID** - this is the ID of the player that the event is logged for, as described in the corresponding Players file.

_EventType_ - this is the type of the event that was logged (see listing below).
**Event Types:**
(53) **assist** - an assist was credited on a made shot

(54) **block** - a blocked shot was recorded

(55) **steal** - a steal was recorded
        
(56) **turnover** - a turnover was recorded

(57) **timeout, timeout_tv** - a regular timeout or TV timeout was called

(58) **foul_pers, foul_tech** - a personal foul or technical foul was committed

(59) **reb_off, reb_def, reb_dead** - an offensive rebound, defensive rebound, or dead-ball rebound was recorded

(60) **sub_in, sub_out** - a player entered or exited the game via a substitution

(61) **made1_free, miss1_free** - a one-point free throw was made or missed

(62) **made2_dunk, miss2_dunk** - a two-point field goal (dunk) was made or missed

(63) **made2_tip, miss2_tip** - a two-point field goal (tip-in) was         made or missed

(64) **made2_lay, miss2_lay** - a two-point field goal (layup) was made or missed

(65) **made2_jump, miss2_jump** - a two-point field goal (jump shot) was made or missed

(66) **made3_jump, miss3_jump** - a three-point field goal (assumed to be a jump shot) was made or missed

```{r}
  suppressMessages(library(dplyr))
  suppressMessages(library(curl))
  suppressMessages(library(data.table))
  suppressMessages(library(tidyr))
  suppressMessages(library(plyr))
```

> **Data Overview**
```{r}
  # read data
  
  teams <- fread('./ncaa/DataFiles/Teams.csv')
  seasons <- fread('./ncaa/DataFiles/Seasons.csv')
  seeds <- fread('./ncaa/DataFiles/NCAATourneySeeds.csv')
  season_results <- fread('./ncaa/DataFiles/RegularSeasonCompactResults.csv')
  tour_results <- fread('./ncaa/DataFiles/NCAATourneyCompactResults.csv')
  season_details <- fread('./ncaa/DataFiles/RegularSeasonDetailedResults.csv')
  tour_details <- fread('./ncaa/DataFiles/NCAATourneyDetailedResults.csv')
  conferences <- fread('./ncaa/DataFiles/Conferences.csv')
  team_conferences <- fread('./ncaa/DataFiles/TeamConferences.csv')
  coaches <- fread('./ncaa/DataFiles/TeamCoaches.csv')
  
  # data overview
  glimpse(teams)
  glimpse(seasons)
  glimpse(seeds)
  glimpse(season_results)
  glimpse(tour_results)
  glimpse(season_details)
  glimpse(conferences)
  glimpse(tour_details)
  glimpse(team_conferences)
  glimpse(coaches)
```
> **Challenges**
From this dataset, we will need to combine the dataset first, which will take some time. For now, we haven't combined the data into one for easy use. This is definitely a challenge, as it would take some time to understand each variable and how it is connected to each dataset. However, some of the variables connects each dataset, which we will be using them to connect datasets into single data. This week, we will be linking redundant variable to resolve this following issue.

Then, we need to make it clean for further analysis. We plan to use K-Nearest Neighbors to train the data then test with testing data, which would be the last row, data from 2018. We might also use random forest for the dataset, and then predict the accuracy of the random forest. We might also consider performing the Support Vector Machine to further classify our result.
