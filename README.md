redbird, a Fantasy Baseball Draft App in Shiny
----------------------------------------------

#### Background

For the last couple of years, I have been in a fantasy baseball league with around 18 teams with head-to-head match-ups. The large number of teams results in an unusual roster set-up, making typical rankings on sites like Yahoo less useful. The draft is also very fast paced as to accomodate all of the teams.

I needed a draft cheat sheet that reflected the unusual structure of the league and could be updated as players were drafted. The result is this application in Shiny, my first.

#### Link to the App

-   The app is hosted on [shinyapps.io](https://alanrkessler.shinyapps.io/redbird/)

-   The code can be found on [github](https://github.com/alanrkessler/redbird)

#### Limitations

The app uses input from [Fangraphs](http://www.fangraphs.com), so any limitations in their methodology would also apply here. That methodology accounts for the relative value of different positions and each players contribution to the fantasy team's tracked stats.

What the method lacks are real time adjustments to player value to reflect the players remianing on the board and how a given player will contribute the fantasy team based on the players already selected by that team.

For example, you have drafted almost all of your starting pitchers. You have one pitcher spot left open and you don't have a second baseman yet. While the draft sheet could indicate that another pitcher is better than the next available second baseman, another pitcher and a replacment level second baseman may not improve your team as much the best available second baseman and a lesser pitcher. This problem exists because the draft sheet doesn't know the context of the team as a whole.

Here is another example. Say you have drafted a team so far that is not going to steal many bases. For the next player you draft, should you select someone that can steal bases to boost that stat or give up on steals knowing that one player will not win you many match-ups? You could argue that giving up the stat could allow you to strenghten other stats. Again, the draft sheet does not know which players you have on your team and will not help in this determination.

In these cases, you need to use your judgment.

#### Importing Data

-   Data is from the [Fangraphs Auction Calculator](http://www.fangraphs.com/auctiontool.aspx).

-   Pitchers and Batters data is downloaded as `.CSV`s and saved in the `data` folder.

#### Draft Board

-   The data table displays all of the available players in the draft.

-   The drop-down menu filters the table to show players eligible for that position and/or team.

-   As players are drafted, type each player's name into the text box and press delete.

-   Refresh the application to reset the table.

<img src="sheet.png" width="800px" />

#### Player Tiers

-   The first table shows the number of players by tier and position for available players.

-   The second table shows the number of players by tier and position for all players in the draft regardless of availability.

-   Players are double counted if they are eligible for more than one position.

-   Tiers are set up based on the following rules:
    -   Tier 5 are players with fewer PA/IP than a set threshold for each position.
    -   Tier 4 are players with a rank worse than the sum of all starting spots on all teams.
    -   Tiers 1-3 are set based on k-means clustering using the dollar amount.

#### Plots

-   The dollar values for each player are plotted on the y-axis.

-   The user can select whether to plot position rank or overall rank on the x-axis. The rank is determined on the order of the original data.

-   The user can select whether to plot players that are no longer available.

-   The slider below the plot adjusts the x-axis.

<img src="plot.png" width="800px" />
