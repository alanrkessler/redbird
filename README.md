### Link to the App

-   The app is hosted on [shinyapps.io](https://alanrkessler.shinyapps.io/redbird/)

### Importing Data

-   Data is from the [Fangraphs Auction Calculator](http://www.fangraphs.com/auctiontool.aspx).

-   Pitchers and Batters data is downloaded as `.CSV`s and saved in the `data` folder.

### Draft Board

-   The data table displays all of the available players in the draft.

-   The drop-down menu filters the table to show players eligible for that position.

-   As players are drafted, type each player's name into the text box and press delete. Enter one player at a time.

-   Refresh the application to reset the table.

### Player Tiers

-   The first table shows the number of players by tier and position for available players.

-   The second table shows the number of players by tier and position for all players in the draft regardless of availability.

-   Players are double counted if they are eligible for more than one position.

-   Tiers are set up based on the following rules:
    -   Tier 5 are players with fewer PA/IP than a set threshold for each position.
    -   Tier 4 are players with a rank worse than the sum of all starting spots on all teams.
    -   Tiers 1-3 are set based on k-means clustering using the dollar amount.

### Plots

-   The dollar values for each player are plotted on the y-axis.

-   The user can select whether to plot position rank or overall rank on the x-axis. The rank is determined on the order of the original data.

-   The user can select whether to plot players that are no longer available.

-   The slider below the plot adjusts the x-axis.