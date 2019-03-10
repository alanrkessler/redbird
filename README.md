
# redbird
*Fantasy Baseball Draft Helper using Shiny*

### Background

Using a cheat sheet or draft list for fantasy baseball is helpful but it is hard to make the list current during the draft. The goal of this tool is to quickly be able to prioritize players and keep up with what is going on.

### Importing Data

-   Data is from the [Fangraphs Auction Calculator](http://www.fangraphs.com/auctiontool.aspx).

-   Pitchers and Batters data is downloaded as `.CSV`s and saved in the `data` directory.

### Draft Board

-   The data table displays all of the available players in the draft.

-   The drop-down menu filters the table to show players eligible for that position and/or team.

-   As players are drafted, type each player's name into the text box and press delete to remove that player from the list. Names make use of auto-complete shown in the image below.

-   It is possible to adjust the dollar figures by a multiple applied to pitchers.
