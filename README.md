## Author: Ethan Sterbis | [LinkedIn](https://www.linkedin.com/in/ethansterbis/) | [Twitter](https://x.com/EthanSterbis)

## Information:
NFL With/Without Analysis is a Shiny App I created to visualize how a certain NFL player performs
when selected teammates are present/absent in the same game. How much do a WR's fantasy points dip
without his injury-prone QB? How much does a pass-catching RB's target share differ when a star
TE is injured? This app aims to provide answers to these types of questions.

The app outputs all weeks in which the main player played in the selected season with the selected team filtered
by included and excluded players. The 'SAMPLE AVG' row shows the averages of all weeks displayed in the table
(accounts for included/excluded players). The 'OVERALL AVG' row shows the averages of all weeks in which the
main player played in the selected season with the selected team (doesn't account for included/excluded players).
The 'PERCENT OVER/UNDER' row is the percent difference between the 'SAMPLE AVG' row and the 'OVERALL AVG' row.

## Samples:

### 2023 Matthew Stafford with Puka Nacua, without Cooper Kupp
<img src="https://github.com/user-attachments/assets/776a6900-6293-4781-b465-4a27369e9d69" width="100%" alt="Matthew Stafford Example" />

In games with Puka Nacua and without Cooper Kupp, Matthew Stafford averaged 42.55 (306.75 - 264.20) more passing
yards per game compared to his season average. Stafford also had a 13.62% decrease in fantasy points per game, which is due to
the 53.12% decrease in total touchdowns.


<img src="https://github.com/user-attachments/assets/07561dda-fe9e-41f0-84cf-61077039447f" width="100%" alt="Matthew Stafford Example (cont)" />

The 'TEAM' section shows team metrics across the sample (accounting for included/excluded players) as well as overall metrics (all games in that season).
When Nacua is playing a game and Kupp is not, the Rams are more likely to favor rushing touchdowns
compared to passing touchdowns as seen by the table above. A reasonable conclusion would be that
when Cooper Kupp is playing, the Rams benefit more by utilizing their talented WRs to score passing
touchdowns. On the contrary, when Kupp is out and Nacua is in, the Rams lean more into their red zone
run game and heavier personnel groupings to score rushing touchdowns.


### 2022, 2023 Christian McCaffrey without Deebo Samuel
<img src="https://github.com/user-attachments/assets/a9569ee9-f881-4ad2-8357-108cfb74f140" width="100%" alt="Christian McCaffrey Example" />

As we can see, Christian McCaffrey scored an average of 30.32 fantasy points in games without
Deebo Samuel in 2022 and 2023. Across all games in 2022 and 2023, McCaffrey scored an average of
22.99 fantasy points, meaning then McCaffrey's fantasy points increase by 31.92% when Deebo
Samuel is not in the game.


### 2023 Justin Jefferson with Kirk Cousins
<img src="https://github.com/user-attachments/assets/67b100c3-057f-4578-b776-41667fe97aef" width="100%" alt="Justin Jefferson Example" />

In 2023, Justin Jefferson's average fantasy points per game was only 1.60 higher with Cousins compared
to his season average, which is surprising to most because one might think that Jefferson relies
on his QB to succeed.


<img src="https://github.com/user-attachments/assets/767f6804-29af-4bcc-87a6-e6b188476541" width="100%" alt="Justin Jefferson Example (cont)" />

Under the 'TEAM' section, we can see that in games with Kirk Cousins, the Vikings saw
a 30.0% increase in passing touchdowns and an 18.18% increase in total touchdowns.


## Notes:
For any further information and/or inquires, please [reach out to me](https://x.com/EthanSterbis)!

### Mentions: [Brad Congelio](https://x.com/BradCongelio)
