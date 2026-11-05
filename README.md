# Baseball Analytics

R code and data behind my writing at https://henryjbelton.substack.com

## Projects
- **Free agent contract predictions, 2026-27**: my model's predicted length and AAV for every free agent this winter, locked before any free agent signed. [Read the post](https://henryjbelton.substack.com/p/pricing-the-26-27-free-agent-class) and [try the prediction tool](https://hankbelton.github.io/baseball/tool/). The graded predictions are in `fa-predictions-2026-27-nov3/`, and the original Oct. 30 lock is in `fa-predictions-2026-27/`.
- **Bat speed**: What drives bat speed? OLS on swing length, weight, age, height and strike zone height. Code in `bat_speed_model.R`. [Read the post](https://henryjbelton.substack.com/p/what-drives-bat-speed)
- **Sun altitude**: Does sun altitude affect xwOBA at Rate Field? Code in `sun_altitude_vs_xwOBA.R`. [Read the post](https://henryjbelton.substack.com/p/sunlight-and-stuff)
- **Hitter aging**: GAM age curves and regression to the mean for hitters. [Read the post](https://henryjbelton.substack.com/p/father-time-vs-hitters)

## Data
Data comes from Baseball Savant, FanGraphs, Baseball-Reference, MLB, MLB Trade Rumors, Spotrac, Cot's Contracts and baseballr. Raw and paid-source data are not included. Each script's header says how to pull what it needs.

## Author
Henry Belton, Chicago Booth MBA. [LinkedIn](https://www.linkedin.com/in/henryjbelton/)
