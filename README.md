# Causality and Regression Shinylive

## Intro
This app is designed to help users understand the relationships between causality and regression. In particular, it helps teach researchers how to decide which regression to run based an their beliefs about the most plausible causal structure.

This app was originally built in RShiny. Recently it was migrated to [shinylive](https://posit-dev.github.io/r-shinylive/), which allows it to be hosted statically on github and all the R code is run in the browser. How cool that this is possible!

## File Structure
 - app.R has the RShiny app. If you download this you can run it locally on your computer with RStudio
 - compiling_and_uploading.R reminds me the code I need to compule it and upload it to github
 - the app itself is everything in the /docs folder. In Settings -> Pages -> Build and Deployment, the /docs folder is set as the main page.
 - the app live at [https://benrottman.github.io/Causality_and_Regression_shinylive/](https://benrottman.github.io/Causality_and_Regression_shinylive/)

## Contribution
I would love to hear your suggestions to make the app better or to have help making it better. Submit an issue or a pull request on github, or contact me at rottman@pitt.edu
