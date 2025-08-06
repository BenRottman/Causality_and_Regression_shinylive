


#following along to this:
# https://posit-dev.github.io/r-shinylive/

install.packages("shinylive")
install.packages("usethis")
install.packages("gh")

library(shinylive)
library(httpuv)

# had to install these earlier
# install.packages(c("knitr", "pillar", "rmarkdown", "tinytex", "data.table", "Hmisc", "nloptr"))

#the compiling
#for this function, for the first folder, it knows to get the app.R file
shinylive::export("/Users/ROTTMAN/Library/CloudStorage/OneDrive-UniversityofPittsburgh/Teaching/Research Methods/Tutorials/CausalityAndMultipleRegression/Version for webr_shinylive/Causality_and_Regression_shinylive", 
                  "/Users/ROTTMAN/Library/CloudStorage/OneDrive-UniversityofPittsburgh/Teaching/Research Methods/Tutorials/CausalityAndMultipleRegression/Version for webr_shinylive/Causality_and_Regression_shinylive/docs",
                  package_cache = TRUE)

#test it locally
httpuv::runStaticServer("/Users/ROTTMAN/Library/CloudStorage/OneDrive-UniversityofPittsburgh/Teaching/Research Methods/Tutorials/CausalityAndMultipleRegression/Version for webr_shinylive/Causality_and_Regression_shinylive/docs")


#putting the whole foldler with R code as well on GitHub
setwd("/Users/ROTTMAN/Library/CloudStorage/OneDrive-UniversityofPittsburgh/Teaching/Research Methods/Tutorials/CausalityAndMultipleRegression/Version for webr_shinylive/Causality_and_Regression_shinylive")

#this mades a .git folder and a .gitignore file inside the folder, but it is hidden, press Command + shift + period to see it
usethis::use_git()

system("git remote -v")
system("git remote add origin https://github.com/BenRottman/Causality_and_Regression_shinylive.git")
system("git branch") #should say main
system("git branch -M main")  # Only needed if your branch is not 'main'
system("git push -u origin main")



usethis::use_github()
# usethis::use_github_action(url="https://github.com/posit-dev/r-shinylive/blob/actions-v1/examples/deploy-app.yaml")