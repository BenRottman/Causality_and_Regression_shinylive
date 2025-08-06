


#following along to this:
# https://posit-dev.github.io/r-shinylive/

install.packages("shinylive")
library(shinylive)
library(httpuv)



#the compiling
#for this function, for the first folder, it knows to get the app.R file
shinylive::export("/Users/ROTTMAN/Library/CloudStorage/OneDrive-UniversityofPittsburgh/Teaching/Research Methods/Tutorials/CausalityAndMultipleRegression/Version for webr_shinylive/Causality_and_Regression_shinylive", 
                  "/Users/ROTTMAN/Library/CloudStorage/OneDrive-UniversityofPittsburgh/Teaching/Research Methods/Tutorials/CausalityAndMultipleRegression/Version for webr_shinylive/Causality_and_Regression_shinylive/docs",
                  package_cache = TRUE)

#test it locally
httpuv::runStaticServer("/Users/ROTTMAN/Library/CloudStorage/OneDrive-UniversityofPittsburgh/Teaching/Research Methods/Tutorials/CausalityAndMultipleRegression/Version for webr_shinylive/Causality_and_Regression_shinylive/docs")



setwd("/Users/ROTTMAN/Library/CloudStorage/OneDrive-UniversityofPittsburgh/Teaching/Research Methods/Tutorials/CausalityAndMultipleRegression/Version for webr_shinylive/webr_shinylive_export")
usethis::use_git()
usethis::use_github()

# install.packages(c("knitr", "pillar", "rmarkdown", "tinytex", "data.table", "Hmisc", "nloptr"))

install.packages("usethis")
install.packages("gh")

usethis::use_github_action(url="https://github.com/posit-dev/r-shinylive/blob/actions-v1/examples/deploy-app.yaml")