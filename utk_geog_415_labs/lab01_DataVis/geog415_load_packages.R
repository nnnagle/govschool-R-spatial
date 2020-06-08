cran.pkg.list <- c('learnr','gapminder','kableExtra', 'nycflights13', 'sf',
                   'timetk')
if (length(setdiff(cran.pkg.list, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(cran.pkg.list, rownames(installed.packages())))  
}
if('gradethis' %in% installed.packages() == FALSE)
  remotes::install_github('rstudio-education/gradethis', upgrade='never', force=TRUE)

