
options(repos=structure(c(CRAN="https://cloud.r-project.org/")))

packages<-c("config","golem",'readr', 'janitor', 'echarts4r', 'DT', 'lubridate', 'shinycssloaders', "plotly","shinyTime")
Sys.setenv(RENV_DOWNLOAD_METHOD = "libcurl")
install.packages(packages)
install.packages("~/ProcessVizApp/processViz_1.1.0.0.tar.gz", repos = NULL, type="source")
