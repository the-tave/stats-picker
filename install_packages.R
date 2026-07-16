# Install relevant packages
options(timeout = 600)

# Regular packages
packages <- c('shinyjs', 'dplyr', 'ggplot2', 'bslib', 'shiny.i18n', 'jsonlite', 
'rclipboard', 'shinydashboard', 'shinyWidgets', 'httr2', 'ggridges', 
'shinythemes', 'xtable', 'cowplot', 'faux', 'tidyr', 'tibble', 
'markdown', 'readr', 'urltools') 
# urltools is necessary as a dependency for shiny.pwa

# Listed before for some reason but most likely not needed: 
# , 'extrafont'

for (pkg in packages) {
  cat("Installing:", pkg, "\n")
  install.packages(pkg, repos = 'https://cloud.r-project.org')
}
# install.packages(packages, repos = 'https://cloud.r-project.org')

missing <- setdiff(packages, rownames(installed.packages()))

if (length(missing)) stop('Missing R packages: ', paste(missing, collapse = ', '))

# Install shiny.pwa, which is not on CRAN
install.packages('https://cran.r-project.org/src/contrib/Archive/shiny.pwa/shiny.pwa_0.2.1.tar.gz', 
repos = NULL, type = 'source')

if (!requireNamespace('shiny.pwa', quietly = TRUE)) stop('Missing R package: shiny.pwa')
