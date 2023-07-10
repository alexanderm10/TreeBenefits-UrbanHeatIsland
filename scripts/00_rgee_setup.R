library(rgee)

# Set your Python ENV
Sys.setenv("RETICULATE_PYTHON" = "/usr/bin/python3")

# Set Google Cloud SDK. Only need it the first time you log in. 
Sys.setenv("EARTHENGINE_GCLOUD" = "home/csaybar/google-cloud-sdk/bin/")
ee_Authenticate()

# Init your Earth Session  
ee_Initialize()

install.packages("rgee")
library(remotes)
install_github("r-spatial/rgee")

install.packages(c("remotes", "googledrive"))
remotes::install_github("r-spatial/rgee")

library(rgee)

# Get the username
HOME <- Sys.getenv("HOME")

# 1. Install miniconda
reticulate::install_miniconda(force=T)

# 2. Install Google Cloud SDK
system("curl -sSL https://sdk.cloud.google.com | bash")

# 3 Set global parameters
Sys.setenv("RETICULATE_PYTHON" = sprintf("%s/.local/share/r-miniconda/bin/python3", HOME))
Sys.setenv("EARTHENGINE_GCLOUD" = sprintf("%s/google-cloud-sdk/bin/", HOME))

# 4 Install rgee Python dependencies
ee_install()

# 5. Authenticate and init your EE session
ee_Initialize()
