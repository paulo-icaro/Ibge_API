# =========================================================== #
# === DATA COLLECTION OF POPULATION DATA THROUGH IBGE API === #
# =========================================================== #

# --- Script by Paulo Icaro --- #


# ----------------- #
# --- Libraries --- #
# ----------------- #
library(devtools)             # Import scripts from github



# Script to insert the geographic level of the query
source('https://raw.githubusercontent.com/paulo-icaro/Ibge_API/main/Geographic_Level.R')


source('IBGE_API.R')
#source('https://raw.githubusercontent.com/paulo-icaro/Ibge_API/main/IBGE_API.R')


# --------------------------- #
# --- Auxiliary Functions --- #
# --------------------------- #
x = ibge_api(ibge_url(4709, 2022, 93))
x = x[[4]][[1]][[2]][[1]]

