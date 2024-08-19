# ==================================================================================================== #
# === FUNCTIONS FOR ACCESSING BRAZILIAN INSTITUTE OF GEOGRAPHIC AND STATISTICS (IBGE) DATA VIA APIW === #
# ==================================================================================================== #

# --- Script by Paulo Icaro --- #


# ----------------- #
# --- Libraries --- #
# ----------------- #
library(httr2)                # API Connection (see https://httr2.r-lib.org/)
library(dplyr)                # Library for data manipulation
library(jsonlite)             # Convert Json data to an objetc
library(svDialogs)            # Library for displaying message boxes
library(devtools)             # Import scripts from github






# ------------------------------- #
# --- URL Generation Function --- #
# ------------------------------- #
# Base URL - Version 3.0.0 --> https://servicodados.ibge.gov.br/api/v3/agregados
# Example query to acess Resident Population: ibge_url(4709, 2022, 93)

ibge_url = function(serie, period, variables){
  
  # Script to insert the geographic level of the query
  source('https://raw.githubusercontent.com/paulo-icaro/Ibge_API/main/Geographic_Level.R')
  
  # URL creation
  url = paste0('https://servicodados.ibge.gov.br/api/v3/agregados', '/', serie, '/periodos/', period, '/variaveis/', variables, locality_url) 
  
  # Return output
  return(url)
  
  # Cleasing
  rm(locality_url)
}





# ------------------------------------ #
# --- API Data Collection Function --- #
# ------------------------------------ #
ibge_api = function(url){
  message('Iniciando a conexao com a API do Bacen\n')
  flag = 0
  
  # -- API Connection -- # 
  api_connection = request(base_url = url) %>% req_perform()
  
  
  # --- Connection Flag --- #
    # Sucessfull connection
    if(api_connection$status_code == 200){
      dlg_message(message = 'Conexao bem sucedida ! \nDados sendo coletados ...\n', type  = 'ok')
    } 
    else if(api_connection$status_code != 200){
      while(api_connection$status_code != 200 & flag <= 3){
        flag = flag + 1
      
        if(flag == 1){
          Sys.sleep(2)
          message('Problemas na conexao. \nTentando acessar a API novamente ...\n')}
        if(flag == 2){
          Sys.sleep(5)
          message('Problemas na conexao. \nTentando acessar a API novamente ...\n')}
        if(flag == 3){
          Sys.sleep(10)
          message('Problemas na conexao. ! \nTentando acessar a API uma última vez ...\n')}
      
        api_connection = request(base_url = url) %>% req_perform()
      }
    
      ifelse(api_connection$status_code == 200,
           dlg_message(message = 'Conexao bem sucedida ! \nDados sendo coletados ...\n', type = 'ok'),
           dlg_message(message = 'Falha na conexao ! \nTente conectar com a API mais tarde.', type = 'ok')
      )
    }
  
  
  # --- Converting Data to a Readable Format --- #
  api_connection = rawToChar(api_connection$body)                 # Raw to JSon
  api_connection = fromJSON(api_connection, flatten = TRUE)       # Json to Data Frame
  
}



#query = (ibge_api(ibge_url(4709, 2022, 93)))