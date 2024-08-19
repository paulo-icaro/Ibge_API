# ============================================= #
# === DETERMINE THE GEOGRAPHIC LEVEL CHOICE === #
# ============================================= #

# --- Script by Paulo Icaro --- #


# ----------------- #
# --- Libraries --- #
# ----------------- #
library(svDialogs)
library(readxl)
library(dplyr)



# ------------------------------------------------ #
# --- Regions, States and Municipalities Table --- #
# ------------------------------------------------ #
localities = read.csv(file = 'https://raw.githubusercontent.com/paulo-icaro/Ibge_API/main/Tabela_(Regioes_Estados_Municipios).csv', header = TRUE, sep = ';', fileEncoding =  'LATIN1')
dic_region = unique(localities[c(1,2)])
dic_state = unique(localities[c(3,4)])
dic_municipality = unique(localities[c(5,6)])




# ------------------------------------------------ #
# --- 1° Step - Determine the Geographic Level --- #
# ------------------------------------------------ #
geographic_level = 
  dlg_list(
    choices = c('Pais', 'Regiao', 'Estado', 'Municipio'),
    preselect = 'Brasil',
    multiple = FALSE,
    title = 'Nivel Geografico'
  )$res




# --------------------------------------------------------- #
# --- 2° Step - Generate the Specific URL Part Accordly --- #
# --------------------------------------------------------- #

# --- URL - Country Level --- #
if(geographic_level == 'Pais'){locality_url = '?localidades=N1[all]'}



# --- URL - Region Level --- #
if(geographic_level == 'Regiao'){
  
  # Region selection
  region_level = dlg_list(
                    choices = unlist(unique(localities[,2])),
                    preselect = 'Norte',
                    multiple = TRUE,
                    title = 'Regiao'
                  )$res
  
  
  # URL part for single or multiple selection
  if(length(region_level) == 1){
    cod_region = (dic_regions %>% filter(Regiao == region_level))[,1]
    locality_url = paste0('?localidades=N2[', cod_region)
  }
  else{
    cod_region = (dic_regions %>% filter(Regiao %in% region_level))[,1]
    
    for(i in 1:length(unlist(cod_region))){
      if(i == 1){string = NULL}
      if(i < length(unlist(cod_region))){string = paste0(string, unlist(code_region)[i], ',')}
      else {string = paste0(string, unlist(code_region)[i])}
    }
    
    locality_url = paste0('?localidades=N2[', string, ']')
  }
  
  # Specific cleasing #
  rm(region_level, cod_region, i, string)
}




# --- URL - State Level --- #
if(geographic_level == 'Estado'){
  
  # State selection
  state_level = dlg_list(
    choices = unlist(unique(localities[,4])),
    preselect = 'Acre',
    multiple = TRUE,
    title = 'Estado'
  )$res
  
  
  # URL part for single or mutiple selection
  if(length(state_level) == 1){
    cod_state = (dic_state %>% filter(Estado == state_level))[,1]
    locality_url = paste0('?localidades=N3[', cod_state)
  }
  else{
    cod_state = (dic_state %>% filter(Estado %in% state_level))[,1]
    
    for(i in 1:length(unlist(cod_state))){
      if(i == 1){string = NULL}
      if(i < length(unlist(cod_state))){string = paste0(string, unlist(cod_state)[i], ',')}
      else{string = paste0(string, unlist(cod_state)[i])}
    }
    
    locality_url = paste0('?localidades=N3[', string, ']')
  }
  
  # Specific cleasing #
  rm(state_level, cod_state, i, string)
}



# --- URL - Municipality Level --- #
if(geographic_level == 'Municipio'){
  
  # Specify the query level #
  specify_query = dlg_list(
    choices = c('Por Estado', 'Por Regiao', 'Lista Completa'),
    preselect = 'Por Estado',
    multiple = FALSE,
    title = 'Tipo de visualização'
  )$res
  
  
  # Specify municipalities according to selected state #
  if(specify_query == 'Por Estado'){
    
      # State Selection    
      state_level = dlg_list(
        choices = unlist(unique(localities[,4])),
        preselect = 'Acre',
        multiple = TRUE,
        title = 'Estado'
      )$res
    
      # Municipality Selection
      municipality_level = dlg_list(
        choices = unlist((localities %>% filter(UF %in% state_level))[,6]),
        multiple = TRUE,
        title = 'Municipio'
      )$res
      
      # Specifc cleasing
      rm(state_level)
      
  }
  else if (specify_query == 'Por Regiao'){
   
      # Region Selection    
      region_level = dlg_list(
        choices = unlist(unique(localities[,2])),
        preselect = 'Norte',
        multiple = TRUE,
        title = 'Regiao'
      )$res
    
      # Municipality Selection
      municipality_level = dlg_list(
        choices = unlist((localities %>% filter(Regiao %in% region_level))[,6]),
        multiple = TRUE,
        title = 'Municipio'
      )$res
      
      # Specifc cleasing
      rm(region_level)
      
  }
  else{
      # Municipality Selection
      municipality_level = dlg_list(
        choices = sort(unlist(unique(localities[,6]))),
        preselect = "Alta Floresta D'Oeste",
        multiple = TRUE,
        title = 'Municipio'
      )$res
  }
  
  
  
  # URL part for single or multiple selection
  if(length(municipality_level) == 1){
    cod_municipality = (dic_municipality %>% filter(Municipio == municipality_level))[,1]
    locality_url = paste0('?localidades=N4[', cod_municipality)
  }
  else{
    cod_municipality = (dic_municipality %>% filter(Municipio %in% municipality_level))[,1]
    
    for(i in 1:length(unlist(cod_municipality))){
      if(i == 1){string = NULL}
      if(i < length(unlist(cod_municipality))){string = paste0(string, unlist(cod_municipality)[i], ',')}
      else{string = paste0(string, unlist(cod_municipality)[i])}
    }
    
    locality_url = paste0('?localidades=N6[', string, ']')
  }

  
  # Specific cleasing #
  rm(municipality_level, cod_municipality, i, string, specify_query)
}



# Main Cleasing #
rm(localities, dic_region, dic_state, dic_municipality, geographic_level)