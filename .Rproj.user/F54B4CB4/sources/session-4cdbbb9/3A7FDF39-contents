library(rgdal)
library(janitor)
library(sf)
library(tidyverse)

capa_mza <- st_read("SHAPES/INEGI/conjunto_de_datos/26m.shp", quiet = TRUE) 

capa_mza<- st_transform(capa_mza, crs = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) %>% 
  clean_names() %>% 
  st_make_valid()

capa_col <- st_read("SHAPES/INE/COLONIA.shp", quiet = TRUE,options = "ENCODING=windows-1252") 

capa_col <- st_transform(capa_col, crs = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) %>% 
  clean_names() %>% 
  st_make_valid() %>% 
  select(-entidad,-municipio,-geometry1, -id)

capa_loc <- st_read("SHAPES/INEGI/conjunto_de_datos/26l.shp", quiet = TRUE,options = "ENCODING=windows-1252") 

capa_loc <- st_transform(capa_loc, crs = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) %>% 
  clean_names() %>% 
  st_make_valid()


# Código de INEGI para municipio y localidad en las colonias del INE

capa_col <- capa_col %>% st_join(capa_loc, join = st_within, largest = TRUE)

capa_col <- capa_col %>%
  unite(cve_col,c(cve_ent,cve_mun,cve_loc,control),  sep = "", remove = FALSE) %>% 
  rename(nom_col=nombre, nom_loc=nomgeo) %>% 
  select(cve_ent, cve_mun, cve_loc, nom_loc, cve_col, nom_col, cp, otros_cp)

st_write(capa_col, "SHAPES/INE/COLONIA_INEGI.shp")

colonias <- capa_col %>% 
  select (cve_col, nom_col)

capa_mza <- capa_mza  %>% 
  st_join(colonias, join = st_within, largest = TRUE)

st_write(capa_col, "SHAPES/INEGI/26mcol.shp")