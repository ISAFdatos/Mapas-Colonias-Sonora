library(tidyverse)
library(scales)
library(tint)
library(rgdal)
library("Cairo")
library(forcats)
library(ggtext)
library(janitor)
library(sf)
library(ggmap)
library(BAMMtools)
library(magick)
library(patchwork)
library(ggspatial)
logo <- image_read("logos/ISAF ODS color.png")
ODS <- image_read("logos/ODS 6 color.png")
fondo <- "white"


nafo <- north_arrow_fancy_orienteering(
  line_width = 1,
  line_col = "black",
  fill = c("white", "black"),
  text_col = "black",
  text_family = "",
  text_face = NULL,
  text_size = 5,
  text_angle = 0
)


y <- "Hermosillo"

# Elementos para gráficos

titulo <- "Poppins ExtraBold"
negrita <- "Manrope Extrabold"
ligera <- "Manrope Light"
grafico <- "Roboto Condensed"


temasmap <-  theme(axis.line = element_blank(),
                   plot.margin = margin(5, 2, 5, 2),
                   plot.title = element_markdown(family = titulo, size = 20, hjust=0),  
                   plot.subtitle = element_text(family = ligera, size = 6.5, color = "black"), legend.title = element_blank(),
                   axis.text= element_blank(),
                   plot.background = element_rect(fill = "white", color = "white", size = 3),
                   axis.title= element_blank(), 
                   plot.caption = element_text(family = ligera, size = 4, color = "black"),
                   legend.text = element_text(family = grafico, size = 6),
                   legend.position = "none",  legend.justification="left", plot.title.position = 'plot', plot.caption.position = 'plot')

#paleta <- c( "#244e57", "#368990","#EAB287", "#B94645", "#960E53", "gray85" )
paleta <- c( "#093a46","#106174","#1687a3","#3cc4e5","gray50", "gray85" )
ramp <-  c("6","5","4", "3", "2", "1", "NA")

#etiquetas <- c("0%","(0-25%)","[25-50%)","[50-75%)","[75-100%)","100%", "Sin\nvivienda")

# Registro de API en google 

key <- "AIzaSyAdjypPM_SXYMD-klIIsYoZCUS2JTfs4gk"
register_google(key)

# Tipo de mapa de fondo
s <- "element:geometry%7Ccolor:0xf5f5f5&style=element:labels%7Cvisibility:off&style=element:labels.icon%7Cvisibility:off&style=element:labels.text.fill%7Ccolor:0x616161&style=element:labels.text.stroke%7Ccolor:0xf5f5f5&style=feature:administrative%7Celement:geometry%7Cvisibility:off&style=feature:administrative.land_parcel%7Celement:labels.text.fill%7Ccolor:0xbdbdbd&style=feature:administrative.neighborhood%7Cvisibility:off&style=feature:poi%7Cvisibility:off&style=feature:poi%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:poi%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:poi.park%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:poi.park%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:road%7Celement:geometry%7Ccolor:0xffffff&style=feature:road%7Celement:labels.icon%7Cvisibility:off&style=feature:road.arterial%7Celement:labels.text.fill%7Ccolor:0x757575&style=feature:road.highway%7Celement:geometry%7Ccolor:0xdadada&style=feature:road.highway%7Celement:labels.text.fill%7Ccolor:0x616161&style=feature:road.local%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&style=feature:transit%7Cvisibility:off&style=feature:transit.line%7Celement:geometry%7Ccolor:0xe5e5e5&style=feature:transit.station%7Celement:geometry%7Ccolor:0xeeeeee&style=feature:water%7Celement:geometry%7Ccolor:0xc9c9c9&style=feature:water%7Celement:labels.text.fill%7Ccolor:0x9e9e9e&size=640x640"

# Se registra censo (2010, 2020) y ciudad a gráficar

# Capas

capa_mza<- sf::st_read("SHAPES/26m.shp", quiet = TRUE) 

capa_mza<- st_transform(capa_mza, crs = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) %>% clean_names() 

capa_loc<- sf::st_read("SHAPES/26l.shp", quiet = TRUE,options = "ENCODING=windows-1252") 

capa_loc<- st_transform(capa_loc, crs = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) %>% clean_names()

capa_col<- sf::st_read("SHAPES/26col.shp", quiet = TRUE,options = "ENCODING=UTF-8") 

capa_col<- st_transform(capa_col, crs = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) %>% clean_names()

centroids<- st_centroid(capa_loc)

centroids$area <- as.numeric(st_area(capa_loc))

centroloc <- centroids %>%
  mutate(lon = unlist(map(centroids$geometry,1)),
         lat = unlist(map(centroids$geometry,2)),
         zoom=if_else(area>37933799.7, 12, 
                      if_else(area>9421287.4,13,14)),
         colonia=if_else(area>37933799.7, .2, 
                         if_else(area>9421287.4,.3,.4)),
         calle=if_else(area>37933799.7, .1, 
                       if_else(area>9421287.4,.2,.3)))

# Jenks

plot_tinaco_jenks <- function(y = y) {
  colonia_manzana <- read_csv("datos/colonia_manzana.csv", 
                              col_types = cols(cve_col = col_character())) %>% 
    clean_names()
  
  censoagebsonora2020 <- read_csv(paste0("datos/conjunto_de_datos_ageb_urbana_26_cpv2020.csv"), 
                              col_types = cols(TVIVPARHAB = col_double(),
                                               VPH_AEASP = col_double(), 
                                               VPH_DRENAJ = col_double(),
                                               VPH_TINACO = col_double(),
                                               VPH_CISTER = col_double(),
                                               VIVTOT = col_double()
                                               )) %>% 
    clean_names() %>% 
    mutate(censo=2020) %>% 
    mutate(tothog=if_else(is.na(tvivparhab) & vivtot>0, vivtot, tvivparhab),
           vph_aeasp=if_else(is.na(tvivparhab) & vivtot>0, 0, vph_aeasp),
           vph_drenaj=if_else(is.na(tvivparhab) & vivtot>0, 0, vph_drenaj),
           vph_tinaco=if_else(is.na(tvivparhab) & vivtot>0, 0, vph_tinaco),
           vph_cister=if_else(is.na(tvivparhab) & vivtot>0, 0, vph_cister))
  
  # censoagebsonora [is.na(censoagebsonora)] <- 0
  # censoagebsonora2010 <- read_csv(paste0("datos/conjunto_de_datos_ageb_urbana_26_cpv2010.csv"), 
  #                                 col_types = cols(tothog = col_double(),
  #                                                  hogjef_f = col_double(),
  #                                                  pobtot=col_double(),
  #                                                  vivtot=col_double())) %>% 
  #   clean_names() %>% 
  #   mutate(censo=2010) %>% 
  #   mutate(tothog=if_else(is.na(tothog) & vivtot>0, vivtot, tothog),
  #          hogjef_f=if_else(is.na(tothog) & vivtot>0, 0, hogjef_f))
  
  sonmza2020 <- censoagebsonora2020 %>% 
    select(censo,entidad,nom_ent,mun,nom_mun,loc,nom_loc,ageb,mza, tvivparhab,vph_aeasp,vph_drenaj,vph_tinaco,vivtot) %>% 
    unite(cvegeo,c(entidad,mun,loc,ageb,mza),  sep = "", remove = TRUE) %>%
    left_join(colonia_manzana, by="cvegeo")
  
  # sonmza2010 <- censoagebsonora2010 %>% 
  #   select(censo,entidad,nom_ent,mun,nom_mun,loc,nom_loc,ageb,mza, pobtot, tothog,hogjef_f) %>% 
  #   unite(cvegeo,c(entidad,mun,loc,ageb,mza),  sep = "", remove = TRUE) %>%
  #   left_join(colonia_manzana, by="cvegeo")
  
  sonmza <-  sonmza2020
  
  soncol <- sonmza %>% 
    group_by(censo, nom_loc, cve_col, colonia) %>% 
    summarise(tvivparhab=sum(tvivparhab, na.rm = TRUE), vph_aeasp=sum(vph_aeasp, na.rm = TRUE), 
              vph_drenaj=sum(vph_drenaj, na.rm = TRUE),  vph_tinaco=sum(vph_tinaco, na.rm = TRUE),vivtot=sum(vivtot, na.rm = TRUE))
  
  
  soncol <- soncol %>% mutate(entubada_sp=round((vph_aeasp/tvivparhab)*100,1), 
                              tinaco=round((vph_tinaco/tvivparhab)*100,1), 
                              drenaje=round((vph_drenaj/tvivparhab)*100,1))
  

  
  capa_col <- capa_col %>%
    merge(soncol) 
  
  

  
  capa_ciudad <- capa_col %>% filter (nom_loc==y)
  
  
  jenks <- getJenksBreaks(capa_ciudad$tinaco[!is.na(capa_ciudad$tinaco)], 5, subset = NULL)
  
  capa_ciudad <- capa_ciudad %>% 
    mutate(agua=if_else(tinaco<=jenks[1], 1,
                         if_else(tinaco<=jenks[2], 2,
                                 if_else(tinaco<=jenks[3],3,
                                         if_else(tinaco<=jenks[4],4,5))))) 
  
  niveles <- c(paste0( jenks[4], " - ",  jenks[5], "%"),
               paste0( jenks[3], " - ",  jenks[4], "%"),
               paste0( jenks[2], " - ",  jenks[3], "%"),
               paste0( jenks[1], " - ",  jenks[2], "%"),
               "Sin tinacos",
               "Sin viviendas censadas") 
  capa_ciudad <- capa_ciudad %>% 
    mutate(qagua=case_when(agua==1 ~ "Sin tinacos",
                            agua==2 ~ paste0( jenks[1], " - ",  jenks[2], "%"),
                            agua==3 ~ paste0( jenks[2], " - ",  jenks[3], "%"),
                            agua==4 ~ paste0( jenks[3], " - ",  jenks[4], "%"),
                            agua==5 ~ paste0( jenks[4], " - ",  jenks[5], "%"),
                            is.na(vivtot) ~ "Sin viviendas censadas",
                            tvivparhab==0 ~ "Sin viviendas censadas")) %>% 
    filter(!is.na(censo))
  titulolab <-paste0("<span style = 'font-size:8pt'>SONORA EN EL DÍA MUNDIAL DEL AGUA</span><br><span style = 'color:#26BDE2';>",str_to_upper(paste(strwrap("Disponibilidad de tinacos", width = 44), collapse = "<br>")),"</span>")
  subtilab <- paste0("Viviendas particulares habitadas que disponen de tinaco en relación al total en las colonias de la localidad | 2020") 
  fuente <- paste0("Elaborado por la Dirección General de Estudios de Políticas Públicas y Análisis de Datos del ISAF con información de INEGI (2020) y cartografía de INE (2023).")
  
  coord <- centroloc %>% filter(nomgeo==y) %>% unite(cveloc,c(cve_ent,cve_mun,cve_loc),  sep = "", remove = FALSE)
  
  capa_mza <- capa_mza %>% unite(cveloc,c(cve_ent,cve_mun,cve_loc),  sep = "", remove = FALSE) %>% filter(cveloc==coord$cveloc)
  
  gmap <- get_googlemap(maptype ="roadmap", center = c(lon=coord$lon,lat =  coord$lat), zoom = coord$zoom, style = s)
  
  capa_loc <- capa_loc %>%  filter(nomgeo==y)
  
  p1 <- ggmap(gmap) +
    geom_sf(data = capa_ciudad, inherit.aes = FALSE,
            aes(geometry=geometry, fill=qagua), color="white", size=coord$colonia) +
    geom_sf(data = capa_mza, inherit.aes = FALSE,
            aes(geometry=geometry), fill="transparent", color=alpha("white",0.4), size=coord$calle) +
    coord_sf(datum = NA) +
    scale_fill_manual(values = paleta,
                      drop = F,
                      breaks = niveles,
                      labels = niveles,
                      guide = guide_legend(
                        label.position = "top",
                        override.aes = list(size = 0))) + 
    annotation_scale(location = "tr", width_hint = 0.1, text_cex = 0.5, height = unit(0.1, "cm"),) +
    annotation_north_arrow( height = unit(0.5, "cm"),
                            width = unit(0.5, "cm"),
                            location = "tr", which_north = "true", 
                           pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                           style = nafo) +
    guides(fill=guide_legend(title.position = "top", 
                             drop = F,
                             hjust = 0.5, #centres the title horizontally
                             title.hjust = 0.5,
                             label.position = "top", reverse=TRUE, nrow = 1)) +
    theme_void() + 
    temasmap + 
    theme(legend.position = "top",
                                    legend.direction = "horizontal", 
                                    legend.text = element_text(family = grafico, size=6,margin = margin(0, 0, 0, 0, "pt")), 
                                    legend.spacing.x = unit(1, "pt"),
                                    legend.key.width = unit(44, "pt"), 
                                    legend.key.height = unit(5, "pt"),
                                    legend.title = element_text(family = negrita, size = 6, color = "black", hjust=0),
                                    axis.title = element_blank()) +
    guides(fill=guide_legend(title.position = "top", 
                             hjust = 0.5, #centres the title horizontally
                             title.hjust = 0.5,
                             label.position = "top", reverse=TRUE, nrow = 1)) +
    labs(y = NULL, x = NULL, title  = titulolab, 
         subtitle = subtilab,  fill = NULL, 
         caption =fuente)
  p1
  ggsave(paste0("mapas/agua/tinacos/tinacos_",coord$cvegeo,"_2020_",y,".png"),p1, width = 5, height = 6, type = "cairo", dpi = 400)


  # TOTAL POR LOCALIDAD
  localidades <- read_csv(paste0("datos/conjunto_de_datos_iter_26CSV2020.csv"), 
                          col_types = cols(TVIVPARHAB = col_double(),
                                           VPH_AEASP = col_double(), 
                                           VPH_DRENAJ = col_double(),
                                           VPH_TINACO = col_double(),
                                           VPH_CISTER = col_double(),
                                           VIVTOT = col_double()
                                           )) %>% 
    clean_names() 
  locagua <- localidades  %>% 
    select(nom_ent,nom_loc,tvivparhab,vph_aeasp,vph_drenaj,vph_tinaco,vivtot) %>% 
    filter (nom_loc==y) %>% 
    mutate(entubada_sp=round((vph_aeasp/tvivparhab)*100,1), 
           tinaco=round((vph_tinaco/tvivparhab)*100,1), 
           drenaje=round((vph_drenaj/tvivparhab)*100,1)) %>% 
    filter(!is.na(tvivparhab)) %>% 
    filter(tvivparhab==max(tvivparhab))
  
  ggplot() +
    annotate(x=0, y=.38, "text", label = str_to_upper(paste(strwrap(locagua$nom_loc, width = 14), collapse = "\n")),vjust=0, family=titulo, size=3, hjust=0) +
    annotate(x=0, y=.25, "text", label = "Viviendas particulares habitadas\nque disponenen de tinaco en 2020", family=negrita, size=1, hjust=0) +
    annotate(x=0, y=0.1, "text", label = paste0(locagua$tinaco,"%") , family=titulo, size=4, hjust=0, color="#106174") +
    annotate(x=0, y=0.0, "text", label = prettyNum(locagua$vph_tinaco, big.mark ="," ), family=negrita, size=1, hjust=0) +
    scale_x_continuous(limits=c(0,2)) +
    scale_y_continuous(limits=c(0,2)) +
    theme_void() 
  ggsave("logos/pct2020.png", width = 2, height =2, type = "cairo", dpi = 400)
  pct2020 <- image_read("logos/pct2020.png")
  
  
  p2 <-  image_read(paste0("mapas/agua/tinacos/tinacos_",coord$cvegeo,"_2020_",y,".png"))
  p3 <- cowplot::ggdraw() +
    cowplot::draw_image(p2, x = 0, y = 0, width = 1, height = 1) +
    cowplot::draw_image(ODS, x = 0.88, y = 0.89, width = 0.10, height = 0.10)+
    cowplot::draw_image(logo, x = 0.77, y = 0.0001, width = 0.18, height = 0.18)+
    cowplot::draw_image(pct2020, x = 0.02, y = 0.001, width = 0.65, height = 0.65)  
  p3
  ggsave(paste0("mapas/agua/tinacos/tinacos_",coord$cvegeo,"_2020_",y,".png"),p3, width = 5, height = 6, type = "cairo", dpi = 400)
  
}


ciudad <- "Miguel Alemán (La Doce)"


plot_tinaco_jenks(ciudad)
#censos <- c("2020", "2010")
ciudades <- c("Hermosillo", "Ciudad Obregón", "Heroica Nogales", "San Luis Río Colorado", "Navojoa", "Puerto Peñasco", 
              "Heroica Guaymas", "Agua Prieta", "Empalme", "Heroica Caborca", "Heroica Ciudad de Cananea", "Etchojoa", "Huatabampo")


for (i in unique(ciudades)) {
    plot_agua_jenks(i)
  }




plot_tinaco_jenks <- function(y = y) {
  colonia_manzana <- read_csv("datos/colonia_manzana.csv", 
                              col_types = cols(cve_col = col_character())) %>% 
    clean_names()
  
  censoagebsonora2020 <- read_csv(paste0("datos/conjunto_de_datos_ageb_urbana_26_cpv2020.csv"), 
                                  col_types = cols(TVIVPARHAB = col_double(),
                                                   VPH_AEASP = col_double(), 
                                                   VPH_DRENAJ = col_double(),
                                                   VPH_TINACO = col_double(),
                                                   VPH_CISTER = col_double(),
                                                   VIVTOT = col_double()
                                  )) %>% 
    clean_names() %>% 
    mutate(censo=2020) %>% 
    mutate(tothog=if_else(is.na(tvivparhab) & vivtot>0, vivtot, tvivparhab),
           vph_aeasp=if_else(is.na(tvivparhab) & vivtot>0, 0, vph_aeasp),
           vph_drenaj=if_else(is.na(tvivparhab) & vivtot>0, 0, vph_drenaj),
           vph_tinaco=if_else(is.na(tvivparhab) & vivtot>0, 0, vph_tinaco),
           vph_cister=if_else(is.na(tvivparhab) & vivtot>0, 0, vph_cister))
  
  # censoagebsonora [is.na(censoagebsonora)] <- 0
  # censoagebsonora2010 <- read_csv(paste0("datos/conjunto_de_datos_ageb_urbana_26_cpv2010.csv"), 
  #                                 col_types = cols(tothog = col_double(),
  #                                                  hogjef_f = col_double(),
  #                                                  pobtot=col_double(),
  #                                                  vivtot=col_double())) %>% 
  #   clean_names() %>% 
  #   mutate(censo=2010) %>% 
  #   mutate(tothog=if_else(is.na(tothog) & vivtot>0, vivtot, tothog),
  #          hogjef_f=if_else(is.na(tothog) & vivtot>0, 0, hogjef_f))
  
  sonmza2020 <- censoagebsonora2020 %>% 
    select(censo,entidad,nom_ent,mun,nom_mun,loc,nom_loc,ageb,mza, tvivparhab,vph_aeasp,vph_drenaj,vph_tinaco,vivtot) %>% 
    unite(cvegeo,c(entidad,mun,loc,ageb,mza),  sep = "", remove = TRUE) %>%
    left_join(colonia_manzana, by="cvegeo")
  
  # sonmza2010 <- censoagebsonora2010 %>% 
  #   select(censo,entidad,nom_ent,mun,nom_mun,loc,nom_loc,ageb,mza, pobtot, tothog,hogjef_f) %>% 
  #   unite(cvegeo,c(entidad,mun,loc,ageb,mza),  sep = "", remove = TRUE) %>%
  #   left_join(colonia_manzana, by="cvegeo")
  
  sonmza <-  sonmza2020
  
  soncol <- sonmza %>% 
    group_by(censo, nom_loc, cve_col, colonia) %>% 
    summarise(tvivparhab=sum(tvivparhab, na.rm = TRUE), vph_aeasp=sum(vph_aeasp, na.rm = TRUE), 
              vph_drenaj=sum(vph_drenaj, na.rm = TRUE),  vph_tinaco=sum(vph_tinaco, na.rm = TRUE),vivtot=sum(vivtot, na.rm = TRUE))
  
  
  soncol <- soncol %>% mutate(entubada_sp=round((vph_aeasp/tvivparhab)*100,1), 
                              tinaco=round((vph_tinaco/tvivparhab)*100,1), 
                              drenaje=round((vph_drenaj/tvivparhab)*100,1))
  
    capa_col <- capa_col %>%
    merge(soncol) 
  
  
  
  
  capa_ciudad <- capa_col %>% filter (nom_loc==y)
  
  
  jenks <- getJenksBreaks(capa_ciudad$entubada_sp[!is.na(capa_ciudad$entubada_sp)], 5, subset = NULL)
  
  capa_ciudad <- capa_ciudad %>% 
    mutate(agua=if_else(entubada_sp<=jenks[1], 1,
                        if_else(entubada_sp<=jenks[2], 2,
                                if_else(entubada_sp<=jenks[3],3,
                                        if_else(entubada_sp<=jenks[4],4,5))))) 

  niveles <- c(paste0( jenks[4], " - ",  jenks[5], "%"),
               paste0( jenks[3], " - ",  jenks[4], "%"),
               paste0( jenks[2], " - ",  jenks[3], "%"),
               paste0( jenks[1], " - ",  jenks[2], "%"),
               "Sin tinacos",
               "Sin viviendas censadas") 
  capa_ciudad <- capa_ciudad %>% 
    mutate(qagua=case_when(agua==1 ~ "Sin agua entubada abastecida del servicio público",
                           agua==2 ~ paste0( jenks[1], " - ",  jenks[2], "%"),
                           agua==3 ~ paste0( jenks[2], " - ",  jenks[3], "%"),
                           agua==4 ~ paste0( jenks[3], " - ",  jenks[4], "%"),
                           agua==5 ~ paste0( jenks[4], " - ",  jenks[5], "%"),
                           is.na(vivtot) ~ "Sin viviendas censadas",
                           tvivparhab==0 ~ "Sin viviendas censadas")) %>% 
    filter(!is.na(censo))
  titulolab <-paste0("<span style = 'font-size:8pt'>SONORA EN EL DÍA MUNDIAL DEL AGUA</span><br><span style = 'color:#26BDE2';>",str_to_upper(paste(strwrap("Agua entubada", width = 44), collapse = "<br>")),"</span>")
  subtilab <- paste0("Viviendas particulares habitadas que disponen de agua entubada abastecida del servicio público\nen relación al total en las colonias de la localidad | 2020") 
  fuente <- paste0("Elaborado por la Dirección General de Estudios de Políticas Públicas y Análisis de Datos del ISAF con información de INEGI (2020) y cartografía de INE (2023).")
  
  coord <- centroloc %>% filter(nomgeo==y) %>% unite(cveloc,c(cve_ent,cve_mun,cve_loc),  sep = "", remove = FALSE)
  
  capa_mza <- capa_mza %>% unite(cveloc,c(cve_ent,cve_mun,cve_loc),  sep = "", remove = FALSE) %>% filter(cveloc==coord$cveloc)
  
  gmap <- get_googlemap(maptype ="roadmap", center = c(lon=coord$lon,lat =  coord$lat), zoom = coord$zoom, style = s)
  
  capa_loc <- capa_loc %>%  filter(nomgeo==y)
  
  p1 <- ggmap(gmap) +
    geom_sf(data = capa_ciudad, inherit.aes = FALSE,
            aes(geometry=geometry, fill=qagua), color="white", size=coord$colonia) +
    geom_sf(data = capa_mza, inherit.aes = FALSE,
            aes(geometry=geometry), fill="transparent", color=alpha("white",0.4), size=coord$calle) +
    coord_sf(datum = NA) +
    scale_fill_manual(values = paleta,
                      drop = F,
                      breaks = niveles,
                      labels = niveles,
                      guide = guide_legend(
                        label.position = "top",
                        override.aes = list(size = 0))) + 
    annotation_scale(location = "tr", width_hint = 0.1, text_cex = 0.5, height = unit(0.1, "cm"),) +
    annotation_north_arrow( height = unit(0.5, "cm"),
                            width = unit(0.5, "cm"),
                            location = "tr", which_north = "true", 
                            pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                            style = nafo) +
    guides(fill=guide_legend(title.position = "top", 
                             drop = F,
                             hjust = 0.5, #centres the title horizontally
                             title.hjust = 0.5,
                             label.position = "top", reverse=TRUE, nrow = 1)) +
    theme_void() + 
    temasmap + 
    theme(legend.position = "top",
          legend.direction = "horizontal", 
          legend.text = element_text(family = grafico, size=6,margin = margin(0, 0, 0, 0, "pt")), 
          legend.spacing.x = unit(1, "pt"),
          legend.key.width = unit(44, "pt"), 
          legend.key.height = unit(5, "pt"),
          legend.title = element_text(family = negrita, size = 6, color = "black", hjust=0),
          axis.title = element_blank()) +
    guides(fill=guide_legend(title.position = "top", 
                             hjust = 0.5, #centres the title horizontally
                             title.hjust = 0.5,
                             label.position = "top", reverse=TRUE, nrow = 1)) +
    labs(y = NULL, x = NULL, title  = titulolab, 
         subtitle = subtilab,  fill = NULL, 
         caption =fuente)
  p1
  ggsave(paste0("mapas/agua/aguaent/aguaent_",coord$cvegeo,"_2020_",y,".png"),p1, width = 5, height = 6, type = "cairo", dpi = 400)
  
  
  # TOTAL POR LOCALIDAD
  localidades <- read_csv(paste0("datos/conjunto_de_datos_iter_26CSV2020.csv"), 
                          col_types = cols(TVIVPARHAB = col_double(),
                                           VPH_AEASP = col_double(), 
                                           VPH_DRENAJ = col_double(),
                                           VPH_TINACO = col_double(),
                                           VPH_CISTER = col_double(),
                                           VIVTOT = col_double()
                          )) %>% 
    clean_names() 
  locagua <- localidades  %>% 
    select(nom_ent,nom_loc,tvivparhab,vph_aeasp,vph_drenaj,vph_tinaco,vivtot) %>% 
    filter (nom_loc==y) %>% 
    mutate(entubada_sp=round((vph_aeasp/tvivparhab)*100,1), 
           tinaco=round((vph_tinaco/tvivparhab)*100,1), 
           drenaje=round((vph_drenaj/tvivparhab)*100,1)) %>% 
    filter(!is.na(tvivparhab)) %>% 
    filter(tvivparhab==max(tvivparhab))
  
  ggplot() +
    annotate(x=0, y=.38, "text", label = str_to_upper(paste(strwrap(locagua$nom_loc, width = 14), collapse = "\n")),vjust=0, family=titulo, size=3, hjust=0) +
    annotate(x=0, y=.25, "text", label = "Viviendas particulares habitadas que disponen de agua\nentubada abastecida del servicio público en 2020", family=negrita, size=1, hjust=0) +
    annotate(x=0, y=0.1, "text", label = paste0(locagua$entubada_sp,"%") , family=titulo, size=4, hjust=0, color="#106174") +
    annotate(x=0, y=0.0, "text", label = prettyNum(locagua$vph_aeasp, big.mark ="," ), family=negrita, size=1, hjust=0) +
    scale_x_continuous(limits=c(0,2)) +
    scale_y_continuous(limits=c(0,2)) +
    theme_void() 
  ggsave("logos/pct2020.png", width = 2, height =2, type = "cairo", dpi = 400)
  pct2020 <- image_read("logos/pct2020.png")
  
  
  p2 <-  image_read(paste0("mapas/agua/aguaent/aguaent_",coord$cvegeo,"_2020_",y,".png"))
  p3 <- cowplot::ggdraw() +
    cowplot::draw_image(p2, x = 0, y = 0, width = 1, height = 1) +
    cowplot::draw_image(ODS, x = 0.88, y = 0.89, width = 0.10, height = 0.10)+
    cowplot::draw_image(logo, x = 0.77, y = 0.0001, width = 0.18, height = 0.18)+
    cowplot::draw_image(pct2020, x = 0.02, y = 0.001, width = 0.65, height = 0.65)  
  p3
  ggsave(paste0("mapas/agua/aguaent/aguaent_",coord$cvegeo,"_2020_",y,".png"),p3, width = 5, height = 6, type = "cairo", dpi = 400)
  
}

ciudad <- "Miguel Alemán (La Doce)"

plot_agua_jenks(ciudad)
#censos <- c("2020", "2010")
ciudades <- c("Hermosillo", "Ciudad Obregón", "Heroica Nogales", "San Luis Río Colorado", "Navojoa", "Puerto Peñasco", 
              "Heroica Guaymas", "Agua Prieta", "Empalme", "Heroica Caborca", "Heroica Ciudad de Cananea", "Etchojoa", "Huatabampo")


for (i in unique(ciudades)) {
  plot_agua_jenks(i)
}

