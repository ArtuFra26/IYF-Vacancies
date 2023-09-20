
#Definir tema de gráficos
theme_app_iyf <- theme(
  text = element_text(family = "lora", size = 10),
  title = element_text(face = "bold", size = 12),
  axis.title = element_text(size = 10)
)

#Función para lectura del directorio
readVacs <- function(path){
  out <- tryCatch(
    #Función de búsqueda
    {
      #Probar con distintos paths hasta encontrar el correcto
      read_excel(path, guess_max = 50000)
    },
    #Devolución de error
    error = function(cond){
      #Mensaje de error
      message(paste("El directorio no existe:", path))
      #Valor a retornar en caso de error
      return(NA)
    },
    #Devolución de advertencia
    warning = function(cond){
      #Mensaje de advertencia
      message(paste("El intento de apertura del directorio ha causado una advertencia:", path))
      #Valor a retornar en caso de advertencia
      return(NA)
    },
    #Mensaje final de ejecución
    finally = {
      message(paste("Directorio abierto:", path))
    }
  )
  #Devolver matriz de objetos
  return(out)
}


#Competencias duras por área
com_plt <- function(df, no, area, esco, pais){
  #Generar dataframe con información
  df <- df %>% 
    filter(!Competencias_demandadas == "NA") %>%
    mutate(Competencias_demandadas = str_split(Competencias_demandadas, " ",)) %>%
    unnest(Competencias_demandadas) %>% 
    group_by(Competencias_demandadas) %>%
    tally() %>%
    dplyr::rename(Competencia = Competencias_demandadas, Value = n) %>%
    slice_max(Value, n = no) 
  
  #Generar gráfico de posiciones
  plt1 <- ggpubr::ggdotchart(df, x = "Competencia", y = "Value",
                     color = "#00AFBB",
                     add = "segments",  
                     sorting = "descending",
                     rotate = TRUE, 
                     dot.size = 16,
                     label = df$Value,
                     font.label = list(color = "white", size = 8, 
                                       vjust = 0.5),               
                     ggtheme = theme_pubclean()) +
    ylab("\nCantidad de vacantes") + xlab("Competencias demandadas\n") +
    ggtitle(paste0("Competencias técnicas más demandadas al nivel", " ", esco,"\n", area)) +
    theme_app_iyf
  
  #Imprimir grafica generada
  plot(plt1)
}


#Habilidades suaves por área
sk_plt <- function(df, no, area, esco, pais){
  
  #Generar dataframe con información
  df <- df %>% 
    filter(!SoftSkills_demandadas == "NA") %>%
    mutate(SoftSkills_demandadas = str_split(SoftSkills_demandadas, " ",)) %>%
    unnest(SoftSkills_demandadas) %>% 
    group_by(SoftSkills_demandadas) %>%
    tally() %>%
    dplyr::rename(Habilidad = SoftSkills_demandadas, Value = n) %>%
    slice_max(Value, n = no) 
  
  #Generar gráfico de posiciones
  plt2 <- ggpubr::ggdotchart(df, x = "Habilidad", y = "Value",
                     color = "#00AFBB",
                     add = "segments",  
                     sorting = "descending",
                     rotate = TRUE, 
                     dot.size = 16,
                     label = df$Value,
                     font.label = list(color = "white", size = 8, 
                                       vjust = 0.5),               
                     ggtheme = theme_pubclean()) +
    xlab("\nCantidad de vacantes") + ylab("Competencias demandadas\n") +
    ggtitle(paste0("Habilidades suaves más demandadas al nivel", " ", esco,"\n", area)) +
    theme_app_iyf
  
  #Imprimir gráfica generada
  plot(plt2)
}

#Función para hacer subset por país
pais_sbs <- function(df, pais){
  #Hacer subset por país 
  df <- df %>%
    filter(Pais == pais)
}

#Función para hacer subset dadas área profesional y nivel de escolaridad
esco_sbs <- function(df, area, nvl){
  
  #Definir si hay subset por nivel de escolaridad
  if(nvl != "Todos"){
  df <- df %>%
    filter(Area.Amplia == area) %>%
    filter(str_detect(Educativo, nvl))
  } else{
    df <- df %>%
      filter(Area.Amplia == area)
  }
}


#Función para modificar la columna de escolaridad demandada
edu_rd <- function(df){
  
  #Modificar variable de nivel educativo
  df <-  df %>%
    mutate(Educativo = paste(Educacion, Educativo, sep = " ")) %>%
    mutate(Educativo = stri_trans_general(Educativo, id = "Latin-ASCII")) %>%
    mutate(Educativo = str_replace_all(Educativo, "[^[:alnum:]]", " ")) %>%
    mutate(Educativo = str_remove_all(Educativo, "C ")) %>%
    mutate(Educativo = gsub("\\s+", " ", str_trim(Educativo))) %>%
    mutate(Educativo = str_replace_all(Educativo, 
                                       c("Ingeniero" = "Ingenieria", "Licenciado" = 
                                           "Licenciatura", "Tecnico" = "Tecnica", 
                                         "Practicante" =  "Practicas", "Universidad" = 
                                           "Licenciatura", "Universitario" = "Licenciatura", 
                                         "Pasante" = "Practicas", "Tecnologo" = "Tecnica",
                                         "Preparatoria" = "Bachillerato", "Bachelor" = "Licenciatura",
                                         "Bachelor Degree" = "Licenciatura", "Engineer" = "Ingenieria",
                                         "Technical" = "Tecnica", "Engineer Degree" = "Ingenieria", 
                                         "High School" = "Bachillerato", "Training Degree" = "Practicas", 
                                         "University Degree" = "Licenciatura", "University" = "Licenciatura", 
                                         "Technical Degree" = "Técnica", "Preparatoria Degree" = "Bachillerato",
                                         "Technician" = "Tecnica", "Technical Training" = "Tecnica"))) %>%
    mutate(Educativo = sapply(strsplit(Educativo, ' '), function(i)paste(unique(i), collapse = ',')))
  
}


#Función para el conteo de vacantes por escolaridad
scl_cnt <- function(df){
  
  #Hacer resumen de menciones por nivel educativo
  edu_cnt <- df %>% 
    tally() %>% 
    summarise(sum(n))
  
  #Almacenar resultado como vector
  edu_cnt <- as.vector(edu_cnt)
  return(edu_cnt)
}


#Función para estimar el salario por escolaridad
scl_wg <- function(df){
  
  #Hacer resumen de menciones por nivel educativo
  edu <-  df %>% 
    filter(is.na(Salario_lo) == F & is.na(Salario_up) == F) %>%
    mutate(Salario_mid = ifelse(is.na(Salario_up) == F, (as.numeric(Salario_lo) + as.numeric(Salario_up))/2, Salario_lo)) %>%
    dplyr::select(c(Educativo, Salario_mid)) %>% 
    summarise(mean(Salario_mid)) %>%
    mutate(`mean(Salario_mid)` = round(`mean(Salario_mid)`, 2))
  
  #Almacenar resultado como vector
  edu_cnt <- as.vector(edu)
  return(edu_cnt)
}


#Función para estimar la experiencia promedio por escolaridad
scl_exp <- function(df){
  
  #Hacer resumen de menciones por nivel educativo
  edu <- df %>% 
    dplyr::select(Educativo, Experiencia_yr) %>%
    group_by(Educativo)%>% 
    filter(is.na(Experiencia_yr) == F) %>%
    summarise(mean(as.numeric(Experiencia_yr))) %>%
    summarise(mean(`mean(as.numeric(Experiencia_yr))`)) %>%
    dplyr::rename(mean_wg = 1) %>%
    mutate(mean_wg = round(mean_wg, 1)) 
    
  
  #Almacenar resultado como vector
  edu_cnt <- as.vector(edu)
  return(edu_cnt)
}


#Función para visualización de tendencia de empleos
ten_ar <- function(df, esco, area, pais){
  
  #Generar grafico de tendencia de publicaciones
  plt5 <- df %>%
    mutate(Fecha.de.Publicacion = date(Fecha.de.Publicacion)) %>%
    mutate(Vec = 1) %>%
    group_by(Mes = lubridate::floor_date(Fecha.de.Publicacion, "month")) %>%
    dplyr::summarise(sum(Vec)) %>%
    dplyr::rename(Value = `sum(Vec)`) %>%
    ggplot(aes(Mes, Value)) +
    geom_line(color = "#00AFBB") +
    geom_point() +
    ggtitle(paste("Oferta mensual de posiciones para",esco,"en",pais,"\n",area)) +
    xlab("\nMes") + ylab("Cantidad de vacantes\n") +
    scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
                 date_labels = "%B-%y") +
    theme_pubclean() +
    theme_app_iyf
  
  #Retornar grafico generado
  plot(plt5)
}


#Función para hacer subset dado programa seleccionado
pro_sbs <- function(ex, df, pro){
  
  #Definir universo de busqueda
  ex <- ex %>%
    filter(grepl(pro, Programa))
  
  #Definir función de matching
  pr_ma <- ex %>% 
    dplyr::select(Inicial_P) %>% 
    distinct()
  
  #Definir lista de trayectos
  pr_tr <- ex %>%
    dplyr::select(Inicial_T) %>%
    mutate(Inicial_T = str_c(Inicial_T, collapse = "|")) %>%
    distinct()
  
  #Filtrar por programa seleccionado y remover trayectos foraneos
  df <- df %>%
    filter(Pais == "Mexico") %>%
    filter(grepl(pr_ma$Inicial_P, Programa)) %>%
    mutate(Trayecto = str_extract_all(Trayecto, pr_tr$Inicial_T)) %>%
    mutate(Trayecto = str_remove_all(Trayecto, "character(0)|c|[:punct:]")) %>%
    mutate(Trayecto = strsplit(Trayecto, " ")) %>%
    unnest(Trayecto) %>%
    filter(grepl(pr_tr$Inicial_T, Trayecto)) %>%
    mutate(Trayecto = trimws(Trayecto))
  
  #Generar vector de nombres de trayectos
  {
    #Definir vector vacío
    tra <- c()
    #Generar vector en loop
    for(i in 1:nrow(df)){
      ix <- ex$Trayecto[ex$Inicial_T == df$Trayecto[i]]
      tra[i] <- ix
      }
    }
  
  #Reemplazar rotulos de trayectos con nombre completo
  df <- df %>% mutate(Trayecto = tra)
}


#Generar función para visualizar demanda de trayectos por sector
tra_sec <- function(df, pro){
  
  #Generar tabla de datos
  df <- df %>%
    group_by(Trayecto, Area.Amplia) %>%
    tally() %>%
    dplyr::rename(Value = n) %>%
    arrange(Trayecto, desc(Area.Amplia)) %>%
    mutate(Area.Amplia = as.factor(Area.Amplia)) %>%
    dplyr::top_n(5, Value)
  
  #Generar grafico de posiciones
  plt3 <- ggdotchart(df, "Area.Amplia", "Value",
                     color = "Trayecto",
                     palette = c("#00AFBB", "#E7B800", "#FC4E07", "brown"),
                     sorting = "descending", 
                     add = "segment", 
                     add.params = list(color = "lightgray", size = 1.5),
                     #rotate = T,
                     group = "Trayecto",
                     dot.size = 8.5,
                     label = df$Value, 
                     font.label = list(color = "black", 
                                       size = 8, vjust = 0.5, hjust = 0.8),   
                     position = position_dodge(0.5),
                     repel = T,
                     label.rectangle = T,
                     ggtheme = theme_pubclean()) +
    xlab("\nÁrea profesional") + ylab("Cantidad de vacantes\n") +
    ggtitle(paste0("Demanda de trayectos de programa por sector\n", pro)) +
    theme_app_iyf
  
  #Imprimir grafico generado
  plot(plt3)
}


#Función generadora de lista de competencias por programa
hks_gen <- function(num, df){
  
  #Definir si se trata de habilidades duras
  if(num == 1){
    #Generar lista de competencias
    hsk_ls <- df %>%
      filter(!Competencias_demandadas == "NA") %>%
      mutate(Competencias_demandadas = str_split(Competencias_demandadas, " ",)) %>%
      unnest(Competencias_demandadas) %>% 
      group_by(Competencias_demandadas) %>%
      select(Competencias_demandadas) %>%
      distinct() %>%
      arrange(Competencias_demandadas) %>%
      as.list()
  }
      #Proceso para habilidades suaves
      else{
        #Generar lista de competencias
        hsk_ls <- df %>%
          filter(!SoftSkills_demandadas == "NA") %>%
          mutate(SoftSkills_demandadas = str_split(SoftSkills_demandadas, " ",)) %>%
          unnest(SoftSkills_demandadas) %>% 
          group_by(SoftSkills_demandadas) %>%
          select(SoftSkills_demandadas) %>%
          distinct() %>%
          arrange(SoftSkills_demandadas) %>%
          as.list()
      }
    #Convertir en vector y colocar en entorno global
    hsk_ls <- unlist(hsk_ls)
}


#Función para visualizar reglas de asociación entre competencias
com_gra <- function(num, df, no, com){
  
  #Proceso para habilidades duras
  if(num == 1){
  
  #Seleccionar vector de competencias
  df_hrd <- df %>%  
    dplyr::select(`ID`, `Competencias_demandadas`) %>%
    mutate(Competencias_demandadas = trimws(Competencias_demandadas)) %>%
    mutate(Competencias_demandadas = str_replace_all(Competencias_demandadas, " ", ","))
    } 
    #Proceso para habilidades suaves
    else{
      df_hrd <- df %>%  
        dplyr::select(`ID`, `SoftSkills_demandadas`) %>%
        mutate(SoftSkills_demandadas = trimws(SoftSkills_demandadas)) %>%
        mutate(SoftSkills_demandadas = str_replace_all(SoftSkills_demandadas, " ", ","))
      }
  #Generar lista con dataframe
  df_hrd <- apply(df_hrd[-1], 1, function(x) x[x!= ""])
  #names(df_hrd) <- df_t$ID
  
  #Eliminar filas con elementos en blanco
  df_hrd <- df_hrd[lapply(df_hrd, length)>0]
  
  #Separar palabras
  df_hrd <- sapply(df_hrd, strsplit, split = ",")
  
  #Generar objeto tipo transactions
  df_hrd <- as(df_hrd, "transactions")
  
  #Algoritmo a priori para competencias específicas
  hrd_rules_sp <-  arules::apriori(df_hrd, parameter = 
                                     list( supp=0.0001,conf=0.0005, maxlen=10, target= "rules"), 
                                           appearance = list(lhs = com, default="rhs"))
  
  #Visualizar elementos con el lift más significativo
  plt4 <- plot(head(hrd_rules_sp, no,  by = "confidence"), method = 
                 "paracoord", main = paste("Competencias asociadas a", com))
}


#Función para visualización de principales titulos de vacantes
pro_cld <- function(df, pro, no){
  
  #Función generadora de bigramas
  tdm.bigram <- df %>%
    mutate(Titulo = removeWords(Titulo, stopwords("spanish"))) %>%
    unnest_tokens(bigram, Titulo, token = "ngrams", n = 3) %>%
    filter(!is.na(bigram)) %>%
    dplyr::count(Trayecto, bigram, sort = T) %>%
    group_by(Trayecto) %>%
    top_n(no, n)
  
  #Representar gráficamente por trayecto
  ggplot(tdm.bigram, aes(n, bigram)) +
    geom_bar(stat = "identity", 
             color = "#00AFBB",
             fill = "#00AFBB") +
    facet_wrap(~Trayecto, scales = "free") +
    ggtitle(paste0("Principales posiciones ofertadas por trayecto\n",pro)) +
    xlab("\nCantidad de vacantes") + ylab("Titulo del puesto\n") +
    theme_pubclean() +
    theme_app_iyf
}


#Función para visualizar demanda sectorial por estado
sec_est <- function(df, est, no, pais){
  
  #Generar tabla de demanda sectorial
  df <- df %>%
    filter(Estado == est) %>%
    group_by(Area.Amplia) %>%
    tally() %>%
    arrange(desc(n)) %>%
    top_n(no, n)
  
  #Generar gráfico de posiciones
  plt6 <- ggpubr::ggdotchart(df, x = "Area.Amplia", y = "n",
                             color = "#00AFBB",
                             add = "segments",  
                             sorting = "descending",
                             rotate = TRUE, 
                             dot.size = 16,
                             label = df$n,
                             font.label = list(color = "white", size = 8, 
                                               vjust = 0.5),               
                             ggtheme = theme_pubclean()) +
    xlab("Área profesional\n") + ylab("\nCantidad de vacantes") +
    ggtitle(paste0("Distribución de la demanda sectorial\n", est, ",", pais)) +
    theme_app_iyf
  
  #Imprimir grafico
  plot(plt6)
}


#Función para visualizar patrones territoriales de la demanda por sector 
map_sec <- function(pais, ux_mx, ux_cl, df, capas_mx, capas_col, area){
  
  if(pais == "Mexico"){
    
    #Unificar datos globales con claves espaciales
    df <- join(df, ux_mx, by = "Estado")
    
    #Generar tabla de demanda estatal del sector
    df <- df %>%
      filter(Area.Amplia == area) %>%
      mutate(Counter = 1) %>%
      group_by(Clave) %>%
      tally(Counter) %>%
      dplyr::rename(CVE_ENT = Clave, Value = n) 
    
   #Unir frecuencias con datos espaciales
   capas_mx <- inner_join(capas_mx, df, by="CVE_ENT")
   
   #Reemplazar valores cero para graficar
   capas_mx <- capas_mx %>%
     mutate(Value = tidyr::replace_na(Value, 0))  
   
   #Generar mapa con datos
   plt7.1 <-  capas_mx %>%
     ggplot() +  
     geom_polygon(aes(x=long, y=lat, group=group, fill=Value)) + 
     theme_pubclean() +
     theme_app_iyf +
     scale_fill_gradient2(low = "lightgray", mid = "#00AFBB", high = "navy") +
     labs(title = paste0("Demanda sectorial al nivel subnacional\n", area),
          fill = "Cantidad de vacantes") +
     theme(axis.text = element_blank(),
           axis.title = element_blank(),
           axis.ticks = element_blank(),
           legend.text = element_text(angle = 45)) 
   
   #Imprimir mapa generado
   plot(plt7.1)
    
  } else if(pais == "Colombia"){
    
    #Unificar datos globales con claves espaciales
    df <- join(df, ux_cl, by = "Estado")
    
    #Generar tabla de demanda estatal del sector
    df <- df %>%
      filter(Area.Amplia == area) %>%
      mutate(Counter = 1) %>%
      group_by(Clave) %>%
      tally(Counter) %>%
      dplyr::rename(id = Clave, Value = n) %>%
      mutate(id = as.character(id)) 
    
    #Unir frecuencias con datos espaciales
    capas_col <- inner_join(capas_col, df, by="id")
    
    #Reemplazar valores cero para graficar
    capas_col <- capas_col %>%
                    mutate(Value = tidyr::replace_na(Value, 0)) 
    
    #Generar mapa con datos
    plt7.2 <-  capas_col %>%
      ggplot() +  
      geom_polygon(aes(x=long, y=lat, group=group, fill=Value)) + 
      theme_pubclean() +
      theme_app_iyf +
      scale_fill_gradient2(low = "lightgray", mid = "#00AFBB", high = "navy") +
      labs(title = paste0("Demanda sectorial al nivel subnacional\n", area),
           fill = "Cantidad de vacantes") +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            legend.text = element_text(angle = 45)) 
    
    #Imprimir mapa generado
    plot(plt7.2)

  }
}

#Función para visualizar patrones territoriales de la demanda de programas de IYF
map_pro <- function(ux, ex, df, capas, pro){
  
  #Unificar datos globales con claves espaciales
  df <- join(df, ux, by = "Estado")
  
  #Definir universo de busqueda
  ex <- ex %>%
    filter(grepl(pro, Programa))
  
  #Definir función de matching
  pr_ma <- ex %>% 
    dplyr::select(Inicial_P) %>% 
    distinct()
  
  #Generar tabla de demanda estatal del sector
  df <- df %>%
    filter(grepl(pr_ma$Inicial_P, Programa)) %>%
    mutate(Counter = 1) %>%
    group_by(Clave) %>%
    tally(Counter) %>%
    dplyr::rename(CVE_ENT = Clave, Value = n)
  
  #Unir frecuencias con datos espaciales
  capas <- inner_join(capas, df, by="CVE_ENT")
  
  #Generar mapa con datos
  plt8 <-  capas %>%
    ggplot() +  
    geom_polygon(aes(x=long, y=lat, group=group, fill=Value)) + 
    theme_pubclean() +
    theme_app_iyf +
    scale_fill_gradient2(low = "lightgray", mid = "#00AFBB", high = "navy") +
    labs(title = paste0("Demanda de programas de IYF al nivel estatal\n", pro),
         fill = "Cantidad de vacantes") +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(angle = 45))
  
  #Imprimir mapa generado
  plot(plt8)
}
