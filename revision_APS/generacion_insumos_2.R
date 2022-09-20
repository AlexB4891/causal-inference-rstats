library(tidyverse)
library(ggrepel)
library(patchwork)

lista_tablas <- c("balanced",
                  "unbalanced",
                  "semibalanced") %>% 
  set_names() %>% 
  str_c("revision_APS/data/",.,"/") %>% 
  map(list.files,full.names = TRUE) %>% 
  transpose() %>% 
  map(unlist)


tablas_lectura <- lista_tablas %>% 
  map_depth(2,~{
    
    file <- .x
    
    tipo <- str_remove(string = .x,
                       pattern = "revision_APS/data/") %>% 
      str_remove("/.+")
    
    # list(
      read_tsv(file) %>% 
        mutate(panel = tipo)
    # )
    
  })

tabla_resumen_APS <- tablas_lectura %>% 
  map(reduce,bind_rows) %>% 
  reduce(inner_join)

plots_declaracion <- map2(.x = c("dummy_aps_declarado",
           "dummy_en_ruc",
           "dummy_en_101",
           "dummy_en_ruc_101"),
     .y = c("Sociedades nformantes del Anexo APS",
            "S. Informantes registrados en el RUC",
            "S. Informantes que declararon impuesto a la renta",
            "S. Informantes en RUC y declarantes de IR"),
    .f = ~{
      
      tabla_resumen_APS %>% 
        mutate(across(.cols = .x,~.x/1000),
               across(.cols = .x,list(label = ~if_else(anio_fiscal %in% c(2012,2015,2019),.x,NA_real_)),.names = "{.fn}")) %>% 
        # select(label)
        
        ggplot(aes_string(x = "anio_fiscal",y = .x,color = "panel")) +
        geom_line() +
        geom_point(size = 2) +
        geom_label_repel(aes_string(label = "label")) +
        scale_color_manual(values = c("balanced" = "#900C3F",
                                      "unbalanced" = "#5A499A",
                                      "semibalanced" = "#1B3F83")) +
        theme_minimal() +
        labs(title = .y,
             y = "Informantes") +
        theme(axis.title.x = element_blank())
      
    })

cumplimiento <- patchwork::wrap_plots(plots_declaracion[c(1,4)],guides = "collect")  +
  patchwork::plot_annotation(title = "Figura 1. Cumplimiento de las obligaciones tributarias por parte de las sociedades",
                             subtitle = "El eje verticial se presenta como miles de informantes",
                             caption = "Fuente: Anexos APS, formularios 101, y RUC",
                             tag_levels = c("a","b"),
                             tag_suffix = ")") &
  theme(legend.position = "bottom")


ggsave(filename = "revision_APS/f1_cumplimiento.png",plot = cumplimiento,width = 12)

names(tabla_resumen_APS)


plots_completos <- map2(.x = c("dummy_completo",
                               "dummy_completo_101" ),
  .y = c("Informantes con APS completo (100%)",
         "I. con APS completos y con declaración de IR"),
  .f = ~{
    
    tabla_resumen_APS %>% 
      mutate(across(.cols = .x,~.x/1000),
             across(.cols = .x,list(label = ~if_else(anio_fiscal %in% c(2012,2015,2019),.x,NA_real_)),.names = "{.fn}")) %>% 
      # select(label)
      
      ggplot(aes_string(x = "anio_fiscal",y = .x,color = "panel")) +
      geom_line() +
      geom_point(size = 2) +
      geom_label_repel(aes_string(label = "label")) +
      scale_color_manual(values = c("balanced" = "#900C3F",
                                    "unbalanced" = "#5A499A",
                                    "semibalanced" = "#1B3F83")) +
      theme_minimal() +
      labs(title = .y,
           y = "Informantes") +
      theme(axis.title.x = element_blank())
    
  })

completos <- patchwork::wrap_plots(plots_completos,guides = "collect")  +
  patchwork::plot_annotation(title = "Figura 2. Sociedades que declaran el 100% de su participación societaria",
                             subtitle = "El eje verticial se presenta como miles de informantes",
                             caption = "Fuente: Anexos APS, formularios 101, y RUC",
                             tag_levels = c("a","b"),
                             tag_suffix = ")") &
  theme(legend.position = "bottom")


ggsave(filename = "revision_APS/f2_cumplimiento.png",plot = completos,width = 12)
