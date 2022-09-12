library(tidyverse)
library(ggrepel)

make_chunks <- function(directorio = character(),
                        expr_reg = character(),
                        tipo = "adivinar"){
  
  
  archivos <- list.files(path = directorio,
                         pattern = expr_reg,
                         full.names = TRUE)
  
  if(tipo == "imagen"){
    texto <- map(archivos,
                ~ str_c(
                  "```{r}",
                  "knitr::include_graphics(path = '",.x,"')",
                  "```"
                )) 
    
  }else {
  
    
    tabla <- archivos %>% 
      map(read_tsv) %>% 
      reduce(inner_join) %>% 
      pivot_longer(cols = -anio_fiscal,
                   names_to ="variable",
                   values_to =  "conteo") %>% 
      mutate(variable = str_remove(variable,"dummy_") %>% 
               str_replace_all("_"," ") %>% 
               str_to_sentence())
    
    plot <- tabla %>% 
      ggplot(mapping = aes(x = anio_fiscal,y = conteo,color = variable)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      geom_label_repel(aes(label = scales::number(conteo)),show.legend = FALSE) +
      scale_y_continuous(name = "Empresas informantes",labels = scales::number) +
      scale_color_brewer(palette = 6,type = "qual") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "transparent"),
            legend.position = "top",
            legend.title = element_blank(),
            axis.title.x = element_blank())
  
      
  }
  
  if(tipo == "conteo_dummy_dummy_completo"){
  
    plot <- plot +
      labs(title = "Figura 1. Número de anexos APS completos")
    
    
    ggsave(filename = "revision_APS/conteo_dummy_dummy_completo.png",
           plot = plot)
    
    texto <- c(
      "```{r}",
      "knitr::include_graphics(path = 'revision_APS/conteo_dummy_dummy_completo.png'')",
      "```"
    )
  }else if(tipo == "conteo_dummy_dummy_incompleto"){
    
    plot <- plot +
      labs(title = "Figura 2. Número de anexos APS incompletos")
    
    
    ggsave(filename = "revision_APS/conteo_dummy_dummy_incompleto.png",
           plot = plot)
    
    texto <- c(
      "```{r}",
      "knitr::include_graphics(path = 'revision_APS/conteo_dummy_dummy_incompleto.png')",
      "```"
    )
  }else if(tipo == "conteo_dummy_dummy_pf50"){
    
    plot <- plot +
      labs(title = "Figura 3. Número de anexos APS con participación en paraisos fiscales",
           subtitle = "Porcentaje igual o mayor al 50%")
    
    ggsave(filename = "revision_APS/conteo_dummy_dummy_pf50.png",
           plot = plot)
    
    texto <- c(
      "```{r}",
      "knitr::include_graphics(path = 'revision_APS/conteo_dummy_dummy_pf50.png'')",
      "```"
    )
    
  }else if(tipo == "conteo_dummy_dummy_pfparcial"){
    
    plot <- plot +
      labs(title = "Figura 4. Número de anexos APS con participación en paraisos fiscales",
           subtitle = "Porcentaje menr al 50%")
    
    ggsave(filename = "revision_APS/conteo_dummy_dummy_pfparcial.png",
           plot = plot)
    
    texto <- c(
      "```{r}",
      "knitr::include_graphics(path = 'revision_APS/conteo_dummy_dummy_pfparcial.png')",
      "```"
    )
  } 
  
  return(texto)
  
}

map(.x = c("imagen",
  "conteo_dummy_dummy_completo",
  "conteo_dummy_dummy_incompleto",
  "conteo_dummy_dummy_pf50",
  "conteo_dummy_dummy_pfparcial"),
  .f = ~{ 
 
    text <- .x
    
    make_chunks(
    directorio = "revision_APS/data",
    expr_reg = text,
    tipo = text)
})

make_chunks(directorio = "revision_APS/data",
            expr_reg = "distribucion_variacion",
            tipo = "imagen")

directorio <- list.files(path = "revision_APS/data/",
                         pattern = "conteo_variacion",
                         full.names = TRUE)

tabla_total <- directorio %>% 
  map(read_tsv) %>% 
  reduce(bind_rows)


tabla_total <- tabla_total %>% 
  mutate(index_fact = case_when(index & index2 ~ "Entre -100& y 100%",
                                !index & !index2 ~ "Mayor a 200%",
                                index & !index2 ~ "Entre 100% y 200%"),
         index_fact = factor(index_fact,levels = c("Entre -100& y 100%",
                                                   "Entre 100% y 200%",
                                                   "Mayor a 200%"
                                                   ))) 


plot_total <- tabla_total %>% 
  split(.$index_fact) %>% 
  map( ~ .x %>% 
         ggplot(aes(x = anio,y = n,color = index_fact)) +
         geom_line(size = 1) +
         geom_point(size = 3) +
         geom_label_repel(aes(label = scales::number(n)),show.legend = FALSE) +
         scale_y_continuous(name = "Empresas informantes",labels = scales::number) +
         scale_color_brewer(palette = 6,type = "qual") +
         theme_minimal() +
         theme(plot.background = element_rect(fill = "transparent"),
               legend.position = "top",
               legend.title = element_blank(),
               axis.title.x = element_blank()) 
         )
  

plot_1 <- plot_total[[1]] + 
  labs(title = "Figura 5. Intervalos en la distribución de cambios en la declaración del APS",
       subtitle = "Entre -100% y 100%")

ggsave(filename = "revision_APS/intervalos_distribucion_1.png",plot = plot_1)
  
  
plot_1 <- plot_total[[2]] +
  labs(title = "Figura 6. Intervalos en la distribución de cambios en la declaración del APS",
       subtitle = "Entre 100% y 200%")

ggsave(filename = "revision_APS/intervalos_distribucion_2.png",plot = plot_1)

plot_1 <- plot_total[[3]] + 
  labs(title = "Figura 5. Intervalos en la distribución de cambios en la declaración del APS",
       subtitle = "Mayor a 200%")

ggsave(filename = "revision_APS/intervalos_distribucion_3.png",plot = plot_1)
