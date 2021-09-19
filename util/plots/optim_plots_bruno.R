arquivos_leitura <- list.files()[str_detect(list.files(), ".rds")]

objetos <- lapply(arquivos_leitura, readRDS)

objetos_modificados <- lapply(objetos, function(x){
  x + theme(axis.text = element_text(size = 20), 
            axis.title = element_text(size = 20), 
            title = element_text(size = 25), 
            legend.text = element_text(size = 13))
})

categ_interesse <- unique(objetos_modificados[[3]]$data$.path)

objetos_modificados[[3]]$data %>% 
  filter(.path != categ_interesse[2]) %>% 
  ggplot() + 
  theme_bw() + 
  labs(title = "Global Surrogate") +
  geom_bar() + 
  aes(x = .class) + 
  facet_wrap(~.path, ncol = 3) + 
  theme(axis.text = element_text(size = 20), 
          axis.title = element_text(size = 20), 
          title = element_text(size = 25), 
          legend.text = element_text(size = 13))

nomes_arq <- list.files('result/')[-2]

nomes_arq_mod <- nomes_arq %>% 
  str_split("\\.") %>% 
  lapply(function(xxx){
    paste0(xxx[1], "_aux.", xxx[2])
  }) %>% unlist()

lapply(1:length(objetos_modificados), function(x){
  ggsave(paste0("result/", nomes_arq_mod[x]), 
         objetos_modificados[[x]], 
         width = 10, 
         height = 10)
}) %>% invisible()




