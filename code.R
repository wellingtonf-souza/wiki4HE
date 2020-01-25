
rm(list = ls(all = T))

#=======================
# Pacotes
#=======================

library(tidyverse)
library(viridis)
library(plotly)
library(heatmaply)
library(dunn.test)
library(factoextra)
library(naniar)
library(mice)

#=======================
# Lendo os dados
#=======================

dados = read.csv2("wiki4HE.csv",na.strings = "?")
head(dados,3)
dim(dados)

#=======================
# Funções
#=======================

scale_likertI = function(x){
  x = case_when(x==1~"Discordo totalmente",
                x==2~"Discordo parcialmente",
                x==3~"Não concordo, nem discordo",
                x==4~"Concordo parcialmente",
                x==5~"Concordo totalmente")
  x = factor(x, levels = c("Concordo totalmente",
                           "Concordo parcialmente",
                           "Não concordo, nem discordo",
                           "Discordo parcialmente",
                           "Discordo totalmente"))
  return(x)
}

scale_likertII = function(x){
  x = case_when(x==1~"Nunca",
                x==2~"Quase nunca",
                x==3~"Às vezes",
                x==4~"Quase sempre",
                x==5~"Sempre")
  x = factor(x, levels = c("Sempre",
                           "Quase sempre",
                           "Às vezes",
                           "Quase nunca",
                           "Nunca"))
  return(x)
  
}

#==============================================================
# Fazendo algumas codificações para as analises posteriores
#==============================================================

dados = dados %>% 
  mutate(GENDER = case_when(GENDER==0~"Masculino",
                            GENDER==1~"Femenino"),
         DOMAIN = case_when(DOMAIN==1~"Artes e Humanidades",
                            DOMAIN==2~"Ciências",
                            DOMAIN==3~"Ciências da Saúde",
                            DOMAIN==4~"Engenharia e Arquitetura",
                            DOMAIN==5~"Direito e Política",
                            DOMAIN==6~"Ciências Sociais"),
         PhD = case_when(PhD==0~"Não",
                         PhD==1~"Sim"),
         UNIVERSITY = case_when(UNIVERSITY==1~"UOC",
                                UNIVERSITY==2~"UPF"),
         UOC_POSITION = case_when(UOC_POSITION==1~"Professor",
                                  UOC_POSITION==2~"Associado",
                                  UOC_POSITION==3~"Assistente",
                                  UOC_POSITION==4~"Palestrante",
                                  UOC_POSITION==5~"Instrutor",
                                  UOC_POSITION==6~"Adjunto"),
         OTHER_POSITION = case_when(OTHER_POSITION==1~"Professor",
                                    OTHER_POSITION==2~"Associado",
                                    OTHER_POSITION==3~"Assistente",
                                    OTHER_POSITION==4~"Palestrante",
                                    OTHER_POSITION==5~"Instrutor",
                                    OTHER_POSITION==6~"Adjunto"),
         # OTHERSTATUS nao esta definida na descricao e as categorias nao condizem com OTHER
         USERWIKI = case_when(USERWIKI==0~"Não",
                              USERWIKI==1~"Sim")
         ) %>% 
  dplyr::select(-OTHERSTATUS)

dados = dados %>% magrittr::set_colnames(c("Idade","Gênero","Domínio",names(dados[,-c(1,2,3)])))

g1 = ggplot(dados,mapping = aes(Idade))+
  geom_density(fill = "#6f42c1",alpha=0.2)+
  xlab("Idade")+
  theme_minimal()
ggplotly(g1)

g2 = ggplot(dados,mapping = aes(YEARSEXP))+
  geom_density(fill = "#6f42c1",alpha=0.2)+
  xlab("Anos de experiência no ensino universitário")+
  theme_minimal()
ggplotly(g2)

dados %>% 
  group_by(Gênero) %>% 
  summarise(Quant = n()) %>% 
  plot_ly(labels = ~Gênero, values = ~Quant) %>% 
  add_pie(hole = 0.7) %>% 
  layout(title= "Gênero")

dados %>% 
  mutate(Domínio = case_when(is.na(Domínio)~"Resposta Ausente",TRUE~Domínio)) %>% 
  group_by(Domínio) %>% 
  summarise(Quant = n()) %>% 
  plot_ly(labels = ~Domínio, values = ~Quant) %>% 
  add_pie(hole = 0.7) %>% 
  layout(title= "Domínio")

dados %>% 
  group_by(PhD) %>% 
  summarise(Quant = n()) %>% 
  plot_ly(labels = ~PhD, values = ~Quant) %>% 
  add_pie(hole = 0.7) %>% 
  layout(title= "PhD")

dados %>% 
  group_by(UNIVERSITY) %>% 
  summarise(Quant = n()) %>% 
  plot_ly(labels = ~UNIVERSITY, values = ~Quant) %>% 
  add_pie(hole = 0.7) %>% 
  layout(title= "Universidade")

dados %>% 
  mutate(UOC_POSITION = case_when(is.na(UOC_POSITION)~"Resposta Ausente",TRUE~UOC_POSITION)) %>%
  group_by(UOC_POSITION) %>% 
  summarise(Quant = n()) %>% 
  plot_ly(labels = ~UOC_POSITION, values = ~Quant) %>% 
  add_pie(hole = 0.7) %>% 
  layout(title= "Posição Acadêmica")

dados %>% 
  mutate(USERWIKI = case_when(is.na(USERWIKI)~"Resposta Ausente",TRUE~USERWIKI)) %>%
  group_by(USERWIKI) %>% 
  summarise(Quant = n()) %>% 
  plot_ly(labels = ~USERWIKI, values = ~Quant) %>% 
  add_pie(hole = 0.7) %>% 
  layout(title= "Usuário registrado da Wikipedia")

#==============================================================
# Analise das respostas com escala Likert
#==============================================================

# Utilidade percebida

g3 = dados %>% 
  dplyr::select(starts_with("PU")) %>% 
  magrittr::set_colnames(c("O uso da Wikipedia facilita para os alunos o desenvolvimento de novas habilidades",
                           "O uso da Wikipedia melhora o aprendizado dos alunos",
                           "A Wikipedia é útil para ensinar")) %>% 
  purrr::map(scale_likertI) %>% 
  bind_cols() %>% 
  pivot_longer(everything(),values_to = "Likert",
               names_to = "var") %>% 
  group_by(var,Likert) %>% 
  na.omit() %>% 
  summarise(contagem = n()) %>% 
  mutate(prop = round(contagem/sum(contagem)*100,2)) %>% 
  ggplot(mapping = aes(var,contagem,fill=Likert,text = sprintf("Categoria: %s<br>Contagem:%s<br>Porcentagem:%s%%",
                                                               Likert,contagem,prop)))+
  geom_bar(position="fill", stat="identity")+
  ggtitle("Utilidade percebida")+
  scale_fill_viridis(discrete = T)+
  theme_minimal()+
  ylab("%")+
  xlab("")+
  coord_flip()
ggplotly(g3,tooltip = "text")  %>% layout(showlegend = FALSE)

# facilidade de uso percebida

g4 = dados %>% 
  dplyr::select(starts_with("PEU")) %>% 
  magrittr::set_colnames(c("A Wikipedia é fácil de usar",
                           "É fácil encontrar na Wikipedia as informações que você procura",
                           "É fácil adicionar ou editar informações na Wikipedia")) %>% 
  purrr::map(scale_likertI) %>% 
  bind_cols() %>% 
  pivot_longer(everything(),values_to = "Likert",
               names_to = "var") %>% 
  group_by(var,Likert) %>% 
  na.omit() %>% 
  summarise(contagem = n()) %>% 
  mutate(prop = round(contagem/sum(contagem)*100,2)) %>% 
  ggplot(mapping = aes(var,contagem,fill=Likert,text = sprintf("Categoria: %s<br>Contagem:%s<br>Porcentagem:%s%%",
                                                               Likert,contagem,prop)))+
  geom_bar(position="fill", stat="identity")+
  ggtitle("Facilidade de uso percebida")+
  scale_fill_viridis(discrete = T)+
  theme_minimal()+
  ylab("%")+
  xlab("")+
  coord_flip()
ggplotly(g4,tooltip = "text") 

# Perceived Enjoyment

g5 = dados %>% 
  dplyr::select(starts_with("ENJ")) %>% 
  magrittr::set_colnames(c("O uso da Wikipedia estimula a curiosidade",
                           "O uso da Wikipedia é divertido")) %>% 
  purrr::map(scale_likertI) %>% 
  bind_cols() %>% 
  pivot_longer(everything(),values_to = "Likert",
               names_to = "var") %>% 
  group_by(var,Likert) %>% 
  na.omit() %>% 
  summarise(contagem = n()) %>% 
  mutate(prop = round(contagem/sum(contagem)*100,2)) %>% 
  ggplot(mapping = aes(var,contagem,fill=Likert,text = sprintf("Categoria: %s<br>Contagem:%s<br>Porcentagem:%s%%",
                                                               Likert,contagem,prop)))+
  geom_bar(position="fill", stat="identity")+
  ggtitle("Perceived Enjoyment")+
  scale_fill_viridis(discrete = T)+
  theme_minimal()+
  ylab("%")+
  xlab("")+
  coord_flip()
ggplotly(g5,tooltip = "text")

# Qualidade

g6 = dados %>% 
  dplyr::select(starts_with("Qu")) %>% 
  magrittr::set_colnames(c("Os artigos na Wikipedia são confiáveis",
                           "Os artigos na Wikipedia são atualizados",
                           "Os artigos na Wikipedia são abrangentes",
                           "Na minha área de especialização, a Wikipedia tem uma qualidade inferior a outros recursos educacionais",
                           "Confio no sistema de edição da Wikipedia")) %>% 
  purrr::map(scale_likertI) %>% 
  bind_cols() %>% 
  pivot_longer(everything(),values_to = "Likert",
               names_to = "var") %>% 
  group_by(var,Likert) %>% 
  na.omit() %>% 
  summarise(contagem = n()) %>% 
  mutate(prop = round(contagem/sum(contagem)*100,2)) %>% 
  ggplot(mapping = aes(var,contagem,fill=Likert,text = sprintf("Categoria: %s<br>Contagem:%s<br>Porcentagem:%s%%",
                                                               Likert,contagem,prop)))+
  geom_bar(position="fill", stat="identity")+
  ggtitle("Qualidade")+
  scale_fill_viridis(discrete = T)+
  theme_minimal()+
  ylab("%")+
  xlab("")+
  coord_flip()
ggplotly(g6,tooltip = "text")

# Visibilidade

g7 = dados %>% 
  dplyr::select(starts_with("Vis")) %>% 
  magrittr::set_colnames(c("Wikipedia melhora a visibilidade do trabalho dos alunos",
                           "É fácil ter um registro das contribuições feitas na Wikipedia",
                           "Cito a Wikipedia em meus trabalhos acadêmicos")) %>% 
  purrr::map(scale_likertI) %>% 
  bind_cols() %>% 
  pivot_longer(everything(),values_to = "Likert",
               names_to = "var") %>% 
  group_by(var,Likert) %>% 
  na.omit() %>% 
  summarise(contagem = n()) %>% 
  mutate(prop = round(contagem/sum(contagem)*100,2)) %>% 
  ggplot(mapping = aes(var,contagem,fill=Likert,text = sprintf("Categoria: %s<br>Contagem:%s<br>Porcentagem:%s%%",
                                                               Likert,contagem,prop)))+
  geom_bar(position="fill", stat="identity")+
  ggtitle("Visibilidade")+
  scale_fill_viridis(discrete = T)+
  theme_minimal()+
  ylab("%")+
  xlab("")+
  coord_flip()
ggplotly(g7,tooltip = "text")

# Imagem Social

g8 = dados %>% 
  dplyr::select(starts_with("Im")) %>% 
  magrittr::set_colnames(c("O uso da Wikipedia é bem considerado entre os colegas",
                           "Na academia, o compartilhamento de recursos educacionais abertos é apreciado",
                           "Meus colegas usam a Wikipedia")) %>% 
  purrr::map(scale_likertI) %>% 
  bind_cols() %>% 
  pivot_longer(everything(),values_to = "Likert",
               names_to = "var") %>% 
  group_by(var,Likert) %>% 
  na.omit() %>% 
  summarise(contagem = n()) %>% 
  mutate(prop = round(contagem/sum(contagem)*100,2)) %>% 
  ggplot(mapping = aes(var,contagem,fill=Likert,text = sprintf("Categoria: %s<br>Contagem:%s<br>Porcentagem:%s%%",
                                                               Likert,contagem,prop)))+
  geom_bar(position="fill", stat="identity")+
  ggtitle("Imagem Social")+
  scale_fill_viridis(discrete = T)+
  theme_minimal()+
  ylab("%")+
  xlab("")+
  coord_flip()
ggplotly(g8,tooltip = "text")

# atitude de compartilhamento da Wikipedia

g9 = dados %>% 
  dplyr::select(starts_with("SA")) %>% 
  magrittr::set_colnames(c("É importante compartilhar conteúdo acadêmico em plataformas abertas",
                           "É importante publicar resultados de pesquisas em outras mídias que não sejam periódicos acadêmicos ou livros",
                           "É importante que os alunos se familiarizem com ambientes colaborativos online")) %>% 
  purrr::map(scale_likertI) %>% 
  bind_cols() %>% 
  pivot_longer(everything(),values_to = "Likert",
               names_to = "var") %>% 
  group_by(var,Likert) %>% 
  na.omit() %>% 
  summarise(contagem = n()) %>% 
  mutate(prop = round(contagem/sum(contagem)*100,2)) %>% 
  ggplot(mapping = aes(var,contagem,fill=Likert,text = sprintf("Categoria: %s<br>Contagem:%s<br>Porcentagem:%s%%",
                                                               Likert,contagem,prop)))+
  geom_bar(position="fill", stat="identity")+
  ggtitle("Atitude de compartilhamento da Wikipedia")+
  scale_fill_viridis(discrete = T)+
  theme_minimal()+
  ylab("%")+
  xlab("")+
  coord_flip()
ggplotly(g9,tooltip = "text")

# Uso e comportamento

g10 = dados %>% 
  dplyr::select(-USERWIKI) %>% 
  dplyr::select(starts_with("Use")) %>% 
  magrittr::set_colnames(c("Eu uso a Wikipedia para desenvolver meus materiais de ensino",
                           "Eu uso a Wikipedia como plataforma para desenvolver atividades educacionais com os alunos",
                           "Recomendo que meus alunos usem a Wikipedia",
                           "Recomendo que meus colegas usem a Wikipedia",
                           "Concordo que meus alunos usem a Wikipedia nos meus cursos")) %>% 
  purrr::map(scale_likertII) %>% 
  bind_cols() %>% 
  pivot_longer(everything(),values_to = "Likert",
               names_to = "var") %>% 
  group_by(var,Likert) %>% 
  na.omit() %>% 
  summarise(contagem = n()) %>% 
  mutate(prop = round(contagem/sum(contagem)*100,2)) %>% 
  ggplot(mapping = aes(var,contagem,fill=Likert,text = sprintf("Categoria: %s<br>Contagem:%s<br>Porcentagem:%s%%",
                                                               Likert,contagem,prop)))+
  geom_bar(position="fill", stat="identity")+
  ggtitle("Uso e comportamento")+
  scale_fill_viridis(discrete = T)+
  theme_minimal()+
  ylab("%")+
  xlab("")+
  coord_flip()
ggplotly(g10,tooltip = "text")

# Perfil 2.0

g11 = dados %>% 
  dplyr::select(starts_with("Pf")) %>% 
  magrittr::set_colnames(c("Contribuo para blogs",
                           "Participo ativamente de redes sociais",
                           "Publico conteúdo acadêmico em plataformas abertas")) %>% 
  purrr::map(scale_likertII) %>% 
  bind_cols() %>% 
  pivot_longer(everything(),values_to = "Likert",
               names_to = "var") %>% 
  group_by(var,Likert) %>% 
  na.omit() %>% 
  summarise(contagem = n()) %>% 
  mutate(prop = round(contagem/sum(contagem)*100,2)) %>% 
  ggplot(mapping = aes(var,contagem,fill=Likert,text = sprintf("Categoria: %s<br>Contagem:%s<br>Porcentagem:%s%%",
                                                               Likert,contagem,prop)))+
  geom_bar(position="fill", stat="identity")+
  ggtitle("Perfil 2.0")+
  scale_fill_viridis(discrete = T)+
  theme_minimal()+
  ylab("%")+
  xlab("")+
  coord_flip()
ggplotly(g11,tooltip = "text")

# Relevância do trabalho

g12 = dados %>% 
  dplyr::select(starts_with("JR")) %>% 
  magrittr::set_colnames(c("Minha universidade promove o uso de ambientes colaborativos abertos na Internet",
                           "Minha universidade considera o uso de ambientes colaborativos abertos na Internet como um mérito de ensino")) %>% 
  purrr::map(scale_likertI) %>% 
  bind_cols() %>% 
  pivot_longer(everything(),values_to = "Likert",
               names_to = "var") %>% 
  group_by(var,Likert) %>% 
  na.omit() %>% 
  summarise(contagem = n()) %>% 
  mutate(prop = round(contagem/sum(contagem)*100,2)) %>% 
  ggplot(mapping = aes(var,contagem,fill=Likert,text = sprintf("Categoria: %s<br>Contagem:%s<br>Porcentagem:%s%%",
                                                               Likert,contagem,prop)))+
  geom_bar(position="fill", stat="identity")+
  ggtitle("Relevância do trabalho")+
  scale_fill_viridis(discrete = T)+
  theme_minimal()+
  ylab("%")+
  xlab("")+
  coord_flip()
ggplotly(g12,tooltip = "text")

# Intenção comportamental

g13 = dados %>% 
  dplyr::select(starts_with("BI")) %>% 
  magrittr::set_colnames(c("No futuro, recomendarei o uso da Wikipedia para meus colegas e alunos",
                           "No futuro, usarei a Wikipedia em minha atividade de ensino")) %>% 
  purrr::map(scale_likertI) %>% 
  bind_cols() %>% 
  pivot_longer(everything(),values_to = "Likert",
               names_to = "var") %>% 
  group_by(var,Likert) %>% 
  na.omit() %>% 
  summarise(contagem = n()) %>% 
  mutate(prop = round(contagem/sum(contagem)*100,2)) %>% 
  ggplot(mapping = aes(var,contagem,fill=Likert,text = sprintf("Categoria: %s<br>Contagem:%s<br>Porcentagem:%s%%",
                                                               Likert,contagem,prop)))+
  geom_bar(position="fill", stat="identity")+
  ggtitle("Intenção comportamental")+
  scale_fill_viridis(discrete = T)+
  theme_minimal()+
  ylab("%")+
  xlab("")+
  coord_flip()
ggplotly(g13,tooltip = "text")

# Incentivos

g14 = dados %>% 
  dplyr::select(starts_with("Inc")) %>% 
  magrittr::set_colnames(c("Para projetar atividades educacionais usando a Wikipedia, seria útil: um guia de boas práticas",
                           "Para projetar atividades educacionais usando a Wikipedia, seria útil: obter instruções de um colega",
                           "Para projetar atividades educacionais usando a Wikipedia, seria útil: obter treinamento específico",
                           "Para projetar atividades educacionais usando a Wikipedia, seria útil: maior reconhecimento institucional")) %>% 
  purrr::map(scale_likertI) %>% 
  bind_cols() %>% 
  pivot_longer(everything(),values_to = "Likert",
               names_to = "var") %>% 
  group_by(var,Likert) %>% 
  na.omit() %>% 
  summarise(contagem = n()) %>% 
  mutate(prop = round(contagem/sum(contagem)*100,2)) %>% 
  ggplot(mapping = aes(var,contagem,fill=Likert,text = sprintf("Categoria: %s<br>Contagem:%s<br>Porcentagem:%s%%",
                                                               Likert,contagem,prop)))+
  geom_bar(position="fill", stat="identity")+
  ggtitle("Incentivos")+
  scale_fill_viridis(discrete = T)+
  theme_minimal()+
  ylab("%")+
  xlab("")+
  coord_flip()
ggplotly(g14,tooltip = "text")

# Experiência

g15 = dados %>% 
  dplyr::select(starts_with("Exp")) %>% 
  magrittr::set_colnames(c("Eu consulto a Wikipedia para questões relacionadas à minha área de especialização",
                           "Eu consulto a Wikipedia para outros assuntos acadêmicos",
                           "Eu consulto a Wikipedia para questões pessoais",
                           "Eu contribuo com a Wikipedia (edições, revisões, melhoria de artigos ...)",
                           "Eu uso wikis para trabalhar com meus alunos")) %>% 
  purrr::map(scale_likertII) %>% 
  bind_cols() %>% 
  pivot_longer(everything(),values_to = "Likert",
               names_to = "var") %>% 
  group_by(var,Likert) %>% 
  na.omit() %>% 
  summarise(contagem = n()) %>% 
  mutate(prop = round(contagem/sum(contagem)*100,2)) %>% 
  ggplot(mapping = aes(var,contagem,fill=Likert,text = sprintf("Categoria: %s<br>Contagem:%s<br>Porcentagem:%s%%",
                                                               Likert,contagem,prop)))+
  geom_bar(position="fill", stat="identity")+
  ggtitle("Experiência")+
  scale_fill_viridis(discrete = T)+
  theme_minimal()+
  ylab("%")+
  xlab("")+
  coord_flip()
ggplotly(g15,tooltip = "text")

#==============================================================
# Heatmap das correlações de spearman entre as variaveis Likert
#==============================================================

correlacoes = cor(dados[,-c(1:9)],method = "spearman",use = "complete.obs")
heatmaply(round(correlacoes,2),dendrogram = "none",column_text_angle = 90)

#===========================================
# Analise de dados faltantes
#===========================================

gmiss = gg_miss_var(dados)$data %>% 
  ggplot(mapping = aes(reorder(variable,n_miss),n_miss,text = sprintf("Missing: %s",
                                                                      n_miss)))+
  geom_point(col="#6f42c1")+
  geom_segment(mapping = aes(x = variable, xend = variable,
                             y = 0, yend = n_miss),col="#6f42c1")+
  theme_minimal()+
  xlab("")+
  ylab("Missing")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) 

ggplotly(gmiss,tooltip = "text")

#===================================================================================================================
# Analisando respostas de diferentes grupos de usuários para os itens da categoria Perceived Enjoyment: ENJ1 e ENJ2
#===================================================================================================================

# ENJ1 vs Gênero

wilcox.test(ENJ1~Gênero,data = dados)

g16 = ggplot(dados)+
  geom_boxplot(mapping = aes(x = Gênero, y = ENJ1,fill=Gênero))+
  geom_segment(aes(x=1,xend=1,y=5.1,yend = 5.4))+
  geom_segment(aes(x=1,xend=2,y=5.4,yend = 5.4))+
  geom_segment(aes(x=2,xend=2,y=5.4,yend = 5.1))+
  annotate("text",x = 1.5,y = 5.6,label = "Wilcoxon p-value = 0.018")+
  theme_minimal() 
ggplotly(g16)
     
# ENJ1 vs Domínio

kruskal.test(ENJ1~Domínio,data = dados)
dunn.test::dunn.test(dados$ENJ1,dados$Domínio)

g17 = dados %>% 
  dplyr::select(Domínio,ENJ1) %>% 
  na.omit() %>% 
  ggplot()+
  geom_boxplot(mapping = aes(x = Domínio, y = ENJ1,fill=Domínio))+
  geom_segment(aes(x=2,xend=2,y=5.1,yend = 5.4))+
  geom_segment(aes(x=2,xend=4,y=5.4,yend = 5.4))+
  geom_segment(aes(x=4,xend=4,y=5.4,yend = 5.1))+
  annotate("text",x = 3,y = 5.6,label = "Dunn p-value = 0.005")+
  geom_segment(aes(x=4,xend=4,y=5.5,yend = 5.8))+
  geom_segment(aes(x=4,xend=6,y=5.8,yend = 5.8))+
  geom_segment(aes(x=6,xend=6,y=5.8,yend = 5.5))+
  annotate("text",x = 5,y = 6,label = "Dunn p-value = 0.000")+
  theme_minimal() 
ggplotly(g17)      

# ENJ1 vs PhD

wilcox.test(ENJ1~PhD,data = dados)

g18 = ggplot(dados)+
  geom_boxplot(mapping = aes(x = PhD, y = ENJ1,fill=PhD))+
  theme_minimal() 
ggplotly(g18)

# ENJ1 vs UNIVERSITY

wilcox.test(ENJ1~UNIVERSITY,data = dados)

g19 = ggplot(dados)+
  geom_boxplot(mapping = aes(x = UNIVERSITY, y = ENJ1,fill=UNIVERSITY))+
  theme_minimal() 
ggplotly(g19)

# ENJ2 vs Gênero

wilcox.test(ENJ2~Gênero,data = dados)

g20 = ggplot(dados)+
  geom_boxplot(mapping = aes(x = Gênero, y = ENJ2,fill=Gênero))+
  theme_minimal() 
ggplotly(g20)

# ENJ2 vs PhD

wilcox.test(ENJ2~PhD,data = dados)

g21 = ggplot(dados)+
  geom_boxplot(mapping = aes(x = PhD, y = ENJ2,fill=PhD))+
  theme_minimal() 
ggplotly(g21)

# ENJ2 vs UNIVERSITY

wilcox.test(ENJ2~UNIVERSITY,data = dados)

g22 = ggplot(dados)+
  geom_boxplot(mapping = aes(x = UNIVERSITY, y = ENJ2,fill=UNIVERSITY))+
  theme_minimal() 
ggplotly(g22)

#===========================================
# Analise de Agrupamento
#===========================================

# retirando variaveis com mais de 10% de missing e imputando via pacote mice o restante das observações

set.seed(6525923)

imput = mice::mice(dados %>% dplyr::select(-c(OTHER_POSITION,Vis2,UOC_POSITION)),m=1)
completeData = mice::complete(imput,1)
completeData = na.omit(completeData)

fviz_nbclust(completeData[,-c(1:7)],hcut)

hc.ward = eclust(completeData[,-c(1:7)],"hclust",2)

fviz_dend(hc.ward,k=2,show_labels = F)
