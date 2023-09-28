
# Bibliotecas ------------

# shiny
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(markdown)
# model
library(tidymodels)
library(janitor)
library(xgboost)
#interpretabilidade
library(iml)
# graphs
library(ggplot2)
library(scales)
library(glmnet)
library(partykit)


# UI -----------------------

ui <- navbarPage(
  theme = "style.css",
  "INTERPRETABILIDADE EM MODELO PREDITIVO NA ÁREA DA MEDICINA OBSTÉTRICA",
  # menu gráficos -----
  navbarMenu(
    "Gráficos", 
    icon = icon("square-poll-horizontal"),
    # painel de interpretabilidade global -----
    tabPanel(
      "De interpretabilidade global", 
      fluidPage(
        #theme = shinythemes::shinytheme("united"),
        titlePanel("Interpretabilidade Global"),
        hr(),
        sidebarLayout(
          sidebarPanel(
            width = 5,
            selectInput(
              inputId = "SelectMetGlobal", 
              label = h4("Selecione o método de interpretação:"),
              choices = c(
                "Gráfico de Dependência Parcial (PDP) - 1 variável" = "pdp1",
                "Gráfico de Dependência Parcial (PDP) - 2 variáveis" = "pdp2",
                "Gráfico da Esperança Condicional Individual (ICE)" = "ice",
                "Gráfico dos Efeitos Locais Acumulados (ALE)" = "ale",
                "Importância das Covariáveis por Permutação" = "imp",
                "Interação Total das Covariáveis" = "int",
                "Interação Bidimensional das Covariáveis" = "intbi",
                "Modelo Interpretável Substituto Global" = "sub"
              ),
              selected = "Gráfico de Dependência Parcial (PDP) - 1 variável",
              multiple = FALSE
            ),
            tags$div(submitButton("Atualizar Método", icon = icon("arrows-rotate")), `align` = "center"),
            hr(),
            shiny::conditionalPanel(
              condition = "input.SelectMetGlobal != 'pdp2' & input.SelectMetGlobal != 'imp' & input.SelectMetGlobal != 'int' & input.SelectMetGlobal != 'sub'",
              selectInput(
                inputId = "SelectVarInteresse",
                label = h4("Selecione a variável de interesse:"),
                choices = c(
                  "Idade" = "idade",
                  "Número de gestações anteriores" = "n_gestacoes",
                  "IMC categorizado" = "imc_classe",
                  "Histórico de diabetes na família" = "diabetes_familia",
                  "Macrossomia fetal" = "macrossomia_fetal",
                  "Histórico de diabetes gestacional" = "diabetes_gestacional",
                  "Indicador de tabagista" = "tabagista",
                  "Indicador de hipertensão" = "hipertensao",
                  "Valor do exame de glicemia de jejum" = "glicemia_jejum"
                ),
                selected = "Idade",
                multiple = FALSE
              )
            ),
            shiny::conditionalPanel(
              condition = "input.SelectMetGlobal == 'pdp2'",
              selectInput(
                inputId = "Select1VarInteresse",
                label = h4("Selecione a primeira variável de interesse:"),
                choices = c(
                  "Idade" = "idade",
                  "Número de gestações anteriores" = "n_gestacoes",
                  "IMC categorizado" = "imc_classe",
                  "Histórico de diabetes na família" = "diabetes_familia",
                  "Macrossomia fetal" = "macrossomia_fetal",
                  "Histórico de diabetes gestacional" = "diabetes_gestacional",
                  "Indicador de tabagista" = "tabagista",
                  "Indicador de hipertensão" = "hipertensao",
                  "Valor do exame de glicemia de jejum" = "glicemia_jejum"
                ),
                selected = "idade",
                multiple = FALSE
              ),
              selectInput(
                inputId = "Select2VarInteresse",
                label = h4("Selecione a segunda variável de interesse:"),
                choices = c(
                  "Idade" = "idade",
                  "Número de gestações anteriores" = "n_gestacoes",
                  "IMC categorizado" = "imc_classe",
                  "Histórico de diabetes na família" = "diabetes_familia",
                  "Macrossomia fetal" = "macrossomia_fetal",
                  "Histórico de diabetes gestacional" = "diabetes_gestacional",
                  "Indicador de tabagista" = "tabagista",
                  "Indicador de hipertensão" = "hipertensao",
                  "Valor do exame de glicemia de jejum" = "glicemia_jejum"
                ),
                selected = "imc_classe",
                multiple = FALSE
              )
            ),
            shiny::conditionalPanel(condition = "input.SelectMetGlobal == 'imp' | input.SelectMetGlobal == 'int' | input.SelectMetGlobal == 'sub'"),
            tags$div(submitButton("Atualizar Filtro", icon = icon("arrows-rotate")), `align` = "center"),
          ),
          # tela de visualizacao 1 -----
          mainPanel(
            width = 7,
            shinycssloaders::withSpinner(
              plotOutput("Plot1", height = "600px"), 
              color = getOption("spinner.color", "#1E5AA0"), 
              type = getOption("spinner.type", 1)
            ),
            hr(),
            print("Desenvolvido por: Agatha Rodrigues e Ornella Scardua")
          )
        )
      )
    ),
    # painel de interpretabilidade individual -----
    tabPanel(
      "De interpretabilidade individual",
      fluidPage(
        #theme = shinythemes::shinytheme("united"),
        titlePanel("Interpretabilidade Individual"),
        hr(),
        p(
          em(
            HTML(
              paste(
                "Observação: é sugerido minimizar e depois maximizar a tela do computador para", 
                "aparecer os widgets com os valores das variáveis quantitativas.", 
                sep = "<br/>"
              )
            )
          )
        ),
        sidebarLayout(
          sidebarPanel(
            width = 5,
            selectInput(
              inputId = "SelectMetIndividual",
              label = h4("Selecione o método de interpretação:"),
              choices = c(
                "Modelo Interpretável Substituto Local (LIME)" = "lime",
                "Valores Shapley" = "shapley"
              ),
              selected = "Modelo Interpretável Substituto Local (LIME)",
              multiple = FALSE
            ),
            hr(),
            h4("Insira os dados da gestante:"),
            fluidRow(
              column(
                6,
                shinyWidgets::knobInput(
                  inputId = "KnobIdade",
                  label = "Idade (em anos):",
                  value = 16, min = 16, max = 47,
                  displayPrevious = TRUE, lineCap = "round", width = "60%",
                  fgColor = "#000000", inputColor = "#000000"
                ),
                shinyWidgets::knobInput(
                  inputId = "KnobNGestacoes",
                  label = "N° de gestações:",
                  value = 1, min = 1, max = 10,
                  displayPrevious = TRUE, 
                  lineCap = "round", width = "60%",
                  fgColor = "#000000", inputColor = "#000000"
                ),
                shinyWidgets::knobInput(
                  inputId = "KnobGlicemia",
                  label = "Valor do exame de glicemia de jejum (em mg/dL):",
                  value = 92, min = 92, max = 124,
                  displayPrevious = TRUE, lineCap = "round", width = "60%",
                  fgColor = "#000000", inputColor = "#000000")
              ),
              column(
                6,
                selectInput(
                  inputId = "SelectIMC",
                  label = "IMC categorizado:",
                  choices = c(
                    "Até normal" = "até normal", 
                    "Sobrepeso" = "sobrepeso",
                    "Obeso" = "obeso"
                  ),
                  selected = "Até normal",
                  multiple = FALSE
                ),
                hr(),
                shinyWidgets::materialSwitch(
                  inputId = "SwitchDiabFam",
                  label = tags$b("Diabetes na família:"), 
                  status = "success"
                ),
                hr(),
                shinyWidgets::materialSwitch(
                  inputId = "SwitchMacFetal", 
                  label = tags$b("Macrossomia fetal:"), 
                  status = "success"
                ),
                hr(),
                shinyWidgets::materialSwitch(
                  inputId = "SwitchDiabGest", 
                  label = tags$b("Antecedência de diabetes gestacional:"), 
                  status = "success"
                ),
                hr(),
                shinyWidgets::materialSwitch(
                  inputId = "SwitchTabagismo", 
                  label = tags$b("Tabagista:"), 
                  status = "success"
                ),
                hr(),
                shinyWidgets::materialSwitch(
                  inputId = "SwitchHipertensao", 
                  label = tags$b("Hipertensão:"), 
                  status = "success"
                )
              )
            ),
            tags$div(submitButton("Atualizar Filtro", icon = icon("arrows-rotate")), `align` = "center"),
          ),
          # tela de visualizacao 2 -----
          mainPanel(
            width = 7,
            shinycssloaders::withSpinner(
              plotOutput("Plot2", height = "600px"),
              color = getOption("spinner.color", "#1E5AA0"),
              type = getOption("spinner.type", 1)
            ),
            hr(),
            print("Desenvolvido por: Agatha Rodrigues e Ornella Scardua")
          )
        )
      )
    )
  ),
  # menu como interpretar -----
  navbarMenu(
    "Como Interpretar", icon = icon("question-sign", lib = "glyphicon"),
    tabPanel("Gráfico de Dependência Parcial (PDP)", includeMarkdown("md/pdp.md")), 
    tabPanel("Gráfico da Esperança Condicional Individual (ICE)", includeMarkdown("md/ice.md")),
    tabPanel("Gráfico dos Efeitos Locais Acumulados (ALE)", includeMarkdown("md/ale.md")),
    tabPanel("Importância da Covariável por Permutação", includeMarkdown("md/imp.md")),
    tabPanel("Interação das Covariáveis", includeMarkdown("md/int.md")),
    tabPanel("Modelo Interpretável Substituto Global", includeMarkdown("md/sub.md")),
    tabPanel("Modelo Interpretável Substituto Local (LIME)", includeMarkdown("md/lime.md")),
    tabPanel("Valores Shapley", includeMarkdown("md/shapley.md"))
  ),
  # menu sobre -----
  tabPanel(
    "Sobre", icon = icon("info-sign", lib = "glyphicon"),
    includeMarkdown("md/sobre.md")
  )
)


# Dados ----------------

# dados de treino

d_train <- readRDS("train_data.rds") |> 
  mutate(diabetes_gestacional = recode(diabetes_gestacional, nao = "não", sim = "sim"))

X <- d_train[-10]
y <- d_train$insulina

# modelo ajustado 

xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = 9, min_n = 5,
  loss_reduction = 0.00000461,    
  sample_size = 0.640, mtry = 7,  
  learn_rate = 0.00209,            
) |> 
  set_engine("xgboost") |> 
  set_mode("classification")

xgb_wf <- workflow() |> 
  add_formula(insulina ~ .) |> 
  add_model(xgb_spec) |> 
  fit(d_train)

# funcao para prever novos dados

predict_function <- function(model, newdata){
  predict(model, new_data = newdata)$.pred_class
}

# preditor

predictor_one <- Predictor$new(
  model = xgb_wf, 
  data = X, y = y, 
  predict.function = predict_function,
  type = "prob"
)

predictor_two <- Predictor$new(
  model = xgb_wf, 
  data = X, y = y,
  type = "prob"
)


# Server -----------------

server <- function(input, output) {
  # graficos de interpretabilidade global ----
  output$Plot1 <- renderPlot({
    # pdp - 1 variavel
    if (input$SelectMetGlobal == "pdp1") {
      base_grafico <- FeatureEffect$new(
        predictor = predictor_one,
        method = "pdp",
        feature = input$SelectVarInteresse
      )
      
      base_grafico$results <-
        filter(base_grafico$results, as.integer(.class) == 1)
      
      g <- plot(base_grafico) +
        ggtitle("PDP") +
        scale_y_continuous("Probabilidade de insulina predita", labels = percent) +
        theme_bw() +
        theme(strip.text.x = element_blank(),
              text = element_text(size = 15))
    }
    # pdp - 2 variaveis
    else if (input$SelectMetGlobal == "pdp2") {
      base_grafico <- FeatureEffect$new(
        predictor = predictor_one,
        method = "pdp",
        feature = c(input$Select1VarInteresse, 
                    input$Select2VarInteresse)
      )
      
      base_grafico$results <- filter(base_grafico$results, as.integer(.class) == 1)
      
      muda_label <- TRUE
      if (class(base_grafico$results[ , input$Select1VarInteresse]) == "numeric" & 
            class(base_grafico$results[ , input$Select2VarInteresse]) == "numeric"){
        muda_label <- FALSE
      } 
      else if (all(class(base_grafico$results[ , input$Select1VarInteresse]) != "numeric",
                   class(base_grafico$results[ , input$Select2VarInteresse]) != "numeric")){
        muda_label <- FALSE
      }
      
      g <- plot(base_grafico) +
        ggtitle("PDP") +
        theme_bw() +
        theme(strip.text.x = element_blank(),
              text = element_text(size = 15),
              legend.position = "bottom",
              legend.key.width = unit(4, 'cm')) +
        scale_fill_viridis_c(option = "inferno", labels = percent) +
        scale_color_viridis_d(option = "inferno", labels = percent) +
        labs(fill = "")
      
      if (muda_label) g <- g + 
                           labs() +
                           scale_y_continuous("Probabilidade de insulina predita", labels = percent) +
                           scale_color_manual(values = c("#F1711FFF", "#71196EFF", "#000004FF")) +
                           scale_fill_manual(values = c("#F1711FFF", "#71196EFF", "#000004FF"))
    }
    # ice
    else if (input$SelectMetGlobal == "ice") {
      base_grafico <- FeatureEffect$new(
        predictor = predictor_two,
        method = "pdp+ice", 
        feature = c(input$SelectVarInteresse)
      )
      
      levels(base_grafico$results$.class) <- c("sim","não")
      
      base_grafico$results <- filter(base_grafico$results, as.integer(.class) == 1)
      
      g <- plot(base_grafico) + 
        ggtitle("ICE") + 
        scale_y_continuous("Probabilidade de insulina predita", labels = scales::percent) +
        theme_bw() +
        theme(strip.text.x = element_blank(),
              text = element_text(size = 15))
    }
    # ale
    else if (input$SelectMetGlobal == "ale") {
      base_grafico <- FeatureEffect$new(
        predictor = predictor_one,
        method = "ale", 
        feature = c(input$SelectVarInteresse)
      )
      
      base_grafico$results <- filter(base_grafico$results, as.integer(.class) == 1)
      
      g <- base_grafico$plot() + 
        ggtitle("ALE") + 
        scale_y_continuous("Diferença para a previsão média") +
        theme_bw() +
        theme(strip.text.x = element_blank(),
              text = element_text(size = 15))
    }
    # feature importance
    else if (input$SelectMetGlobal == "imp") {
      base_grafico <- FeatureImp$new(
        predictor = predictor_one, 
        loss = "ce"
      )
      
      g <- plot(base_grafico) +
        ggtitle("Importância das covariáveis") +
        theme_bw() + 
        scale_y_discrete("Covariável") + 
        scale_x_continuous("Importância")
    }
    # feature total interaction
    else if (input$SelectMetGlobal == "int") {
      base_grafico <- Interaction$new(
        predictor_one
      )
      
      base_grafico$results <- filter(base_grafico$results, as.integer(.class) == 1)
      
      g <- plot(base_grafico) + 
        ggtitle("Interação total das covariáveis") +
        scale_x_continuous("Força da interação") +
        scale_y_discrete("Covariável") +
        theme_bw() +
        theme(strip.text.x = element_blank(),
              text = element_text(size = 15))
    }
    # feature 2-way interaction
    else if (input$SelectMetGlobal == "intbi") {
      base_grafico <- Interaction$new(
        predictor_one,
        feature = c(input$SelectVarInteresse)
      )
      
      base_grafico$results <- filter(base_grafico$results, as.integer(.class) == 1)
      
      g <- plot(base_grafico) + 
        ggtitle("Interação bidimensional das covariáveis") +
        scale_x_continuous("Força da interação") +
        scale_y_discrete("Covariáveis") +
        theme_bw() +
        theme(strip.text.x = element_blank(),
              text = element_text(size = 15))
    }
    # global surrogate
    else if (input$SelectMetGlobal == "sub") {
      base_grafico <- TreeSurrogate$new(
        predictor_one, 
        maxdepth = 3
      ) 
      
      grafico <- plot(base_grafico) + 
        ggtitle("Modelo interpretável substituto global") +
        theme_bw()
      
      categ_interesse <- unique(grafico$data$.path)
      
      g <- grafico$data %>%
        filter(.path != categ_interesse[2]) %>%
        ggplot() +
        theme_bw() +
        theme(text = element_text(size = 15)) +
        geom_bar() +
        aes(x = .class) +
        labs(x = "Classe", y = "Frequência") +
        facet_wrap(~.path, ncol = 3)
    }
    g
  })
  # graficos de interpretabilidade individual ----
  output$Plot2 <- renderPlot({
    # lime
    if (input$SelectMetIndividual == "lime") {
      base_grafico <- LocalModel$new(
        predictor_two, 
        k = 9,
        x.interest = data.frame(
          idade = input$KnobIdade, 
          n_gestacoes = input$KnobNGestacoes, 
          imc_classe = input$SelectIMC, 
          diabetes_familia = ifelse(input$SwitchDiabFam, "sim", "não"), 
          macrossomia_fetal = ifelse(input$SwitchMacFetal, "sim", "não"), 
          diabetes_gestacional = ifelse(input$SwitchDiabGest, "sim", "nao"), 
          tabagista = ifelse(input$SwitchTabagismo, "sim", "não"), 
          hipertensao = ifelse(input$SwitchHipertensao, "sim", "não"), 
          glicemia_jejum = input$KnobGlicemia
        )
      )
      
      base_grafico$results$.class <- ifelse(base_grafico$results$.class == ".pred_sim", "sim", "não")
      
      base_grafico$results <- filter(base_grafico$results, .class == "sim")
      
      g <- plot(base_grafico) + 
        ggtitle("LIME") +
        labs(x = "Valor das covariáveis", y = "Efeito") + 
        theme_bw() +
        theme(strip.text.x = element_blank(),
              text = element_text(size = 15))
      
    }
    # shapley values
    else if (input$SelectMetIndividual == "shapley") {
      base_grafico <- Shapley$new(
        predictor_one, 
        x.interest = data.frame(
          idade = input$KnobIdade, 
          n_gestacoes = input$KnobNGestacoes, 
          imc_classe = input$SelectIMC, 
          diabetes_familia = ifelse(input$SwitchDiabFam, "sim", "não"), 
          macrossomia_fetal = ifelse(input$SwitchMacFetal, "sim", "não"), 
          diabetes_gestacional = ifelse(input$SwitchDiabGest, "sim", "nao"), 
          tabagista = ifelse(input$SwitchTabagismo, "sim", "não"), 
          hipertensao = ifelse(input$SwitchHipertensao, "sim", "não"), 
          glicemia_jejum = input$KnobGlicemia
        )
      )
      
      base_grafico$results <- filter(base_grafico$results, as.integer(class) == 1)
      
      g <- base_grafico$plot() + 
        ggtitle("Valores Shapley") +
        labs(y = "Valor Shapley", x = "Valor das covariáveis") +
        theme_bw() +
        theme(strip.text.x = element_blank(),
              text = element_text(size = 15))
    }
    g
  })
}

shinyApp(ui, server)
