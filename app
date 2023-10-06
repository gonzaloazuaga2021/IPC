library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggrepel)
library(viridis)

base_final <- openxlsx::read.xlsx("Base_final_prueba_dash.xlsx",colNames = T)

categ <- openxlsx::read.xlsx("Categorias_2.xlsx",colNames = T)


ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Seguimiento IPC"),
                    dashboardSidebar(
                      selectInput("i_Mes", "Mes-año", choices = base_final %>% select(Mes)),
                      selectInput("i_rubro", "Categoría", choices = categ %>% filter(Rubro != "Nivel general") %>% arrange(Rubro) %>% pull(Rubro))
                    ),
                    dashboardBody(
                      fluidRow(
                        infoBox(title = h4("INDEC"), width = 3, textOutput("Valor_INDEC"), icon = icon("bar-chart")),
                        infoBox(title = h4("Promedio provincias"), width = 3, textOutput("Valor_Promedio"), icon = icon("bar-chart")),
                        box(title = "Nivel General", status = "info", solidHeader = TRUE, collapsible = TRUE, width = 6, plotOutput("Nivel_general"))
                      ),
                      fluidRow(
                        box(title = "Dinámica IPC", status = "info", solidHeader = TRUE, collapsible = TRUE, width = 6, plotOutput("ipc")),
                        box(title = "Categoría", status = "info", solidHeader = TRUE, height = "100%", width = 6, collapsible = TRUE, plotOutput("Categoria"))
                      )
                    )
)





server <- function(input, output) { 
  
  output$ipc <- renderPlot({
    base_final %>%
      filter(Mes == input$i_Mes) %>% 
      ggplot(aes(x = Var_regional, y = Variacion)) +
      geom_point(aes(size = Variacion, colour = Region_indec, alpha = 0.05)) +
      geom_abline(slope = 1, intercept = 0, alpha = 0.4)+
      geom_text_repel(data = filter(base_final, Mes == input$i_Mes), aes(label = Region), box.padding = 0.5) +  # Usar geom_text_repel para evitar superposiciones +
      theme_classic() +
      labs(title = "IPC provinciales vs. IPC INDEC regionales",
           x = "IPC INDEC (en %)",
           y = "IPC Provincial (en %)",
           caption="Nota: El tamaño de cada punto corresponde a la variación mensual del índice provincial.
      Los puntos por debajo de la recta presentan un IPC menor a INDEC.") +
      theme(plot.title = element_text(size=15, face="bold"),
            axis.title.y = element_text(size = 12),
            axis.title.x = element_text(size = 12),
            axis.text = element_text(size = 12, color = "black"),
            legend.text = element_text(size = 12),
            legend.position = "right",
            plot.caption = element_text(face = "bold", size = 11)) +
      scale_color_discrete(name="") + guides (size = FALSE, alpha = FALSE) 
    
  })
  
  
  output$Nivel_general <- renderPlot({
    
    mi_paleta <- viridis(11)
    
    categ %>% 
      filter(Mes == input$i_Mes & Rubro == "Nivel general" & Provincia != "INDEC") %>%
      mutate(Provincia = fct_reorder(Provincia, Var_mensual)) %>% 
      ggplot(aes(x=Provincia, y = Var_mensual, color = Provincia)) + 
      geom_point(size = 4) + 
      geom_segment(aes(x=Provincia, xend =Provincia, y = Var_mensual, yend = 0)) + 
      geom_text_repel(aes(label = Var_mensual, size = 6), box.padding = 0.5, family="bold") +
      coord_flip() +
      theme_classic() +
      labs(x = "",
           y = "Variación mensual (en %)",
           title = " ") +
      scale_colour_manual(values = mi_paleta)+
      theme(plot.title = element_text(size=15, face="bold"),
            axis.text = element_text(size = 12, color = "black"),
            axis.title.x = element_text(size = 12),
            legend.position = "none")
  })
  
  
  output$Categoria <- renderPlot({
    
    mi_paleta <- viridis(11)
    
    categ %>% 
      filter(Mes == input$i_Mes & Rubro == input$i_rubro) %>%
      mutate(Provincia = fct_reorder(Provincia, Var_mensual)) %>% 
      ggplot(aes(x=Provincia, y = Var_mensual, color = Provincia)) + 
      geom_point(size = 4) + 
      geom_segment(aes(x=Provincia, xend =Provincia, y = Var_mensual, yend = 0)) + 
      geom_text_repel(aes(label = Var_mensual, size = 6), box.padding = 0.5, family="bold") +
      coord_flip() +
      theme_classic() +
      labs(x = "",
           y = "Variación mensual (en %)",
           title = input$i_rubro) +
      scale_colour_manual(values = mi_paleta) +
      theme(plot.title = element_text(size=15, face="bold"),
            axis.text = element_text(size = 12, color = "black"),
            axis.title.x = element_text(size = 12),
            legend.position = "none")
  })
  
    # Calcular el valor de nivel general de INDEC
    output$Valor_INDEC <- renderText({
      valor_indec <- categ %>% 
        filter(Mes == input$i_Mes & Rubro == "Nivel general" & Provincia == "INDEC") %>% 
        mutate(Valor_indeC = paste(round(Var_mensual,2), "%", sep = ""))%>% 
        pull(Valor_indeC) 
    })
    
    # Calcular el valor promedio del resto de las provincias de nivel general
    output$Valor_Promedio <- renderText({
      valor_promedio <- categ %>% 
        filter(Mes == input$i_Mes & Rubro == "Nivel general" & Provincia != "INDEC") %>%
        summarise(Promedio = paste(round(mean(Var_mensual, na.rm = TRUE),2), "%", sep ="")) %>% 
        pull(Promedio)
    })
  

}

shinyApp(ui, server)
