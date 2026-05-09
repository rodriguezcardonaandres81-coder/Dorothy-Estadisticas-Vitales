# ==============================================================================
# DOROTHY - Análisis de Estadísticas Vitales (Nacimientos)
# Versión: GitHub + Render
# Autor: Andres Rodriguez - © 2025
# ==============================================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readxl)
library(dplyr)
library(janitor)
library(lubridate)
library(stringr)
library(ggplot2)
library(plotly)
library(DT)
library(leaflet)
library(sf)
library(writexl)
library(tidyr)
library(zip)
library(shinyjs)
library(openxlsx)

options(shiny.maxRequestSize = 100 * 1024^2)

# ==============================================================================
# 0. CONFIGURACIÓN GLOBAL
# ==============================================================================

usuarios_validos <- data.frame(
  usuario   = c("Norha", "invitado", "Andres"),
  contraseña = c("1989", "invitado123", "123"),
  permisos  = c("completo", "lectura", "completo"),
  stringsAsFactors = FALSE
)

VARS_ANALISIS <- c(
  "sexo", "area_residencia", "anio", "mes", "pais_nacimiento_madre",
  "municipio_residencia", "ips_nacimiento", "tipo_parto", "grupo_etario_madre",
  "estado", "peso_nacer", "tiempo_de_gestacion", "pertenencia_etnica",
  "ultimo_ano_estudios_madre", "controles_prenatales", "sitio_parto",
  "profesion_certificador", "eps_1"
)

MESES_ORDEN <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                 "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")

# ==============================================================================
# 1. FUNCIONES DE PROCESAMIENTO
# ==============================================================================

procesar_datos <- function(df) {
  if (nrow(df) == 0) stop("El archivo está vacío")
  if (!("Área Residencia" %in% names(df)))
    stop("La columna 'Área Residencia' no se encontró en el archivo.")

  procesar_edad <- function(edad) {
    case_when(
      is.na(edad) | edad == ""                        ~ "Sin dato",
      suppressWarnings(as.numeric(edad)) < 10        ~ "<10",
      suppressWarnings(as.numeric(edad)) <= 14       ~ "10-14",
      suppressWarnings(as.numeric(edad)) <= 19       ~ "15-19",
      suppressWarnings(as.numeric(edad)) <= 24       ~ "20-24",
      suppressWarnings(as.numeric(edad)) <= 29       ~ "25-29",
      suppressWarnings(as.numeric(edad)) <= 34       ~ "30-34",
      suppressWarnings(as.numeric(edad)) <= 39       ~ "35-39",
      TRUE                                           ~ "40 o más"
    )
  }

  df %>%
    rename(area_residencia = `Área Residencia`) %>%
    clean_names() %>%
    mutate(
      fecha_nacimiento   = as.Date(fecha_nacimiento, origin = "1899-12-30"),
      anio               = year(fecha_nacimiento),
      mes_num            = month(fecha_nacimiento),
      mes                = case_when(
        mes_num == 1  ~ "Enero",    mes_num == 2  ~ "Febrero",
        mes_num == 3  ~ "Marzo",    mes_num == 4  ~ "Abril",
        mes_num == 5  ~ "Mayo",     mes_num == 6  ~ "Junio",
        mes_num == 7  ~ "Julio",    mes_num == 8  ~ "Agosto",
        mes_num == 9  ~ "Septiembre", mes_num == 10 ~ "Octubre",
        mes_num == 11 ~ "Noviembre",  mes_num == 12 ~ "Diciembre",
        TRUE ~ "Sin dato"
      ),
      ips_nacimiento     = if_else(is.na(ips) | ips == "", "Municipio", ips),
      peso_nacer         = case_when(
        is.na(peso) | peso == "" ~ "Sin dato",
        as.numeric(peso) < 2500  ~ "Bajo peso al nacer",
        TRUE                     ~ "Peso adecuado"
      ),
      tiempo_de_gestacion = case_when(
        is.na(tiempo_gestacion) | tiempo_gestacion == "" ~ "Sin dato",
        as.numeric(tiempo_gestacion) < 37  ~ "Pretérmino (<37 semanas)",
        as.numeric(tiempo_gestacion) <= 41 ~ "A término (37–41 semanas)",
        TRUE                               ~ "Postérmino (≥42 semanas)"
      ),
      controles_prenatales = case_when(
        is.na(numero_consultas_prenatales) | numero_consultas_prenatales == "" ~ "Sin dato",
        as.numeric(numero_consultas_prenatales) <= 3 ~ "1-3 controles",
        as.numeric(numero_consultas_prenatales) <= 7 ~ "4-7 controles",
        TRUE                                         ~ "8 o más controles"
      ),
      grupo_etario_madre = procesar_edad(edad_madre),
      grupo_etario_padre = procesar_edad(edad_padre),
      eps_1 = case_when(
        str_to_upper(regimen_seguridad_social) == "NO ASEGURADO" ~ "NO ASEGURADO",
        is.na(eps) | eps == ""                                   ~ "NO ASEGURADO",
        TRUE                                                     ~ eps
      )
    )
}

# ==============================================================================
# 2. FUNCIONES AUXILIARES
# ==============================================================================

limpiar_nombre_variable <- function(nombre) {
  nombre %>% str_remove("\\s*\\(.*?\\)") %>% str_trim() %>% str_to_title()
}

generar_grafico <- function(df, variable) {
  if (nrow(df) == 0)
    return(ggplot() +
      labs(title = "Sin datos para los filtros seleccionados") +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5, color = "#888780", size = 13)))

  variable_sym <- sym(variable)
  df <- df %>%
    mutate(!!variable_sym := str_remove_all(as.character(!!variable_sym), "\\s*\\(.*?\\)"))

  df_conteo <- df %>%
    count(!!variable_sym) %>%
    mutate(
      total_casos = sum(n),
      porcentaje  = round((n / total_casos) * 100, 1)
    ) %>%
    arrange(desc(n))

  n_barras    <- nrow(df_conteo)
  ancho_barra <- max(0.35, min(0.75, 0.9 / sqrt(n_barras)))

  paleta <- c("#185FA5","#0F6E56","#854F0B","#993C1D","#534AB7",
              "#3B6D11","#5F5E5A","#993556","#0C447C","#633806",
              "#712B13","#3C3489")
  colores <- rep(paleta, length.out = n_barras)

  titulo_fmt <- str_to_title(str_replace_all(variable, "_", " "))
  total_fmt  <- format(sum(df_conteo$n), big.mark = ",")

  p <- ggplot(df_conteo, aes(x = reorder(!!sym(variable), -n), y = n,
                              text = paste0("<b>", !!sym(variable), "</b>",
                                           "<br>Casos: ", format(n, big.mark = ","),
                                           "<br>Porcentaje: ", porcentaje, "%"))) +
    geom_bar(stat = "identity", fill = colores, width = ancho_barra, alpha = 0.9) +
    geom_text(aes(label = format(n, big.mark = ",")),
              vjust = -0.6, size = 3, color = "#444441", fontface = "plain") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18)),
                       labels = scales::comma) +
    labs(
      title    = paste("Distribución por", titulo_fmt),
      subtitle = paste("Total:", total_fmt, "registros"),
      x = NULL, y = "Frecuencia"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title      = element_text(size = 13, color = "#1a1a18", margin = margin(b = 4)),
      plot.subtitle   = element_text(size = 11, color = "#888780", margin = margin(b = 12)),
      axis.text.x     = element_text(angle = ifelse(n_barras > 6, 40, 0),
                                     hjust = ifelse(n_barras > 6, 1, 0.5),
                                     size = 10, color = "#444441"),
      axis.text.y     = element_text(size = 9, color = "#888780"),
      axis.title.y    = element_text(size = 10, color = "#888780", margin = margin(r = 8)),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#f1efe8", linewidth = 0.5),
      panel.grid.minor   = element_blank(),
      plot.background    = element_rect(fill = "white", color = NA),
      panel.background   = element_rect(fill = "white", color = NA),
      plot.margin        = margin(t = 10, r = 16, b = 10, l = 10)
    )

  ggplotly(p, tooltip = "text") %>%
    layout(
      margin = list(t = 60, b = ifelse(n_barras > 6, 110, 50), l = 50, r = 20),
      xaxis  = list(tickangle = ifelse(n_barras > 6, -40, 0), tickfont = list(size = 11)),
      yaxis  = list(gridcolor = "#f1efe8", tickfont = list(size = 10)),
      hoverlabel = list(bgcolor = "white", bordercolor = "#d3d1c7",
                        font = list(size = 12, color = "#1a1a18")),
      plot_bgcolor  = "white",
      paper_bgcolor = "white"
    ) %>%
    config(displayModeBar = FALSE)
}
generar_tabla <- function(datos, variable) {
  datos %>%
    count(!!sym(variable)) %>%
    mutate(
      !!sym(variable) := str_to_title(!!sym(variable)),
      Porcentaje = round((n / sum(n, na.rm = TRUE)) * 100, 2)
    ) %>%
    rename(Total = n) %>%
    arrange(desc(Total)) %>%
    janitor::adorn_totals("row")
}

generar_resumen_numerico <- function(datos, variable) {
  datos %>%
    summarise(
      Media              = round(mean(!!sym(variable), na.rm = TRUE), 2),
      Mediana            = round(median(!!sym(variable), na.rm = TRUE), 2),
      Minimo             = min(!!sym(variable), na.rm = TRUE),
      Maximo             = max(!!sym(variable), na.rm = TRUE),
      Desviacion_Estandar = round(sd(!!sym(variable), na.rm = TRUE), 2),
      `Primer Cuartil`   = round(quantile(!!sym(variable), 0.25, na.rm = TRUE), 2),
      `Tercer Cuartil`   = round(quantile(!!sym(variable), 0.75, na.rm = TRUE), 2)
    ) %>%
    t() %>% as.data.frame() %>% setNames("Valor")
}

calcular_p_value_poisson <- function(valor_observado, datos_historicos) {
  if (length(datos_historicos) < 2 || all(is.na(datos_historicos))) return(NA)
  tryCatch({
    lambda  <- mean(datos_historicos, na.rm = TRUE)
    if (is.na(lambda) || lambda <= 0) return(NA)
    p_lower <- ppois(valor_observado,     lambda = lambda, lower.tail = TRUE)
    p_upper <- ppois(valor_observado - 1, lambda = lambda, lower.tail = FALSE)
    min(2 * min(p_lower, p_upper), 1)
  }, error = function(e) NA)
}

# ==============================================================================
# 3. UI DE LOGIN
# ==============================================================================

login_ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML("
    .login-container {
      background: linear-gradient(to top, #A7D9F8 0%, #FFFFFF 100%);
      height: 100vh; display: flex;
      justify-content: center; align-items: center;
      font-family: 'Arial', sans-serif;
    }
    .login-box {
      background: white; padding: 40px; border-radius: 15px;
      box-shadow: 0 10px 30px rgba(0,0,0,0.2); width: 350px; text-align: center;
    }
    .login-title { color: #333; margin-bottom: 30px; font-weight: bold; font-size: 24px; }
    .btn-login {
      background: linear-gradient(45deg, #667eea, #764ba2);
      color: white; border: none; width: 100%;
      padding: 12px; border-radius: 25px; font-size: 16px; transition: all 0.3s ease;
    }
    .btn-login:hover { transform: translateY(-2px); box-shadow: 0 5px 15px rgba(0,0,0,0.2); }
    .copyright { margin-top: 20px; color: #333; font-size: 12px; }
  "))),
  div(class = "login-container",
    div(class = "login-box",
      div(class = "login-title", "Dorothy"),
      textInput("login_usuario",    "Usuario:",    placeholder = "Ingrese su usuario"),
      passwordInput("login_contrasena", "Contraseña:", placeholder = "Ingrese su contraseña"),
      actionButton("btn_login", "Ingresar", class = "btn-login"),
      div(class = "copyright", "© 2025 Andres Rodriguez - Todos los derechos reservados")
    )
  )
)

# ==============================================================================
# 4. UI PRINCIPAL
# ==============================================================================

main_ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title     = "Dorothy — Estadísticas Vitales",
    titleWidth = 320
  ),
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("📁 Cargar Archivos",       tabName = "cargar",      icon = icon("upload")),
      menuItem("📊 Gráficos y Tablas",     tabName = "graficos",    icon = icon("chart-bar")),
      menuItem("🔍 Estadísticas",          tabName = "estadisticas",icon = icon("calculator")),
      menuItem("👥 Pirámide Poblacional",  tabName = "piramide",    icon = icon("users")),
      menuItem("📅 Análisis de Tendencia", tabName = "tendencia",   icon = icon("chart-line")),
      menuItem("🤰 Controles Prenatales",  tabName = "controles",   icon = icon("heart")),
      menuItem("🗺️ Mapa Interactivo",     tabName = "mapa",        icon = icon("map")),
      menuItem("💾 Descargas",             tabName = "descargas",   icon = icon("download")),
      menuItem("🚪 Cerrar Sesión",         tabName = "logout",      icon = icon("sign-out-alt"))
    ),
    conditionalPanel(
      condition = "output.archivos_cargados == true",
      div(style = "padding:8px; margin:8px 6px;",
        h4("Filtros", style = "color:rgba(255,255,255,0.55); font-size:10px; font-weight:500; letter-spacing:0.08em; text-transform:uppercase; margin:8px 6px 10px 6px; padding-bottom:8px; border-bottom:0.5px solid rgba(255,255,255,0.12);"),
        pickerInput("f_ano", "Año", choices = NULL, multiple = TRUE,
                    options = list(`actions-box` = TRUE)),
        pickerInput("f_mes", "Mes", choices = NULL, multiple = TRUE,
                    options = list(`actions-box` = TRUE)),
        pickerInput("f_ips", "IPS de nacimiento", choices = NULL, multiple = TRUE,
                    options = list(`actions-box` = TRUE)),
        actionButton("reset_filtros", "Reiniciar filtros",
                     style = "width:100%; background:rgba(255,255,255,0.08); color:rgba(255,255,255,0.7); border:0.5px solid rgba(255,255,255,0.15); border-radius:6px; font-size:12px; margin-top:4px;")
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(tags$style(HTML("
      body, .skin-blue .main-header .logo, .skin-blue .main-header .navbar {
        font-family: 'Segoe UI', system-ui, -apple-system, sans-serif;
      }
      .skin-blue .main-sidebar { background: #0C447C !important; }
      .skin-blue .sidebar { background: #0C447C !important; }
      .main-sidebar { background: #0C447C !important; }
      .left-side { background: #0C447C !important; }
      .sidebar-menu > li > a { color: rgba(255,255,255,0.7) !important; }
      .sidebar-menu > li.active > a { 
        background: rgba(255,255,255,0.13) !important; 
        color: #ffffff !important;
        border-left: 3px solid #5DCAA5 !important;
      }
      .sidebar-menu > li > a:hover {
        background: rgba(255,255,255,0.1) !important;
        color: #ffffff !important;
      }
      .skin-blue .main-header .logo {
        background: #083260 !important;
        border-bottom: 0.5px solid rgba(255,255,255,0.08) !important;
        font-size: 15px !important; font-weight: 500 !important;
        letter-spacing: 0.02em; color: #ffffff !important;
      }
      .skin-blue .main-header .logo:hover { background: #083260 !important; }
      .skin-blue .main-header .navbar { background: #ffffff !important; border-bottom: 0.5px solid #e2e0d8; }
      .skin-blue .main-header .navbar .sidebar-toggle { color: #444441 !important; }
      .skin-blue .main-header .navbar .sidebar-toggle:hover { background: #f1efe8 !important; }
      .skin-blue .sidebar-menu > li > a {
        color: rgba(255,255,255,0.65) !important; font-size: 13px;
        padding: 9px 14px; border-left: 3px solid transparent !important; transition: all 0.15s;
      }
      .skin-blue .sidebar-menu > li > a:hover {
        background: rgba(255,255,255,0.08) !important; color: #ffffff !important;
      }
      .skin-blue .sidebar-menu > li.active > a,
      .skin-blue .sidebar-menu > li.active > a:hover {
        background: rgba(255,255,255,0.13) !important; color: #ffffff !important;
        border-left: 3px solid #5DCAA5 !important; font-weight: 500;
      }
      .skin-blue .sidebar-menu > li > a > .fa { color: rgba(255,255,255,0.5) !important; }
      .skin-blue .sidebar-menu > li.active > a > .fa { color: #5DCAA5 !important; }
      .skin-blue .sidebar-menu > li.header {
        color: rgba(255,255,255,0.3) !important; font-size: 10px !important;
        font-weight: 500 !important; letter-spacing: 0.08em !important;
        padding: 14px 16px 4px !important; text-transform: uppercase;
      }
      .sidebar .shiny-input-container .control-label { color: rgba(255,255,255,0.6) !important; font-size: 11px !important; }
      .sidebar h4 { color: rgba(255,255,255,0.8) !important; font-size: 12px; font-weight: 500;
        letter-spacing: 0.06em; text-transform: uppercase; margin: 0 0 10px 0;
        padding-bottom: 8px; border-bottom: 0.5px solid rgba(255,255,255,0.12); }
      .content-wrapper { background: #f5f4f0 !important; }
      .content { padding: 16px !important; }
      .box { border-radius: 10px !important; border: 0.5px solid #d3d1c7 !important;
        border-top: none !important; box-shadow: none !important; background: #ffffff; }
      .box-header { background: #ffffff !important; border-bottom: 0.5px solid #d3d1c7 !important;
        border-radius: 10px 10px 0 0 !important; padding: 10px 14px !important; }
      .box-header .box-title { color: #1a1a18 !important; font-size: 13px !important; font-weight: 500 !important; }
      .box.box-primary { border-top: 2px solid #185FA5 !important; }
      .box.box-success { border-top: 2px solid #0F6E56 !important; }
      .box.box-danger  { border-top: 2px solid #993C1D !important; }
      .box.box-warning { border-top: 2px solid #854F0B !important; }
      .dataTables_wrapper { font-size: 13px; }
      table.dataTable thead th { background: #f5f4f0 !important; color: #5F5E5A !important;
        font-size: 11px !important; font-weight: 500 !important; letter-spacing: 0.05em;
        text-transform: uppercase; border-bottom: 0.5px solid #d3d1c7 !important; }
      table.dataTable tbody tr:hover { background: #f5f4f0 !important; }
      .btn-primary { background: #185FA5 !important; border-color: #185FA5 !important; border-radius: 6px !important; font-size: 13px !important; }
      .btn-primary:hover { background: #0C447C !important; border-color: #0C447C !important; }
      .btn-warning { background: #854F0B !important; border-color: #854F0B !important; color: #ffffff !important; border-radius: 6px !important; font-size: 13px !important; }
      .control-label { color: #444441 !important; font-size: 12px !important; font-weight: 500; }
      .shiny-notification { border-radius: 8px !important; border: 0.5px solid #d3d1c7 !important; font-size: 13px; }
    "))),
    tabItems(

      # ---- CARGAR ----
      tabItem(tabName = "cargar",
        fluidRow(box(title = "📁 Cargar Archivos", status = "primary",
          solidHeader = TRUE, width = 12,
          fluidRow(
            column(6,
              h4("Base de Datos de Nacimientos"),
              fileInput("cargar_xls", "Cargar archivo Excel (.xls/.xlsx)",
                        accept = c(".xls",".xlsx"),
                        buttonLabel = "Buscar...",
                        placeholder = "Seleccione el archivo XLS"),
              helpText("Archivo con los datos de nacimientos para análisis")
            ),
            column(6,
              h4("Mapa Shapefile (Opcional)"),
              fileInput("cargar_shp_zip", "Cargar shapefile (.zip)",
                        accept = ".zip",
                        buttonLabel = "Buscar...",
                        placeholder = "Seleccione el archivo ZIP"),
              helpText("Archivo comprimido con el shapefile para el mapa")
            )
          ),
          fluidRow(column(12,
            verbatimTextOutput("estado_carga"),
            uiOutput("ui_preview_datos")
          ))
        ))
      ),

      # ---- GRÁFICOS ----
      tabItem(tabName = "graficos",
        conditionalPanel("output.archivos_cargados == true",  uiOutput("graficos_tablas_ui")),
        conditionalPanel("output.archivos_cargados == false",
          box(width = 12, status = "warning",
            p("Por favor cargue un archivo de datos en la pestaña 'Cargar Archivos'.")))
      ),

      # ---- ESTADÍSTICAS ----
      tabItem(tabName = "estadisticas",
        conditionalPanel("output.archivos_cargados == true",
          fluidRow(
            box(title = "👩 Edad Madre",       tableOutput("desc_edad_madre"),      status = "danger",  solidHeader = TRUE, width = 4),
            box(title = "👨 Edad Padre",        tableOutput("desc_edad_padre"),      status = "warning", solidHeader = TRUE, width = 4),
            box(title = "🤰 Tiempo Gestación", tableOutput("desc_tiempo_gestacion"), status = "success", solidHeader = TRUE, width = 4)
          )
        ),
        conditionalPanel("output.archivos_cargados == false",
          box(width = 12, status = "warning",
            p("Por favor cargue un archivo de datos en la pestaña 'Cargar Archivos'.")))
      ),

      # ---- PIRÁMIDE ----
      tabItem(tabName = "piramide",
        conditionalPanel("output.archivos_cargados == true",
          fluidRow(box(width = 12,
            pickerInput("f_municipio_residencia", "Municipio Residencia:", choices = NULL, multiple = TRUE),
            pickerInput("f_area_residencia",       "Área de Residencia:",   choices = NULL, multiple = TRUE)
          )),
          fluidRow(box(width = 12, uiOutput("piramide_ui")))
        ),
        conditionalPanel("output.archivos_cargados == false",
          box(width = 12, status = "warning",
            p("Por favor cargue un archivo de datos en la pestaña 'Cargar Archivos'.")))
      ),

      # ---- TENDENCIA ----
      tabItem(tabName = "tendencia",
        conditionalPanel("output.archivos_cargados == true",
          fluidRow(box(width = 12,
            pickerInput("var_tendencia", "Variable para análisis:",
              choices = c("IPS Nacimiento" = "ips_nacimiento",
                          "Tipo de Parto"  = "tipo_parto",
                          "Régimen"        = "regimen_seguridad_social")),
            pickerInput("f_ips_tendencia", "IPS Nacimiento:",
                        choices = c("Todos" = "Todos"), multiple = TRUE),
            DTOutput("tabla_tendencia")
          ))
        ),
        conditionalPanel("output.archivos_cargados == false",
          box(width = 12, status = "warning",
            p("Por favor cargue un archivo de datos en la pestaña 'Cargar Archivos'.")))
      ),

      # ---- CONTROLES ----
      tabItem(tabName = "controles",
        conditionalPanel("output.archivos_cargados == true",
          fluidRow(
            box(plotlyOutput("controles_prenatales_plot"), width = 12),
            box(DTOutput("controles_prenatales_table"),   width = 12)
          )
        ),
        conditionalPanel("output.archivos_cargados == false",
          box(width = 12, status = "warning",
            p("Por favor cargue un archivo de datos en la pestaña 'Cargar Archivos'.")))
      ),

      # ---- MAPA ----
      tabItem(tabName = "mapa",
        conditionalPanel("output.mapa_disponible == true",  uiOutput("mapa_ui")),
        conditionalPanel("output.archivos_cargados == true && output.mapa_disponible == false",
          h4("Cargue un archivo shapefile para visualizar el mapa.")),
        conditionalPanel("output.archivos_cargados == false",
          box(width = 12, status = "warning",
            p("Por favor cargue un archivo de datos en la pestaña 'Cargar Archivos'.")))
      ),

      # ---- DESCARGAS ----
      tabItem(tabName = "descargas",
        fluidRow(box(width = 12, title = "Opciones de Descarga",
          status = "primary", solidHeader = TRUE,
          h4("📊 Datos Filtrados"),
          downloadButton("descargar_excel", "Excel (.xlsx)"),
          hr(),
          h4("🗺️ Mapas"),
          downloadButton("descargar_mapa_pdf", "PDF - Alta Calidad"),
          downloadButton("descargar_mapa_png", "PNG - Imagen"),
          hr(),
          h4("📦 Descarga Completa"),
          downloadButton("descargar_todo", "Todas las Tablas (ZIP)")
        ))
      ),

      # ---- LOGOUT ----
      tabItem(tabName = "logout",
        div(style = "text-align:center; padding:50px;",
          h2("¿Está seguro que desea cerrar sesión?"),
          actionButton("confirm_logout", "✅ Sí, Cerrar Sesión",
                       style = "background:#C73E1D; color:white; margin:10px;"),
          actionButton("cancel_logout",  "❌ Cancelar",
                       style = "background:#18A999; color:white; margin:10px;")
        )
      )
    )
  )
)

# ==============================================================================
# 5. SERVIDOR
# ==============================================================================

server <- function(input, output, session) {

  # --- Autenticación ---
  usuario_autenticado <- reactiveVal(FALSE)
  usuario_actual      <- reactiveVal("")

  observeEvent(input$btn_login, {
    # CORRECCIÓN: usar 'contraseña' o 'contrasena' de forma consistente
    match <- usuarios_validos %>%
      filter(usuario == input$login_usuario,
             contraseña == input$login_contrasena)
    if (nrow(match) > 0) {
      usuario_autenticado(TRUE)
      usuario_actual(input$login_usuario)
      showNotification(paste("¡Bienvenido/a", input$login_usuario, "!"), type = "message")
    } else {
      showNotification("Usuario o contraseña incorrectos", type = "error")
    }
  })

  # --- Reactivos de datos ---
  datos_cargados  <- reactiveVal(NULL)
  putumayo_shape  <- reactiveVal(NULL)

  output$archivos_cargados <- reactive({ !is.null(datos_cargados()) })
  outputOptions(output, "archivos_cargados", suspendWhenHidden = FALSE)

  output$mapa_disponible <- reactive({ !is.null(putumayo_shape()) })
  outputOptions(output, "mapa_disponible", suspendWhenHidden = FALSE)

  output$usuario_actual <- renderText({ usuario_actual() })

  # --- UI condicional login/app ---
  output$main_ui <- renderUI({
    if (!usuario_autenticado()) login_ui else main_ui
  })

  # --- Cargar Excel ---
  observeEvent(input$cargar_xls, {
    req(input$cargar_xls)
    tryCatch({
      showNotification("📊 Cargando base de datos...", type = "message")
      datos <- readxl::read_excel(input$cargar_xls$datapath)
      dp    <- procesar_datos(datos)
      datos_cargados(dp)
      updatePickerInput(session, "f_ano", choices = sort(unique(dp$anio)))
      updatePickerInput(session, "f_mes", choices = MESES_ORDEN[MESES_ORDEN %in% unique(dp$mes)])
      updatePickerInput(session, "f_ips", choices = sort(unique(dp$ips_nacimiento)))
      showNotification(paste("✅ Datos cargados:", nrow(dp), "registros"), type = "message")
    }, error = function(e) {
      showNotification(paste("❌ Error:", e$message), type = "error")
    })
  })

  # --- Cargar Shapefile ---
  observeEvent(input$cargar_shp_zip, {
    req(input$cargar_shp_zip)
    tryCatch({
      showNotification("🗺️ Cargando shapefile...", type = "message")
      temp_dir  <- tempfile(); dir.create(temp_dir)
      unzip(input$cargar_shp_zip$datapath, exdir = temp_dir)
      shp_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
      if (length(shp_files) == 0) stop("No se encontró archivo .shp en el ZIP")

      shp <- sf::st_read(shp_files[1], quiet = TRUE)

      # Detectar columna departamento
      cols_depto <- c("DPTO_CNMBR","NOMBRE_DPT","DPTO","NOMBDEPTO","DEPARTAMEN","NOMBRE_DEP","DPTOCCDGO")
      col_depto  <- intersect(cols_depto, names(shp))[1]
      if (is.na(col_depto)) stop("No se encontró columna de departamento en el shapefile")

      putumayo <- shp %>% filter(toupper(!!sym(col_depto)) %in% c("PUTUMAYO","86") |
                                   !!sym(col_depto) == 86)
      if (nrow(putumayo) == 0) putumayo <- shp

      # Estandarizar columna municipio
      cols_mun <- c("MPIO_CNMBR","NOMBRE_MPI","MPIO","NOMB_MPIO","NOMBRE","NAME","NOMBRE_MUN","MPIOCNMBD")
      col_mun  <- intersect(cols_mun, names(putumayo))[1]
      if (!is.na(col_mun) && col_mun != "MPIO_CNMBR")
        putumayo <- putumayo %>% rename(MPIO_CNMBR = all_of(col_mun))

      putumayo_shape(putumayo)
      showNotification(paste("✅ Shapefile cargado:", nrow(putumayo), "municipios"), type = "message")
    }, error = function(e) {
      showNotification(paste("❌ Error shapefile:", e$message), type = "error")
    })
  })

  # --- Estado de carga ---
  output$estado_carga <- renderText({
    base <- if (!is.null(datos_cargados()))
      paste("✅ Base de datos cargada:", nrow(datos_cargados()), "registros")
    else "⏳ Esperando archivo de datos..."
    mapa <- if (!is.null(putumayo_shape()))
      paste("✅ Shapefile cargado:", nrow(putumayo_shape()), "municipios de Putumayo")
    else "⏳ Shapefile no cargado (opcional)"
    paste(base, "\n", mapa)
  })

  output$ui_preview_datos <- renderUI({
    req(datos_cargados())
    tagList(h4("Vista previa:"), DTOutput("preview_tabla"))
  })
  output$preview_tabla <- renderDT({
    req(datos_cargados())
    datatable(head(datos_cargados(), 10), options = list(scrollX = TRUE, pageLength = 5))
  })

  # --- Datos filtrados ---
  datos_filtrados <- reactive({
    req(datos_cargados())
    df <- datos_cargados()
    if (!is.null(input$f_ano) && length(input$f_ano) > 0)
      df <- df %>% filter(anio %in% input$f_ano)
    if (!is.null(input$f_mes) && length(input$f_mes) > 0)
      df <- df %>% filter(mes %in% input$f_mes)
    if (!is.null(input$f_ips) && length(input$f_ips) > 0)
      df <- df %>% filter(ips_nacimiento %in% input$f_ips)
    df
  })

  # --- Reset filtros ---
  observeEvent(input$reset_filtros, {
    updatePickerInput(session, "f_ano", selected = character(0))
    updatePickerInput(session, "f_mes", selected = character(0))
    updatePickerInput(session, "f_ips", selected = character(0))
  })

  # --- Filtros pirámide ---
  observe({
    req(datos_cargados())
    updatePickerInput(session, "f_municipio_residencia",
                      choices = sort(unique(datos_cargados()$municipio_residencia)))
  })
  observeEvent(input$f_municipio_residencia, {
    req(datos_cargados())
    df <- datos_cargados()
    if (length(input$f_municipio_residencia) > 0)
      df <- df %>% filter(municipio_residencia %in% input$f_municipio_residencia)
    updatePickerInput(session, "f_area_residencia", choices = unique(df$area_residencia))
  })

  # --- Gráficos y tablas dinámicos ---
  output$graficos_tablas_ui <- renderUI({
    plots <- lapply(VARS_ANALISIS, function(var) {
      fluidRow(
        box(width = 6, title = paste("Distribución por", var),
            solidHeader = TRUE, status = "primary", plotlyOutput(paste0("plot_", var))),
        box(width = 6, title = paste("Tabla de", var),
            solidHeader = TRUE, status = "primary", DTOutput(paste0("table_", var)))
      )
    })
    do.call(tagList, plots)
  })

  observe({
    req(datos_cargados())
    df <- datos_filtrados()
    for (var in VARS_ANALISIS) {
      local({
        v <- var
        output[[paste0("plot_", v)]] <- renderPlotly({ generar_grafico(df, v) })
        output[[paste0("table_", v)]] <- renderDT({
          datatable(generar_tabla(df, v), options = list(scrollX = TRUE, pageLength = 5))
        })
      })
    }
  })

  # --- Estadísticas descriptivas ---
  output$desc_edad_madre      <- renderTable({ generar_resumen_numerico(datos_filtrados(), "edad_madre") },      rownames = TRUE)
  output$desc_edad_padre      <- renderTable({ generar_resumen_numerico(datos_filtrados(), "edad_padre") },      rownames = TRUE)
  output$desc_tiempo_gestacion <- renderTable({ generar_resumen_numerico(datos_filtrados(), "tiempo_gestacion") }, rownames = TRUE)

  # --- Pirámide poblacional ---
  output$piramide_ui <- renderUI({
    req(datos_filtrados())
    df <- datos_filtrados()
    if (!is.null(input$f_municipio_residencia) && length(input$f_municipio_residencia) > 0)
      df <- df %>% filter(municipio_residencia %in% input$f_municipio_residencia)
    if (!is.null(input$f_area_residencia) && length(input$f_area_residencia) > 0)
      df <- df %>% filter(area_residencia %in% input$f_area_residencia)
    if (nrow(df) == 0) return(div(style = "text-align:center; margin-top:50px;", h4("Sin datos.")))
tagList(
  plotlyOutput("piramide_poblacional_output")
)
  })

  output$piramide_poblacional_output <- renderPlotly({
  req(datos_filtrados())
  df <- datos_filtrados()
  if (!is.null(input$f_municipio_residencia) && length(input$f_municipio_residencia) > 0)
    df <- df %>% filter(municipio_residencia %in% input$f_municipio_residencia)
  if (!is.null(input$f_area_residencia) && length(input$f_area_residencia) > 0)
    df <- df %>% filter(area_residencia %in% input$f_area_residencia)
  if (nrow(df) == 0) return(plotly_empty() %>% layout(title = "Sin datos"))

  orden_grupos <- c("<10","10-14","15-19","20-24","25-29","30-34","35-39","40 o más","Sin dato")

  df_conteo <- df %>%
    pivot_longer(cols = c(grupo_etario_madre, grupo_etario_padre),
                 names_to = "rol", values_to = "grupo_etario") %>%
    mutate(rol = str_replace(rol, "grupo_etario_", "") %>% str_to_title()) %>%
    count(rol, grupo_etario, .drop = FALSE) %>%
    group_by(rol) %>%
    mutate(
      total_por_rol       = sum(n),
      porcentaje          = round((n / total_por_rol) * 100, 1),
      porcentaje          = if_else(is.na(porcentaje), 0, porcentaje),
      porcentaje_piramide = if_else(rol == "Madre", -porcentaje, porcentaje),
      grupo_etario        = factor(grupo_etario, levels = orden_grupos)
    ) %>%
    ungroup()

  p <- ggplot(df_conteo,
              aes(x = grupo_etario, y = porcentaje_piramide, fill = rol,
                  text = paste0("<b>", rol, " — ", grupo_etario, "</b>",
                                "<br>Casos: ", n,
                                "<br>Porcentaje: ", abs(porcentaje), "%"))) +
    geom_bar(stat = "identity", width = 0.7, alpha = 0.9) +
    scale_fill_manual(values = c("Madre" = "#993556", "Padre" = "#185FA5")) +
    scale_y_continuous(
      breaks = seq(-40, 40, 10),
      labels = function(x) paste0(abs(x), "%")
    ) +
    coord_flip() +
    labs(x = NULL, y = "Porcentaje", fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(size = 13, color = "#1a1a18", margin = margin(b = 4)),
      plot.subtitle    = element_text(size = 11, color = "#888780", margin = margin(b = 12)),
      axis.text.y      = element_text(size = 10, color = "#444441"),
      axis.text.x      = element_text(size = 9,  color = "#888780"),
      axis.title.x     = element_text(size = 10, color = "#888780"),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#f1efe8", linewidth = 0.5),
      panel.grid.minor   = element_blank(),
      legend.position    = "bottom",
      legend.text        = element_text(size = 11, color = "#444441"),
      plot.background    = element_rect(fill = "white", color = NA),
      panel.background   = element_rect(fill = "white", color = NA),
      plot.margin        = margin(t = 10, r = 16, b = 10, l = 10)
    ) +
    geom_vline(xintercept = 0, color = "#d3d1c7", linewidth = 0.5)

  ggplotly(p, tooltip = "text") %>%
    layout(
      title = list(
        text = paste0("<b>Pirámide poblacional</b><br>",
                      "<span style='font-size:11px;color:#888780'>",
                      "Total nacimientos: ", format(nrow(df), big.mark = ","), "</span>"),
        x = 0.5, xanchor = "center", font = list(size = 14, color = "#1a1a18")
      ),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15),
      margin = list(t = 80, b = 60, l = 20, r = 20),
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      xaxis = list(tickfont = list(size = 10, color = "#888780"),
                   gridcolor = "#f1efe8", zerolinecolor = "#d3d1c7"),
      yaxis = list(tickfont = list(size = 10, color = "#444441"))
    ) %>%
    config(displayModeBar = FALSE)
})
  # --- Análisis de tendencia ---
  output$tabla_tendencia <- renderDT({
    req(input$f_mes, datos_cargados())
    var_analisis      <- input$var_tendencia
    anios_disponibles <- sort(na.omit(unique(datos_cargados()$anio)))
    if (length(anios_disponibles) < 2)
      return(datatable(data.frame(Error = "No hay suficientes años para el análisis")))

    anio_actual    <- max(anios_disponibles)
    anio_anterior  <- anio_actual - 1
    anios_historicos <- anios_disponibles[anios_disponibles != anio_actual]

    df <- datos_cargados() %>% filter(mes %in% input$f_mes, anio %in% anios_disponibles)
    if (!("Todos" %in% input$f_ips_tendencia) && length(input$f_ips_tendencia) > 0)
      df <- df %>% filter(ips_nacimiento %in% input$f_ips_tendencia)

    df_tend <- df %>%
      group_by(!!sym(var_analisis), anio) %>%
      summarise(nacimientos = n(), .groups = "drop")

    df_total <- df_tend %>%
      group_by(anio) %>%
      summarise(nacimientos = sum(nacimientos), .groups = "drop") %>%
      mutate(!!sym(var_analisis) := "TOTAL") %>%
      relocate(!!sym(var_analisis))

    df_tend <- bind_rows(df_tend, df_total)

    df_wide <- df_tend %>%
      pivot_wider(names_from = anio, values_from = nacimientos, values_fill = 0)

    df_final <- df_wide %>%
      rowwise() %>%
      mutate(
        hist_vals          = list(as.numeric(c_across(all_of(as.character(anios_historicos))))),
        media_historica    = mean(hist_vals, na.rm = TRUE),
        de_historica       = sd(hist_vals,   na.rm = TRUE),
        lim_inf            = media_historica - 2 * de_historica,
        lim_sup            = media_historica + 2 * de_historica,
        actual             = as.numeric(!!sym(as.character(anio_actual))),
        anterior           = if (as.character(anio_anterior) %in% names(df_wide))
                               as.numeric(!!sym(as.character(anio_anterior))) else NA_real_,
        z_score            = if (!is.na(de_historica) && de_historica > 0)
                               (actual - media_historica) / de_historica else NA_real_,
        p_val              = calcular_p_value_poisson(actual, hist_vals),
        var_anterior       = if (!is.na(anterior) && anterior > 0)
                               round(((actual - anterior) / anterior) * 100, 1) else NA_real_,
        var_historico      = if (!is.na(media_historica) && media_historica > 0)
                               round(((actual - media_historica) / media_historica) * 100, 1) else NA_real_,
        tendencia = case_when(
          actual > lim_sup & !is.na(p_val) & p_val <= 0.05 ~ "🚨 EXCESO SIGNIFICATIVO",
          actual > lim_sup                                  ~ "⚠️ POSIBLE EXCESO",
          actual < lim_inf & !is.na(p_val) & p_val <= 0.05 ~ "📉 DISMINUCIÓN SIGNIFICATIVA",
          actual < lim_inf                                  ~ "🔻 POSIBLE DISMINUCIÓN",
          TRUE                                              ~ "➡️ ESTABLE"
        )
      ) %>%
      ungroup() %>%
      select(
        Categoria          = !!sym(var_analisis),
        all_of(as.character(anios_disponibles)),
        Media_Hist         = media_historica,
        DE_Hist            = de_historica,
        LI_2SD             = lim_inf,
        LS_2SD             = lim_sup,
        Z_Score            = z_score,
        p_value            = p_val,
        `Var_Ant_%`        = var_anterior,
        `Var_Hist_%`       = var_historico,
        Evaluacion         = tendencia
      ) %>%
      mutate(across(where(is.numeric), ~round(., 2)))

    datatable(df_final,
      extensions = c("Buttons","Responsive"),
      options = list(scrollX = TRUE, pageLength = 15,
                     dom = "Bfrtip", buttons = c("copy","csv","excel","pdf")),
      rownames = FALSE
    ) %>%
      formatStyle("Var_Ant_%",  color = styleInterval(0, c("red","green"))) %>%
      formatStyle("Var_Hist_%", color = styleInterval(0, c("red","green"))) %>%
      formatStyle("Z_Score",    color = styleInterval(c(-2,2), c("red","black","green")))
  })

  # --- Controles prenatales ---
  output$controles_prenatales_plot <- renderPlotly({
    req(datos_filtrados())
    datos <- datos_filtrados()
    if (nrow(datos) == 0) return(plotly_empty() %>% layout(title = "Sin datos"))

    df_pre <- datos %>%
      filter(!is.na(municipio_residencia)) %>%
      group_by(municipio_residencia) %>%
      summarise(
        total = n(),
        con_4 = sum(as.numeric(numero_consultas_prenatales) >= 4, na.rm = TRUE),
        pct   = if_else(total > 0, (con_4 / total) * 100, 0),
        .groups = "drop"
      ) %>%
      arrange(desc(pct)) %>% filter(total > 0)

    p <- ggplot(df_pre, aes(x = reorder(municipio_residencia, pct), y = pct,
                             text = paste("Municipio:", municipio_residencia,
                                          "<br>Total:", total,
                                          "<br>≥4 controles:", con_4,
                                          "<br>%:", round(pct,1)))) +
      geom_bar(stat = "identity", fill = "#4e79a7") +
      geom_text(aes(label = paste0(round(pct,1),"%")), hjust = -0.1, size = 3) +
      coord_flip() +
      labs(title = "Nacimientos con ≥4 controles prenatales",
           x = "Municipio", y = "Porcentaje") +
      theme_minimal()

    ggplotly(p, tooltip = "text")
  })

  output$controles_prenatales_table <- renderDT({
    req(datos_filtrados())
    datos <- datos_filtrados()
    if (nrow(datos) == 0) return(datatable(data.frame(Mensaje = "Sin datos")))

    datos %>%
      filter(!is.na(municipio_residencia)) %>%
      group_by(municipio_residencia) %>%
      summarise(
        Total           = n(),
        Con_4_o_mas     = sum(as.numeric(numero_consultas_prenatales) >= 4, na.rm = TRUE),
        Porcentaje      = round(Con_4_o_mas / Total * 100, 2),
        .groups = "drop"
      ) %>%
      arrange(desc(Porcentaje)) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  # --- Mapa ---
  output$mapa_ui <- renderUI({
    if (!is.null(putumayo_shape())) {
      tabsetPanel(
        tabPanel("Mapa Interactivo",
                 div(style = "height:700px;", leafletOutput("mapa_interactivo", height = "100%"))),
        tabPanel("Mapa Estático",
                 plotOutput("mapa_estatico", height = "700px"))
      )
    } else {
      h4("Cargue un shapefile para visualizar el mapa.")
    }
  })

  output$mapa_interactivo <- renderLeaflet({
    req(putumayo_shape(), datos_filtrados())
    datos <- datos_filtrados()

    nac_mun <- datos %>%
      group_by(municipio_residencia) %>%
      summarise(casos = n(), .groups = "drop")

    mapa <- putumayo_shape() %>%
      left_join(nac_mun, by = c("MPIO_CNMBR" = "municipio_residencia")) %>%
      mutate(casos = if_else(is.na(casos), 0L, as.integer(casos)))

    pal    <- colorNumeric(c("#E6F2FF","#0066CC","#003366"), domain = c(0, max(mapa$casos, 1)))
    labels <- sprintf("<strong>%s</strong><br/>Nacimientos: %d",
                      str_to_title(mapa$MPIO_CNMBR), mapa$casos) %>%
              lapply(htmltools::HTML)
    bbox   <- sf::st_bbox(putumayo_shape())

    leaflet(mapa) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(casos), weight = 2, color = "white",
                  fillOpacity = 0.8,
                  highlightOptions = highlightOptions(weight = 4, color = "#FF6B00", bringToFront = TRUE),
                  label = labels) %>%
      addLegend(pal = pal, values = ~casos, title = "Nacimientos", position = "bottomright") %>%
      fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
  })

  output$mapa_estatico <- renderPlot({
    req(putumayo_shape(), datos_filtrados())
    datos <- datos_filtrados()

    nac_mun <- datos %>%
      group_by(municipio_residencia) %>%
      summarise(casos = n(), .groups = "drop")

    mapa <- putumayo_shape() %>%
      left_join(nac_mun, by = c("MPIO_CNMBR" = "municipio_residencia")) %>%
      mutate(casos = if_else(is.na(casos), 0L, as.integer(casos)),
             nombre_fmt = str_to_title(MPIO_CNMBR))

    ggplot(mapa) +
      geom_sf(aes(fill = casos), color = "white", linewidth = 0.5) +
      geom_sf_text(aes(label = nombre_fmt), size = 3, color = "black", fontface = "bold",
                   fun.geometry = sf::st_centroid) +
      scale_fill_gradient(name = "Nacimientos", low = "#E6F2FF", high = "#003366", na.value = "grey90") +
      labs(title = "Nacimientos por Municipio — Putumayo",
           subtitle = paste("Total:", sum(mapa$casos), "nacimientos")) +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray40"),
            legend.position = "bottom")
  })

  # --- Descargas ---
  output$descargar_excel <- downloadHandler(
    filename = function() paste0("Nacimientos_Putumayo_", Sys.Date(), ".xlsx"),
    content  = function(file) writexl::write_xlsx(datos_filtrados(), file)
  )

  output$descargar_mapa_pdf <- downloadHandler(
    filename = function() paste0("Mapa_Putumayo_", Sys.Date(), ".pdf"),
    content  = function(file) {
      req(putumayo_shape())
      p <- output$mapa_estatico
      ggsave(file, plot = last_plot(), device = "pdf", width = 14, height = 10, dpi = 300)
    }
  )

  output$descargar_mapa_png <- downloadHandler(
    filename = function() paste0("Mapa_Putumayo_", Sys.Date(), ".png"),
    content  = function(file) {
      req(putumayo_shape())
      ggsave(file, plot = last_plot(), device = "png", width = 14, height = 10, dpi = 300)
    }
  )

  output$descargar_todo <- downloadHandler(
    filename = function() paste0("Reporte_Dorothy_", Sys.Date(), ".zip"),
    content  = function(file) {
      tmp <- tempdir()
      f1  <- file.path(tmp, "Nacimientos.xlsx")
      writexl::write_xlsx(datos_filtrados(), f1)

      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Datos"); openxlsx::writeData(wb, "Datos", datos_filtrados())

      resumen_mun <- datos_filtrados() %>%
        group_by(municipio_residencia) %>% summarise(Total = n(), .groups = "drop") %>%
        arrange(desc(Total))
      openxlsx::addWorksheet(wb, "Por_Municipio"); openxlsx::writeData(wb, "Por_Municipio", resumen_mun)

      f2 <- file.path(tmp, "Resumen.xlsx"); openxlsx::saveWorkbook(wb, f2, overwrite = TRUE)
      zip::zip(file, files = c(f1, f2), mode = "cherry-pick")
    }
  )

  # --- Cerrar sesión ---
  observeEvent(input$confirm_logout, {
    usuario_autenticado(FALSE); usuario_actual("")
    datos_cargados(NULL); putumayo_shape(NULL)
    showNotification("Sesión cerrada", type = "message")
  })
  observeEvent(input$cancel_logout, {
    updateTabItems(session, "tabs", "graficos")
  })

} # fin server

# ==============================================================================
# 6. LANZAR APP
# ==============================================================================

ui <- fluidPage(uiOutput("main_ui"))
shinyApp(ui = ui, server = server)
