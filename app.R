###############################################################################
# Dashboard Shiny — Réplication Thomas More
# Exploration des résultats du pipeline IA (Gemini)
#
# Fichiers attendus dans le même dossier :
#   - resume_global.csv    (une ligne par vidéo)
#   - personnalites.csv    (une ligne par mention)
#   - themes.csv           (une ligne par thème détecté)
#   - corpus.csv           (le corpus original, pour jointure chaîne/date)
#
# Lancement :
#   library(shiny)
#   runApp("app.R")
#
# Packages requis (installer une fois) :
#   install.packages(c("shiny", "bslib", "ggplot2", "dplyr", "tidyr",
#                       "DT", "plotly", "forcats", "stringr", "readr",
#                       "scales", "lubridate", "bsicons"))
###############################################################################

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)
library(forcats)
library(stringr)
library(readr)
library(scales)
library(lubridate)

# ============================================================================
# 1. CHARGEMENT DES DONNÉES
# ============================================================================

load_data <- function(input) {
  # --- Résumé global ---
  resume <- readRDS("resume.rds")

  # --- Personnalités ---
  perso <- readRDS("perso.rds")

  # --- Thèmes ---
  themes <- readRDS("themes.rds")

  # --- Corpus (pour jointure chaîne / date / durée) ---
  # Essayer plusieurs formats
  corpus <- readRDS("corpus.rds")

  # # Segments de text (Iramuteq)
  # df_iramuteq <- readRDS(file="df_iramuteq.rds")

  palettes <- readRDS(file="palettes.rds")
  # palettes$global$hard <- palettes$global$hard[-c(1,5)]
  # palettes$global$soft <- palettes$global$soft[-c(1,5)]



  corpus <- corpus %>%
    select(video_id, chaine, date, titre, duree_sec, poids_video)

  # Joindre la chaîne au résumé
  resume <- resume %>%
    left_join(corpus %>% select(video_id, any_of(c("chaine", "date", "titre",
                                                   "duree_sec", "emission",
                                                   "poids_video"))),
              by = "video_id")
  perso <- perso %>%
    left_join(corpus %>% select(video_id, any_of(c("chaine", "date", "poids_video"))),
              by = "video_id")
  themes <- themes %>%
    left_join(corpus %>% select(video_id, any_of(c("chaine", "date", "poids_video"))),
              by = "video_id")

  corpus <- corpus %>% filter(date >= input$date_min & date <= input$date_max)
  resume <- resume %>% filter(date >= input$date_min & date <= input$date_max)
  perso <- perso %>% filter(date >= input$date_min & date <= input$date_max)
  themes <- themes %>% filter(date >= input$date_min & date <= input$date_max)

  # Nettoyage
  resume <- resume %>%
    mutate(
      orientation_base = factor(orientation_base,
                                levels = c("gauche", "neutre", "droite")),
      date = as.Date(date)
    )

  perso <- perso %>%
    mutate(
      attitude = as.numeric(attitude),
      date = as.Date(date)
    ) %>%
    filter(!is.na(nom))

  nb_perso <- nrow(perso %>% filter(!is.na(courant_politique)))

  perso <- perso %>%
    group_by(nom) %>%
    filter(n() > 100) %>%
    ungroup()

  themes <- themes %>%
    mutate(
      note_bias = as.numeric(note_bias),
      date = as.Date(date)
    ) %>%
    filter(!is.na(theme))

  nb_themes <- nrow(themes)

  themes <- themes %>%
    group_by(theme) %>%
    # filter(n() > 100) %>%
    ungroup()

  list(resume = resume, perso = perso, themes = themes,
       nb_perso = nb_perso, nb_themes = nb_themes)
}

# ============================================================================
# 2. PALETTE ET THÈME
# ============================================================================

pal_orientation <- c("gauche" = "#D32F2F", "neutre" = "#757575", "droite" = "#1565C0")
pal_chaine <- c("BFMTV" = "#003F8A", "CNEWS" = "#E3001B",
                "LCI" = "#FF6600", "France Info" = "#0084CB",
                "FranceInfo" = "#0084CB", "franceinfo" = "#0084CB")

pal_courant <- c("1 - Gauche radicale" = "#E3001B",
                 "2 - Gauche modérée" = "#F40261",
                 "3 - Écologistes" = "#2D9B57",
                 "4 - Centre / majorité présidentielle" = "#FFCC00",
                 "5 - Droite" = "#1A6FC4",
                 "6 - Droite Radicale" = "#3B1A0E")

my_colors <- function(vector,color){
  styleInterval(quantile(vector, probs = seq(.05, .95, .1), na.rm = TRUE),
                colorRampPalette(c("white", color))(11))
}

colorise_segments_html <- function(df,
                                   palette,
                                   text_col = "text",
                                   classe_col = "classe") {
  # Construction HTML
  html_parts <- purrr::pmap_chr(
    df,
    function(...){
      row <- list(...)
      txt <- htmltools::htmlEscape(row[[text_col]])
      classe <- row[[classe_col]]
      col <- palette[[as.character(classe)]]
      # title <- names(palette[[as.character(classe)]])
      glue::glue("<span title={classe} style='background-color:{col}; padding:2px;'>{txt}</span>")
    }
  )

  # Concatène tout en un seul bloc HTML
  paste(html_parts, collapse = " ")
}

theme_dashboard <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15, margin = margin(b = 10)),
    plot.subtitle = element_text(color = "grey40", size = 11, margin = margin(b = 15)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 11)
  )

# ============================================================================
# 3. UI
# ============================================================================

ui <- page_navbar(
  fillable = FALSE,

  title = tags$span(
    tags$strong("Thomas More"),
    tags$span(" — Réplication", style = "font-weight: 300; opacity: 0.8;")
  ),

  header = div(
    style = paste(
      "background-color: #fff3cd;",
      "border-bottom: 1px solid #ffc107;",
      "padding: 6px 16px;",
      "font-size: 0.85rem;",
      "color: #664d03;",
      "display: flex;",
      "align-items: center;",
      "gap: 8px;"
    ),
    tags$span(style = "font-size: 1rem;", "\u26a0\ufe0f"),  # icône ⚠️
    tags$strong("Données provisoires"),
    "— Ces résultats proviennent d'une méthodologie discutable et n'ont pas vocation à être utilisé tels quels."
  ),

  id = "nav",
  theme = bs_theme(
    version = 5,
    # bootswatch = "flatly",
    # primary = "#1B4F72",
    # "navbar-bg" = "#1B4F72",
    base_font = font_google("Source Sans Pro"),
    heading_font = font_google("DM Sans"),
    font_scale = 0.95
  ),
  bg = "#1B4F72",



  # ========== TAB 1 : VUE D'ENSEMBLE ==========
  nav_panel(
    title = "Vue d'ensemble",
    icon = bsicons::bs_icon("speedometer2"),

    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(
        title = "Vidéos analysées",
        value = textOutput("n_videos"),
        showcase = bsicons::bs_icon("camera-video"),
        theme = "primary"
      ),
      value_box(
        title = "Mentions politiques",
        value = textOutput("n_mentions"),
        showcase = bsicons::bs_icon("people"),
        theme = "info"
      ),
      value_box(
        title = "Thèmes détectés",
        value = textOutput("nb_themes"),
        showcase = bsicons::bs_icon("tags"),
        theme = "success"
      ),
      value_box(
        title = "Note moyenne",
        value = textOutput("note_moy"),
        showcase = bsicons::bs_icon("bar-chart"),
        theme = "warning"
      )
    ),

    layout_columns(
      col_widths = c(4, 8),

      card(
        card_header("Orientation globale par chaîne"),
        plotlyOutput("plot_orientation_chaine", height = "380px")
      ),
      card(
        card_header("Distribution des notes"),
        plotlyOutput("plot_hist_notes", height = "380px")
      )
    ),

    layout_columns(
      col_widths = c(7, 5),

      card(
        card_header("Répartition gauche / neutre / droite par chaîne"),
        plotlyOutput("plot_gnd_chaine", height = "350px")
      ),
      card(
        card_header("Évolution temporelle de la note moyenne"),
        plotlyOutput("plot_temporal", height = "350px")
      )
    )
  ),

  # ========== TAB 2 : PERSONNALITÉS ==========
  nav_panel(
    title = "Personnalités",
    icon = bsicons::bs_icon("person-badge"),

    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(
        title = "Mentions politiques",
        value = textOutput("n_perso"),
        showcase = bsicons::bs_icon("people"),
        theme = "primary"
      ),
      value_box(
        title = "Part d'attitude positive",
        value = textOutput("pct_positif"),
        showcase = bsicons::bs_icon("emoji-smile"),
        theme = "success"
      ),
      value_box(
        title = "Part d'attitude négative",
        value = textOutput("pct_negatif"),
        showcase = bsicons::bs_icon("emoji-angry"),
        theme = "danger"
      ),
      value_box(
        title = "Note moyenne",
        value = textOutput("attitude_moy"),
        showcase = bsicons::bs_icon("bar-chart"),
        theme = "warning"
      )
    ),

    layout_sidebar(
      sidebar = sidebar(
        title = "Filtres",
        width = 280,
        # selectInput("perso_chaine", "Chaîne",choices = NULL, multiple = TRUE),
        radioButtons("perso_chaine", "Chaîne",choices = "Toutes"),
        # sliderInput("perso_min_mentions", "Mentions minimum",
        #             min = 1, max = 50, value = 5, step = 1),
        # selectInput("perso_courant", "Courant politique",
        #             choices = NULL, multiple = TRUE),
        radioButtons("perso_sort", "Trier par",
                     choices = c("Attitude moyenne" = "attitude",
                                 "Nombre de mentions" = "n"),
                     selected = "n")
      ),

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Attitude du journaliste par personnalité (Top 30)"),
          plotlyOutput("plot_perso_attitude", height = "500px")
        ),
        card(
          card_header("Attitude par personnalité et par chaîne (Top 10)"),
          plotlyOutput("plot_perso_attitude_chaine2", height = "500px")
        )
      ),
      layout_columns(
        col_widths = c(6,6),
        card(
          card_header("Attitude moyenne par courant politique"),
          plotlyOutput("plot_perso_courant", height = "500px")
        ),
        card(
          card_header("Attitude moyenne par chaîne"),
          plotlyOutput("plot_perso_attitude_chaine", height = "800px")
        )
      ),
      layout_columns(
        col_widths = c(12),
        card(
          card_header("Évolution de l'attitude du journaliste par personnalité (Top 10)"),
          plotlyOutput("plot_temporal_perso_attitude", height = "800px")
        )
      ),

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Présence par chaîne et courant"),
          plotlyOutput("plot_perso_heatmap", height = "500px")
        ),
        card(
          card_header("Répartition négatif / neutre / positif"),
          plotlyOutput("plot_perso_sentiment", height = "500px")
        )
      ),

      layout_columns(
        col_widths = c(12),
        card(
          card_header("Répartition négatif / neutre / positif par chaîne"),
          plotlyOutput("plot_perso_orientation", height = "800px")
        )
      ),

      card(
        card_header("Données détaillées — Personnalités"),
        DTOutput("table_perso", height = "500px")
      )
    )
  ),

  # ========== TAB 3 : THÉMATIQUES ==========
  nav_panel(
    title = "Thématiques",
    icon = bsicons::bs_icon("tag"),

    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(
        title = "Thèmes",
        value = textOutput("n_themes"),
        showcase = bsicons::bs_icon("tags"),
        theme = "primary"
      ),
      value_box(
        title = "Biais à gauche",
        value = textOutput("n_themes_gauche"),
        showcase = bsicons::bs_icon("arrow-left-circle"),
        theme = "danger"
      ),
      value_box(
        title = "Biais à droite",
        value = textOutput("n_themes_droite"),
        showcase = bsicons::bs_icon("arrow-right-circle"),
        theme = "info"
      ),
      value_box(
        title = "Biais moyen",
        value = textOutput("biais_moy"),
        showcase = bsicons::bs_icon("bar-chart"),
        theme = "warning"
      )
    ),

    layout_sidebar(
      sidebar = sidebar(
        title = "Filtres",
        width = 280,
        # selectInput("theme_chaine", "Chaîne",choices = NULL, multiple = TRUE),
        radioButtons("theme_chaine", "Chaîne",choices = "Toutes"),
        selectInput("theme_select", "Thématique",
                    choices = NULL, multiple = TRUE),
        radioButtons("theme_metric", "Métrique",
                     choices = c("Fréquence" = "freq",
                                 "Biais moyen" = "bias"),
                     selected = "freq")
      ),

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Distribution des thématiques"),
          plotlyOutput("plot_theme_freq", height = "500px")
        ),
        card(
          card_header("Biais moyen par thématique"),
          plotlyOutput("plot_theme_bias", height = "500px")
        )
      ),

      layout_columns(
        col_widths = c(12),
        card(
          card_header("Thématiques × Chaîne (heatmap)"),
          plotlyOutput("plot_theme_heatmap", height = "800px")
        )
      ),

      # layout_columns(
      #   col_widths = c(12),
      #   card(
      #     card_header("Biais par chaîne"),
      #     plotlyOutput("plot_theme_note", height = "500px")
      #   )
      # ),

      layout_columns(
        col_widths = c(12),
        card(
          card_header("Orientation par thème et chaîne"),
          plotlyOutput("plot_theme_orientation", height = "1000px")
        )
      ),

      card(
        card_header("Données détaillées — Thèmes"),
        DTOutput("table_themes", height = "500px")
      ),

      card(
        card_header("Texte complet coloré par segment - iRaMuTeQ"),
        htmlOutput("classe_url"),
        htmlOutput("classe_text")
      )

    )
  ),

  # ========== TAB 4 : EXPLORER ==========
  nav_panel(
    title = "Explorer les vidéos",
    icon = bsicons::bs_icon("search"),

    layout_sidebar(
      height = "1000px",
      sidebar = sidebar(
        title = "Filtres",
        width = 280,
        # selectInput("exp_chaine", "Chaîne",choices = NULL, multiple = TRUE),
        radioButtons("exp_chaine", "Chaîne",choices = "Toutes"),
        sliderInput("exp_note_range", "Note d'orientation",
                    min = 0, max = 100, value = c(0, 100)),
        selectInput("exp_orientation", "Orientation",
                    choices = c("Toutes", "gauche", "neutre", "droite"),
                    selected = "Toutes"),
        textInput("exp_search", "Rechercher (titre)",
                  placeholder = "ex: Sarkozy, budget..."),
        actionButton("exp_reset", "Réinitialiser", class = "btn-outline-secondary btn-sm")
      ),

      card(
        card_header(
          "Résultats vidéo par vidéo",
          popover(
            bsicons::bs_icon("info-circle"),
            title = "Info",
            "Cliquez sur une ligne pour voir les détails."
          )
        ),
        DTOutput("table_explorer")
      )
    )
  ),

  # ========== TAB 5 : DONNÉES BRUTES ==========
  nav_panel(
    title = "Données",
    icon = bsicons::bs_icon("table"),

    navset_card_tab(
      height = "1000px",
      nav_panel("Résumé global",
                DTOutput("raw_resume")),
      nav_panel("Personnalités",
                DTOutput("raw_perso")),
      nav_panel("Thèmes",
                DTOutput("raw_themes"))
    )
  ),

  # ========== TAB 6 : THÉMATIQUES ==========
  nav_panel(
    title = "Sources",
    icon = bsicons::bs_icon("code-slash"),

    layout_columns(
      col_widths = c(6 , 6),
      card(
        card_header("Code source Python"),
        verbatimTextOutput("code_source")
      ),

      card(
        card_header("Sources et méthodologie"),
        h1("Corpus vidéo"),
        p("Les données proviennent de cinq chaînes YouTube : franceinfo (@franceinfo), BFMTV (@BFMTV), LCI (@LCI), CNEWS (@CNEWSofficiel) et Europe 1 (@Europe1). La chaîne YouTube de CNEWS ne proposant que très peu de vidéos, et quasi exclusivement des capsules de moins de deux minutes, les vidéos d'Europe 1 ont été intégrées au corpus CNEWS. Ce choix se justifie par le partenariat éditorial désormais établi entre les deux chaînes, Europe 1 rediffusant une large part du contenu de CNEWS (émissions de Pascal Praud, Laurence Ferrari, etc.)."),
        p("Seules les vidéos d'une durée supérieure à deux minutes ont été conservées, afin d'exclure les capsules destinées aux réseaux sociaux (et pour suivre la méthodologie de Thomas More). Le corpus est limité aux vidéos diffusées en 2025."),
        p("Les transcriptions ont été obtenues via les sous-titres automatiques générés par YouTube."),
        h1("Analyse IA"),
        p("Les transcriptions complètes ont été soumises au modèle Gemini 2.5 Flash (Google) via l'API Gemini, en utilisant les prompts publiés par l'Institut Thomas More dans le cadre de son Rapport 35 (février 2026). La méthodologie est donc identique à celle de Thomas More, à deux différences près : le modèle utilisé (Gemini 2.5 Flash au lieu de Gemini 1.5 Pro, ce dernier ayant été retiré par Google) et le périmètre (quatre chaînes d'information en continu au lieu du seul audiovisuel public)."),
        h1("Données produites"),
        p("Trois jeux de données sont issus du pipeline :"),
        p("- L'analyse par vidéo attribue à chaque vidéo une note de biais (0–100) et une orientation (gauche, droite ou neutre), reflétant l'orientation idéologique globale du contenu ;"),
        p("- L'analyse par personnalité identifie, pour chaque vidéo, les personnalités et formations politiques mentionnées, leur courant politique et l'attitude du journaliste à leur égard (échelle de -10 à +10) ;"),
        p("- L'analyse par thème classe chaque vidéo dans une à trois thématiques parmi une liste fermée de 20 catégories, avec pour chacune une note de biais et une orientation."),
        h1("Retraitements manuels"),
        p("Les courants politiques attribués par l'IA ont été partiellement recodés manuellement afin de correspondre aux catégories utilisées par Thomas More. Les thèmes ont fait l'objet du même retraitement. Un nettoyage manuel complémentaire a été effectué pour retirer les scories (personnalités mal identifiées, thèmes incohérents, doublons)."),
        h1("Avertissement"),
        p("Ce dashboard ne prétend pas valider la méthodologie de l'Institut Thomas More ni affirmer que l'analyse par IA générative constitue une mesure fiable de l'orientation idéologique des médias. L'objectif est triple : répliquer la démarche sur France Info afin de comparer les résultats avec ceux publiés par Thomas More ; étendre l'analyse aux autres chaînes d'information en continu, ce que Thomas More n'a pas fait ; et documenter les biais méthodologiques détectés au fil de l'exercice (gestion de l'ironie, confusion entre fait et opinion, sensibilité au prompt, axe unidimensionnel gauche–droite, biais propres au modèle)."),
        p("Le code source du pipeline et du dashboard est disponible librement.")
      )
    ),

  ),

  nav_spacer(),  # pousse ce qui suit vers la droite

  nav_item(
    div(
      style = "margin-top: 0px; margin-bottom: -12px;",
      dateInput(
        "date_min",
        label = NULL,
        value = "2025-01-01",
        format = "dd/mm/yyyy",
        width = "150px"
      )
    )
  ),
  nav_item(
    div(
      style = "margin-top: 0px; margin-bottom: -12px;",
      dateInput(
        "date_max",
        label = NULL,
        value = "2025-12-31",
        format = "dd/mm/yyyy",
        width = "150px"
      )
    )
  ),
)


# ============================================================================
# 4. SERVER
# ============================================================================

server <- function(input, output, session) {

  # --- Source ---
  output$code_source <- renderPrint({
    cat(readLines("pipeline_thomas_more.py"), sep = "\n")
  })

  # --- Chargement ---
  data <- reactive({
    withProgress(message = "Chargement des données...", {
      load_data(input)
    })
  })

  # --- Mise à jour des filtres ---
  observe({
    d <- data()
    chaines <- sort(unique(na.omit(d$resume$chaine)))
    courants <- sort(unique(na.omit(d$perso$courant_politique)))
    theme_list <- sort(unique(na.omit(d$themes$theme)))

    updateRadioButtons(session, "perso_chaine", choices =
                         c("Toutes",chaines), selected = "Toutes")

    updateRadioButtons(session, "exp_chaine", choices =
                         c("Toutes",chaines), selected = "Toutes")

    updateRadioButtons(session, "theme_chaine", choices =
                         c("Toutes",chaines), selected = "Toutes")

    # updateSelectInput(session, "perso_chaine", choices = chaines, selected = chaines)
    # updateSelectInput(session, "perso_courant", choices = courants, selected = courants)
    # updateSelectInput(session, "theme_chaine", choices = chaines, selected = chaines)
    updateSelectInput(session, "theme_select", choices = theme_list, selected = theme_list)
    # updateSelectInput(session, "exp_chaine", choices = chaines, selected = chaines)
  })

  # ==== VALUE BOXES ====
  output$n_videos <- renderText({
    format(nrow(data()$resume), big.mark = " ")
  })
  output$n_mentions <- renderText({
    format(data()$nb_perso, big.mark = " ")
  })
  output$nb_themes <- renderText({
    format(data()$nb_themes, big.mark = " ")
  })
  output$note_moy <- renderText({
    m <- mean(data()$resume$note_base, na.rm = TRUE)
    sprintf("%.1f / 100", m)
  })

  # ==== TAB 1 : VUE D'ENSEMBLE ====

  output$plot_orientation_chaine <- renderPlotly({
    df <- data()$resume %>%
      filter(!is.na(chaine), !is.na(note_base), !is.na(orientation_base)) %>%
      mutate(
        # Transformer en axe -50 / +50
        # gauche = négatif, droite = positif, neutre = 0
        note_axe = case_when(
          orientation_base == "gauche" ~ -note_base,
          orientation_base == "droite" ~  note_base,
          TRUE ~ 0
        )
      ) %>%
      group_by(chaine) %>%
      summarise(note_moy = mean(note_axe, na.rm = TRUE),
                n = n(), .groups = "drop") %>%
      mutate(chaine = fct_reorder(chaine, note_moy))

    p <- ggplot(df, aes(x = chaine, y = note_moy, fill = note_moy)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      geom_hline(yintercept = 0, linewidth = 0.4, color = "grey30") +
      geom_text(aes(label = sprintf("%+.1f", note_moy)),
                hjust = ifelse(df$note_moy >= 0, -0.15, 1.15),
                size = 3.8, fontface = "bold") +
      scale_fill_gradient2(low = "#C62828", mid = "#9E9E9E", high = "#1565C0",
                           midpoint = 0) +
      coord_flip() +
      scale_y_continuous(limits = c(-100, 100),
                         breaks = seq(-100, 100, 10)) +
      labs(x = NULL, y = "← Gauche (-100) | Droite (+100) →") +
      theme_dashboard

    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(showlegend = FALSE)
  })

  output$plot_hist_notes <- renderPlotly({
    df <- data()$resume %>% filter(!is.na(note_base), !is.na(orientation_base))

    p <- ggplot(df, aes(x = note_base, fill = orientation_base)) +
      geom_histogram(binwidth = 5, alpha = 0.85, color = "white", linewidth = 0.2) +
      scale_fill_manual(values = pal_orientation, name = "Orientation") +
      labs(x = "Intensité du biais (0–100)", y = "Nombre de vidéos") +
      theme_dashboard

    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.15))
  })

  output$plot_gnd_chaine <- renderPlotly({
    df <- data()$resume %>%
      filter(!is.na(chaine), !is.na(orientation_base)) %>%
      count(chaine, orientation_base) %>%
      group_by(chaine) %>%
      mutate(pct = n / sum(n) * 100) %>%
      ungroup()

    p <- ggplot(df, aes(x = chaine, y = pct, fill = orientation_base)) +
      geom_col(position = "stack", width = 0.65) +
      scale_fill_manual(values = pal_orientation, name = "Orientation") +
      labs(x = NULL, y = "% des vidéos") +
      coord_flip() +
      theme_dashboard

    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.15))
  })

  output$plot_temporal <- renderPlotly({
    df <- data()$resume %>%
      filter(!is.na(date), !is.na(note_base), !is.na(chaine)) %>%
      mutate(
        # Transformer en axe -50 / +50
        # gauche = négatif, droite = positif, neutre = 0
        note_axe = case_when(
          orientation_base == "gauche" ~ -note_base / 2,
          orientation_base == "droite" ~  note_base / 2,
          TRUE ~ 0
        )
      ) %>%
      mutate(semaine = floor_date(date, "week")) %>%
      group_by(semaine, chaine) %>%
      summarise(note_moy = mean(note_axe, na.rm = TRUE),
                n = n(), .groups = "drop")

    p <- ggplot(df, aes(x = semaine, y = note_moy, color = chaine)) +
      geom_line(linewidth = 0.8, alpha = 0.8) +
      geom_point(aes(size = n), alpha = 0.5) +
      geom_hline(yintercept = 0, linewidth = 0.4, color = "grey30") +
      scale_color_manual(values = pal_chaine, name = "Chaîne") +
      scale_y_continuous(limits = c(-50, 50),
                         breaks = seq(-50, 50, 10)) +
      scale_size_continuous(range = c(1, 4), guide = "none") +
      labs(x = NULL, y = "← Gauche (-50) | Droite (+50) →") +
      theme_dashboard

    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.15))
  })

  # ==== TAB 2 : PERSONNALITÉS ====

  output$n_perso <- renderText({
    format(data()$nb_perso, big.mark = " ")
  })
  output$pct_positif <- renderText({
    pct <- mean(data()$perso$attitude > 0, na.rm = TRUE) * 100
    sprintf("%.1f %%", pct)
  })
  output$pct_negatif <- renderText({
    pct <- mean(data()$perso$attitude < 0, na.rm = TRUE) * 100
    sprintf("%.1f %%", pct)
  })
  output$attitude_moy <- renderText({
    m <- mean(data()$perso$attitude, na.rm = TRUE)
    sprintf("%.1f / 10", m)
  })

  perso_filtered <- reactive({
    df <- data()$perso
    # if (length(input$perso_chaine) > 0)
    if (input$perso_chaine != "Toutes")
      df <- df %>% filter(chaine %in% input$perso_chaine)
    if (length(input$perso_courant) > 0)
      df <- df %>% filter(courant_politique %in% input$perso_courant)
    df
  })

  output$plot_perso_attitude <- renderPlotly({
    df <- perso_filtered() %>%
      filter(!is.na(attitude), !is.na(nom)) %>%
      filter(!is.na(courant_politique)) %>%
      filter(type == "personnalité") %>%
      group_by(nom, courant_politique) %>%
      summarise(attitude = round(weighted.mean(attitude,poids_video, na.rm = TRUE),4),
                n = n(), .groups = "drop")
    # %>%
    #   filter(n >= input$perso_min_mentions)

    if (input$perso_sort == "attitude") {
      df <- df %>% mutate(nom = fct_reorder(nom, attitude))
    } else {
      df <- df %>% mutate(nom = fct_reorder(nom, n))
    }

    df <- df %>% slice_max(n, n = 30, with_ties = FALSE)

    p <- ggplot(df, aes(x = nom, y = attitude, fill = courant_politique)) +
      geom_col(width = 0.7, show.legend = FALSE) +
      geom_text(aes(y = 0.05, label = sprintf("(n=%d)", n)),
                hjust = ifelse(df$attitude >= 0, -0.1, 1.1),
                size = 3.2) +
      geom_hline(yintercept = 0, linewidth = 0.4) +
      scale_fill_manual(values = pal_courant) +
      # scale_fill_manual(values = c("TRUE" = "#2E7D32", "FALSE" = "#C62828")) +
      coord_flip() +
      labs(x = NULL, y = "Attitude moyenne (-10 / +10)") +
           # subtitle = sprintf("Min. %d mentions", input$perso_min_mentions)) +
      theme_dashboard +
      theme(axis.text.y = element_text(size = 9))

    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(legend = list(orientation = "h", y = -0.15))
      # layout(showlegend = T)
  })

  output$plot_perso_attitude_chaine2 <- renderPlotly({
    df <- perso_filtered() %>%
      filter(!is.na(attitude), !is.na(nom)) %>%
      filter(!is.na(courant_politique)) %>%
      filter(type == "personnalité") %>%
      group_by(chaine, nom, courant_politique) %>%
      summarise(attitude = round(weighted.mean(attitude,poids_video, na.rm = TRUE),4),
                n = n(), .groups = "drop")
    # %>%
    #   filter(n >= input$perso_min_mentions)

    if (input$perso_sort == "attitude") {
      df <- df %>% mutate(nom = fct_reorder(nom, attitude))
    } else {
      df <- df %>% mutate(nom = fct_reorder(nom, n))
    }

    df <- df %>%
      group_by(chaine) %>%
      slice_min(attitude, n = 10, with_ties = FALSE)

    p <- ggplot(df, aes(x = nom, y = attitude, fill = courant_politique)) +
      geom_col(width = 0.7, show.legend = FALSE) +
      geom_text(aes(y = 0.05, label = sprintf("(n=%d)", n)),
                hjust = ifelse(df$attitude >= 0, -0.1, 1.1),
                size = 3.2) +
      geom_hline(yintercept = 0, linewidth = 0.4) +
      scale_fill_manual(values = pal_courant) +
      # scale_fill_manual(values = c("TRUE" = "#2E7D32", "FALSE" = "#C62828")) +
      coord_flip() +
      facet_wrap(~chaine,scales ="free")+
      labs(x = NULL, y = "Attitude moyenne (-10 / +10)") +
      # subtitle = sprintf("Min. %d mentions", input$perso_min_mentions)) +
      theme_dashboard +
      theme(axis.text.y = element_text(size = 9))

    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(legend = list(orientation = "h", y = -0.15))
    # layout(showlegend = T)
  })

  output$plot_perso_attitude_chaine <- renderPlotly({
    df <- perso_filtered() %>%
      filter(!is.na(attitude), !is.na(nom)) %>%
      filter(!is.na(courant_politique)) %>%
      filter(type == "personnalité") %>%
      group_by(chaine) %>%
      summarise(attitude = round(weighted.mean(attitude,poids_video, na.rm = TRUE),4),
                n = n(), .groups = "drop")

    p <- ggplot(df, aes(x = chaine, y = attitude, fill = chaine)) +
      geom_col(width = 0.7, show.legend = FALSE) +
      geom_text(aes(y = 0.05, label = sprintf("(n=%d)", n)),
                hjust = ifelse(df$attitude >= 0, -0.1, 1.1),
                size = 3.2) +
      geom_hline(yintercept = 0, linewidth = 0.4) +
      scale_fill_manual(values = pal_chaine) +
      coord_flip() +
      labs(x = NULL, y = "Attitude moyenne (-10 / +10)") +
      theme_dashboard +
      theme(axis.text.y = element_text(size = 9))

    ggplotly(p, tooltip = c("x", "y")) %>% layout(showlegend = T)
  })


  output$plot_temporal_perso_attitude <- renderPlotly({

    df <- perso_filtered() %>%
      filter(!is.na(attitude), !is.na(nom)) %>%
      filter(!is.na(courant_politique)) %>%
      filter(type == "personnalité")

    df <- df %>%
      mutate(mois = floor_date(date, "month")) %>%
      group_by(nom, mois) %>%
      summarise(attitude = weighted.mean(attitude,poids_video, na.rm = TRUE),
                n = n(), .groups = "drop") %>%
      filter(n() > 10)

    p <- ggplot(df, aes(x = mois, y = attitude, color = nom)) +
      geom_line(linewidth = 0.8, alpha = 0.8) +
      geom_point(aes(size = n), alpha = 0.5) +
      geom_hline(yintercept = 0, linewidth = 0.4, color = "grey30") +
      # scale_color_manual(values = pal_chaine, name = "Personalité") +
      scale_y_continuous(limits = c(-5, 5),
                         breaks = seq(-5, 5, 1)) +
      scale_size_continuous(range = c(1, 4), guide = "none") +
      labs(x = NULL, y = "← Négatif | Positif →") +
      theme_dashboard

    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.15))
  })


  output$plot_perso_courant <- renderPlotly({
    df <- perso_filtered() %>%
      filter(!is.na(attitude), !is.na(courant_politique)) %>%
      group_by(courant_politique,chaine) %>%
      summarise(attitude = round(weighted.mean(attitude, poids_video, na.rm = TRUE),4),
                n = n(), .groups = "drop") %>%
      filter(n >= 3) %>%
      mutate(courant_politique = fct_reorder(courant_politique, attitude))

    p <- ggplot(df, aes(x = courant_politique, y = attitude,
                        fill = courant_politique)) +
      geom_col(width = 0.65, show.legend = FALSE) +
      geom_text(aes(y = 0.1, label = sprintf("(n=%d)", n)),
                hjust = ifelse(df$attitude >= 0, -0.1, 1.1),
                size = 3.2) +
      geom_hline(yintercept = 0, linewidth = 0.4) +
      scale_fill_manual(values = pal_courant) +
      scale_x_discrete(limits = rev) +
      facet_wrap(~ chaine) +
      coord_flip() +
      labs(x = NULL, y = "Attitude moyenne") +
      theme_dashboard

    ggplotly(p) %>% layout(showlegend = FALSE)
  })

  output$plot_perso_heatmap <- renderPlotly({
    df <- perso_filtered() %>%
      filter(!is.na(attitude), !is.na(chaine), !is.na(courant_politique)) %>%
      count(chaine, courant_politique) %>%
      group_by(chaine) %>%
      mutate(pct = round(n / sum(n) * 100,1)) %>%
      ungroup()


    p <- ggplot(df, aes(x = chaine, y = courant_politique,
                        fill = pct)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.1f", pct)),
                color = "white", fontface = "bold", size = 3.5) +
      scale_fill_gradient2(low = "#bbb", mid = "#9E9E9E", high = "#222222",
                           midpoint = 0, name = "Présence") +
      scale_y_discrete(limits = rev) +
      labs(x = NULL, y = NULL) +
      theme_dashboard +
      theme(panel.grid = element_blank())

    ggplotly(p)
  })

  output$plot_perso_sentiment <- renderPlotly({
    df <- perso_filtered() %>%
      filter(!is.na(attitude), !is.na(courant_politique)) %>%
      mutate(sentiment = case_when(
        attitude < -1 ~ "Négatif",
        attitude > 1  ~ "Positif",
        TRUE          ~ "Neutre"
      )) %>%
      count(courant_politique, sentiment) %>%
      group_by(courant_politique) %>%
      mutate(
        pct = round(n / sum(n) * 100,1),
        n = sum(n)
      ) %>%
      ungroup() %>%
      mutate(sentiment = factor(sentiment, levels = c("Négatif", "Neutre", "Positif")))

    p <- ggplot(df, aes(x = courant_politique, y = pct, fill = sentiment)) +
      geom_col(position = "stack", width = 0.65) +
      geom_text(aes(y = 105, label = sprintf("(n=%d)", n)),size = 3.2) +
      scale_fill_manual(values = c("Négatif" = "#C62828", "Neutre" = "#9E9E9E",
                                   "Positif" = "#2E7D32"), name = NULL) +
      scale_x_discrete(limits = rev) +
      coord_flip() +
      labs(x = NULL, y = "% des mentions") +
      theme_dashboard

    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.15))
  })

  output$plot_perso_orientation <- renderPlotly({
    df <- perso_filtered() %>%
      filter(!is.na(attitude), !is.na(courant_politique)) %>%
      mutate(sentiment = case_when(
        attitude < -1 ~ "Négatif",
        attitude > 1  ~ "Positif",
        TRUE          ~ "Neutre"
      )) %>%
      group_by(chaine, courant_politique, sentiment) %>%
      summarise(
        n = n()
        # n = sum(poids_video) # pondérer de la taille complète de la vidéo pour une seule mention, c'est un peur fort, et sans impact
      ) %>%
      group_by(chaine, courant_politique) %>%
      mutate(pct = n / sum(n) * 100) %>%
      ungroup() %>%
      mutate(sentiment = factor(sentiment, levels = c("Négatif", "Neutre", "Positif")))

    p <- ggplot(df, aes(x = courant_politique, y = pct, fill = sentiment)) +
      geom_col(position = "stack", width = 0.7) +
      scale_fill_manual(values = c("Négatif" = "#C62828", "Neutre" = "#9E9E9E",
                                   "Positif" = "#2E7D32"), name = NULL) +
      scale_x_discrete(limits = rev) +
      facet_wrap(~chaine, ncol = 2) +
      coord_flip() +
      labs(x = NULL, y = "% des mentions") +
      theme_dashboard +
      theme(strip.text = element_text(size = 9))

    ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.15))

    # ggplotly(p, height = max(400, length(top_themes) * 60)) %>%
    #   layout(legend = list(orientation = "h", y = -0.05))
  })

  output$table_perso <- renderDT({
    perso_filtered() %>%
      select(nom, courant_politique, attitude, ponderation,
             justification, citation, chaine, video_id) %>%
      datatable(
        filter = "top",
        options = list(pageLength = 15, scrollX = TRUE,
                       dom = "fltip",
                       language = list(url = "//cdn.datatables.net/plug-ins/1.13.7/i18n/fr-FR.json")),
        rownames = FALSE
      ) %>%
      formatRound("attitude", digits = 1)
  })

  # ==== TAB 3 : THÉMATIQUES ====

  output$n_themes <- renderText({
    format(data()$nb_themes, big.mark = " ")
  })
  output$n_themes_gauche <- renderText({
    pct <- mean(data()$themes$orientation_bias == "gauche", na.rm = TRUE) * 100
    sprintf("%.1f %%", pct)
  })
  output$n_themes_droite <- renderText({
    pct <- mean(data()$themes$orientation_bias == "droite", na.rm = TRUE) * 100
    sprintf("%.1f %%", pct)
  })
  output$biais_moy <- renderText({
    m <- mean(data()$themes$note_bias, na.rm = TRUE)
    sprintf("%.1f / 100", m)
  })

  themes_filtered <- reactive({
    df <- data()$themes
    # if (length(input$theme_chaine) > 0)
    if (input$theme_chaine != "Toutes")
      df <- df %>% filter(chaine %in% input$theme_chaine)
    if (length(input$theme_select) > 0)
      df <- df %>% filter(theme %in% input$theme_select)
    df
  })

  output$plot_theme_freq <- renderPlotly({
    df <- themes_filtered() %>%
      filter(!is.na(theme)) %>%
      count(theme, sort = TRUE) %>%
      mutate(theme = fct_reorder(theme, n))

    p <- ggplot(df, aes(x = theme, y = n)) +
      geom_col(fill = "#1565C0", width = 0.65) +
      coord_flip() +
      labs(x = NULL, y = "Nombre de détections") +
      theme_dashboard

    ggplotly(p)
  })

  output$plot_theme_bias <- renderPlotly({
    df <- themes_filtered() %>%
      filter(!is.na(theme), !is.na(note_bias)) %>%
      group_by(theme) %>%
      summarise(bias_moy = mean(note_bias, na.rm = TRUE),
                n = n(), .groups = "drop") %>%
      filter(n >= 3) %>%
      mutate(theme = fct_reorder(theme, bias_moy))

    p <- ggplot(df, aes(x = theme, y = bias_moy, fill = bias_moy)) +
      geom_col(width = 0.65, show.legend = FALSE) +
      scale_fill_gradient2(low = "#bbb", mid = "#9E9E9E", high = "#222222",
                           midpoint = median(df$bias_moy, na.rm = TRUE)) +
      coord_flip() +
      labs(x = NULL, y = "Biais moyen (0–100)") +
      theme_dashboard

    ggplotly(p)
  })

  output$plot_theme_heatmap <- renderPlotly({
    df <- themes_filtered() %>%
      filter(!is.na(theme), !is.na(chaine)) %>%
      count(chaine, theme) %>%
      group_by(chaine) %>%
      mutate(pct = n / sum(n) * 100) %>%
      ungroup()

    p <- ggplot(df, aes(x = chaine, y = theme, fill = pct)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.0f%%", pct)),
                size = 4, color = "black", fontface = "bold") +
      # scale_fill_viridis_c(option = "D", name = "% du total") +
      scale_fill_gradient2(low = "#fff", high = "#333",
                           name = "% du total des chaînes") +
      scale_y_discrete(limits = rev) +
      labs(x = NULL, y = NULL) +
      theme_dashboard +
      theme(panel.grid = element_blank(),
            axis.text.y = element_text(size = 12))

    ggplotly(p)
  })

  output$plot_theme_orientation <- renderPlotly({
    df <- themes_filtered() %>%
      filter(!is.na(theme), !is.na(orientation_bias), !is.na(chaine)) %>%
      count(chaine, theme, orientation_bias) %>%
      group_by(chaine, theme) %>%
      mutate(pct = n / sum(n) * 100) %>%
      ungroup() %>%
      mutate(orientation_bias = factor(orientation_bias,
                                       levels = c("gauche", "neutre", "droite")))

    top_themes <- themes_filtered() %>%
      count(theme, sort = TRUE) %>%
      # head(8) %>%
      pull(theme)

    df <- df %>% filter(theme %in% top_themes)

    p <- ggplot(df, aes(x = chaine, y = pct, fill = orientation_bias)) +
      geom_col(position = "stack", width = 0.7) +
      scale_fill_manual(values = pal_orientation, name = "Orientation") +
      facet_wrap(~theme, ncol = 5) +
      coord_flip() +
      labs(x = NULL, y = "%") +
      theme_dashboard +
      theme(strip.text = element_text(size = 9))

    ggplotly(p, height = max(400, length(top_themes) * 60)) %>%
      layout(legend = list(orientation = "h", y = -0.05))
  })

  # output$plot_theme_note <- renderPlotly({
  #   df <- themes_filtered() %>%
  #     filter(!is.na(theme), !is.na(orientation_bias), !is.na(chaine)) %>%
  #     mutate(
  #       # Transformer en axe -50 / +50
  #       # gauche = négatif, droite = positif, neutre = 0
  #       note_axe = case_when(
  #         orientation_bias == "gauche" ~ -note_bias / 2,
  #         orientation_bias == "droite" ~  note_bias / 2,
  #         TRUE ~ 0
  #       )
  #     ) %>%
  #     group_by(chaine, theme) %>%
  #     mutate(note_axe = mean(note_axe,na.rm=T)) %>%
  #     group_by(theme) %>%
  #     mutate(
  #       note_axe_mean = mean(note_axe,na.rm=T),
  #       note_axe_min = min(note_axe,na.rm=T),
  #       note_axe_max = max(note_axe,na.rm=T),
  #     ) %>%
  #     ungroup() %>%
  #     arrange(-note_axe_mean)
  #
  #   p <- ggplot(df, aes(x = theme, y = note_axe, col = chaine)) +
  #     geom_point(width = 0.7) +
  #     geom_segment(aes(x = theme, xend = theme,
  #                      y = note_axe_min,yend = note_axe_max),
  #                  width = 0.7) +
  #     scale_colour_manual(values = pal_chaine) +
  #     coord_flip() +
  #     labs(x = NULL, y = "Biais") +
  #     theme_dashboard +
  #     theme(strip.text = element_text(size = 9))
  #
  #   ggplotly(p) %>% layout(legend = list(orientation = "h", y = -0.05))
  # })

  output$table_themes <- renderDT({
    themes_filtered() %>%
      select(theme, sous_theme, note_bias, orientation_bias, chaine, video_id) %>%
      datatable(
        filter = "top",
        options = list(pageLength = 15, scrollX = TRUE,
                       dom = "fltip",
                       language = list(url = "//cdn.datatables.net/plug-ins/1.13.7/i18n/fr-FR.json")),
        rownames = FALSE
      ) %>%
      formatRound("note_bias", digits = 1)
  })

  # theme_class <- reactive({
  #   req(input$table_themes_row_last_clicked)
  #   s <- input$table_themes_row_last_clicked
  #   id <- themes_filtered()[s,]$video_id
  #   df_iramuteq %>%
  #     filter(video_id == id) %>%
  #     select(video_id,id_segment,text,classe)
  # })
  #
  # output$classe_text <- renderUI({
  #   HTML(colorise_segments_html(theme_class(),palettes$soft))
  # })

  output$classe_url <- renderUI({

    url <- paste0("https://www.youtube.com/watch?v=",theme_class()$video_id[1])
    url <- paste0("URL de la vidéo : <a href=",url,">",url,"</a>")

    HTML(url)
  })


  # ==== TAB 4 : EXPLORER ====

  observeEvent(input$exp_reset, {
    updateSelectInput(session, "exp_chaine",
                      selected = sort(unique(na.omit(data()$resume$chaine))))
    updateSliderInput(session, "exp_note_range", value = c(0, 100))
    updateSelectInput(session, "exp_orientation", selected = "Toutes")
    updateTextInput(session, "exp_search", value = "")
  })

  output$table_explorer <- renderDT({
    df <- data()$resume %>%
      filter(!is.na(note_base))

    # if (length(input$exp_chaine) > 0)
    if (input$exp_chaine != "Toutes")
      df <- df %>% filter(chaine %in% input$exp_chaine)

    df <- df %>%
      filter(note_base >= input$exp_note_range[1],
             note_base <= input$exp_note_range[2])

    if (input$exp_orientation != "Toutes")
      df <- df %>% filter(orientation_base == input$exp_orientation)

    if (nchar(input$exp_search) > 0) {
      pattern <- tolower(input$exp_search)
      df <- df %>%
        filter(str_detect(tolower(titre), fixed(pattern)) |
               str_detect(tolower(video_id), fixed(pattern)))
    }

    df %>%
      select(video_id, chaine, titre, date,
             note_base, orientation_base, nb_personnalites, nb_themes) %>%
      arrange(desc(note_base)) %>%
      datatable(
        selection = "single",
        filter = "top",
        options = list(pageLength = 20, scrollX = TRUE,
                       dom = "fltip",
                       order = list(list(4, "desc")),
                       language = list(url = "//cdn.datatables.net/plug-ins/1.13.7/i18n/fr-FR.json")),
        rownames = FALSE
      ) %>%
      formatRound("note_base", digits = 1) %>%
      formatStyle("orientation_base",
                  color = styleEqual(c("gauche", "neutre", "droite"),
                                     c("#C62828", "#616161", "#1565C0")),
                  fontWeight = "bold")
  })

  # ==== TAB 5 : DONNÉES BRUTES ====

  output$raw_resume <- renderDT({
    data()$resume %>%
      datatable(filter = "top",
                options = list(pageLength = 20, scrollX = TRUE, dom = "fltip"),
                rownames = FALSE)
  })

  output$raw_perso <- renderDT({
    data()$perso %>%
      datatable(filter = "top",
                options = list(pageLength = 20, scrollX = TRUE, dom = "fltip"),
                rownames = FALSE)
  })

  output$raw_themes <- renderDT({
    data()$themes %>%
      datatable(filter = "top",
                options = list(pageLength = 20, scrollX = TRUE, dom = "fltip"),
                rownames = FALSE)
  })
}


# ============================================================================
# 5. LANCEMENT
# ============================================================================

shinyApp(ui = ui, server = server)
