###########################################
## Master Thesis chantal Graßelt, Matrikelnummer: 4036856, Thema: Fertility Benefits in Germany: Assessing Their
#Relevance for Workforce Preferences 
###########################################


###########################################
## 0. Vorbereitung
###########################################

# Bereinige den Workspace
rm(list = ls())

# Liste der benötigten Pakete
packages <- c("dplyr", "tidyr", "readxl", "car", "sandwich", "lmtest", "MASS", "pscl")

# Fehlende Pakete installieren
to_install <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(to_install)) install.packages(to_install)

# Alle Pakete laden
lapply(packages, library, character.only = TRUE)

###########################################
## 1. Dateneinlesung
###########################################

# CSV-Datensatz einlesen 
survey <- read.csv("data_fertilitybenefits_2025-03-31_19-54.csv",
                   sep = ";", fileEncoding = "latin1")

# Excel-Datensatz (Assoziationen - manuell erstellte Bewertung von positiven, negativen und neutralen Assoziationen) einlesen
asso <- read_excel("assoziationen.xlsx", sheet = "Tabelle1")

###########################################
## 2. Datenbereinigung und Zusammenführung
###########################################

# 2.1 Variablen normalisieren: A102_01 in beiden Datensätzen
survey$A102_01 <- tolower(trimws(survey$A102_01))
asso$A102_01   <- tolower(trimws(asso$A102_01))

# 2.2 Aus assoziationen nur eindeutige Einträge behalten (falls doppelte A102_01 vorkommen)
asso_unique <- asso %>% distinct(A102_01, .keep_all = TRUE)

# 2.3 Finde den eindeutigen Identifikator in survey  
# In dem Originaldatensatz heißt die Personen-ID "CASE".  
# Erzeuge einen Datensatz mit eindeutigen Fällen:
survey_unique <- survey %>%
  group_by(CASE) %>%   # Gruppierung nach der eindeutigen Personen-ID
  slice(1) %>%         # Behalte pro Gruppe nur die erste Zeile
  ungroup()

# 2.4 Führe einen Left Join durch, um assoziative Daten zu ergänzen
survey_joined <- left_join(survey_unique, asso_unique, by = "A102_01")

# Prüfung der Zeilenzahl – diese sollte jetzt der Anzahl eindeutiger Fälle entsprechen (z. B. 103):
cat("Anzahl eindeutiger Fälle (survey_joined):", nrow(survey_joined), "\n")


survey_joined$A202_correct <- ifelse(survey_joined$A202 == 1, 1, 0)
survey_joined$A203_correct <- ifelse(survey_joined$A203 == 3, 1, 0)
survey_joined$A204_correct <- ifelse(survey_joined$A204 == 3, 1, 0)

# Berechnung der Summe der korrekt beantworteten Kontrollfragen
survey_joined$control_correct <- survey_joined$A202_correct +
  survey_joined$A203_correct +
  survey_joined$A204_correct

# Filterung nach den Kontrollfragen, wenn man zwei von drei falsch beobachtet hat fällt die Beobachtung aus dem Sample:
survey_joined <- survey_joined %>%
  filter(control_correct >= 2)

cat("Bereinigte Stichprobe (nach Kontrollfragenfilter):", nrow(survey_joined), "\n")


###########################################
## 3. Variablenkonstruktion
###########################################

# sqrt(mean((survey_joined$A402-mean(survey_joined$A402))^2))
# sd(survey_joined$A402)*sqrt(102/103)

# Dummy: Positive Assoziationen – Wertung "positiv" ergibt 1, ansonsten 0
survey_joined$assoc_pos <- ifelse(survey_joined$Wertung == "positiv", 1, 0)

# Dummy: Bekanntheit erstellen (A301)
survey_joined$awareness <- ifelse(survey_joined$A301 == 1, 1, 0)

# Dummy: Gender erstellen – Hier: 1 = weiblich (wenn A402==1), 0 = männlich (wenn A402==2), ansonsten NA
survey_joined$female <- ifelse(survey_joined$A402 == 1, 1,
                               ifelse(survey_joined$A402 == 2, 0, NA))

# Reproduktives Alter Variabe erstllen (A401 in Verbindung mit Geschlecht)
survey_joined$repr_age <- ifelse(
  (survey_joined$A401 %in% c(1, 2, 3) & survey_joined$female == 1) |
    (survey_joined$A401 %in% c(1, 2, 3, 4) & survey_joined$female == 0),
  1, 0)

# Innovation Variable erstellen: aus der Spalte A501_01 (denn in survey_joined heißt sie so)
survey_joined$innovation <- as.numeric(survey_joined$A501_01)

# Variable Erfahrung mit Fruchtbarkeit: Dummy (A701)
survey_joined$experience <- ifelse(survey_joined$A701 == 1, 1, 0)

# Bildung gruppieren: A403 wird in "Niedrig", "Mittel" oder "Hoch" kodiert
survey_joined$edu_group <- case_when(
  survey_joined$A403 %in% c(1, 2) ~ "Niedrig",
  survey_joined$A403 %in% c(3, 4, 9) ~ "Mittel",
  survey_joined$A403 %in% c(5, 6, 7, 8) ~ "Hoch",
  survey_joined$A403 == 10 ~ NA_character_
)
survey_joined$edu_group <- factor(survey_joined$edu_group, levels = c("Niedrig", "Mittel", "Hoch"))

survey_joined$health_group <- case_when(
  survey_joined$A502 %in% c(1, 2) ~ "Gut",
  survey_joined$A502 %in% c(3, 4, 5) ~ "Other",  
  survey_joined$A502 == 6 ~ NA_character_
)
survey_joined$health_group <- factor(survey_joined$health_group, levels = c("Gut", "Other"))

# Kinder: Dummy aus A405
survey_joined$children <- ifelse(survey_joined$A405 == 1, 1, 0)

# Wichtigkeit der Fertility Benefits: Spalte A901_01 aus dem Datensatz,
# da beim Join der Spaltenname angepasst wurde.
survey_joined$importance <- as.numeric(survey_joined$A901_01)

###########################################
## 4. OLS Regression und Modelle
###########################################

# 4.1 OLS-Modell
# Standardisierung der abhängigen Variable und relevanter unabhängiger Variablen
survey_joined <- survey_joined %>%
  mutate(
    importance_standardized = scale(importance, center = TRUE, scale = TRUE),
    innovation_standardized = scale(innovation, center = TRUE, scale = TRUE)
  )

# 1. OLS-Modell mit standardisierten Variablen
model_ols_standardized <- lm(importance_standardized ~ assoc_pos + awareness + repr_age + female +
                               innovation_standardized + experience + edu_group + health_group + children,
                             data = survey_joined)

cat("\n--- OLS Regression Summary (standardisierte Variablen) ---\n")
print(summary(model_ols_standardized))

# Robuste Standardfehler (HC1)
robust_se_standardized <- vcovHC(model_ols_standardized, type = "HC1")
cat("\n--- Robuste Standardfehler (standardisierte Variablen) ---\n")
print(coeftest(model_ols_standardized, vcov = robust_se_standardized))

# Konfidenzintervalle
cat("\n--- Konfidenzintervalle (standardisierte Variablen) ---\n")
print(confint(model_ols_standardized))

# Multikollinearitätsprüfung
cat("\n--- VIF (standardisierte Variablen) ---\n")
print(vif(model_ols_standardized))

# Breusch-Pagan-Test auf Heteroskedastizität
cat("\n--- Breusch-Pagan-Test (standardisierte Variablen) ---\n")
print(bptest(model_ols_standardized))

---
  
# 2. Interaktion hinzufügen (basierend auf standardisierten Variablen)
  survey_joined <- survey_joined %>%
  mutate(female_experience_interaction_standardized = female * experience)

# OLS-Modell mit Interaktion und standardisierten Variablen
model_ols_interaction_standardized <- lm(importance_standardized ~ assoc_pos + awareness + repr_age + female +
                                           innovation_standardized + experience + edu_group + health_group + children +
                                           female_experience_interaction_standardized,
                                         data = survey_joined)

cat("\n--- OLS Regression Summary (standardisierte Variablen mit Interaktion) ---\n")
print(summary(model_ols_interaction_standardized))

# Robuste Standardfehler (HC1) für das Modell mit Interaktion
robust_se_interaction_standardized <- vcovHC(model_ols_interaction_standardized, type = "HC1")
cat("\n--- Robuste Standardfehler (mit Interaktion) ---\n")
print(coeftest(model_ols_interaction_standardized, vcov = robust_se_interaction_standardized))

# Konfidenzintervalle für das Interaktionsmodell
cat("\n--- Konfidenzintervalle (mit Interaktion) ---\n")
print(confint(model_ols_interaction_standardized))

# Multikollinearitätsprüfung (VIF) für das Interaktionsmodell
cat("\n--- VIF (mit Interaktion) ---\n")
print(vif(model_ols_interaction_standardized))

library(ggplot2)
library(dplyr)

# Gruppenmittelwerte + Konfidenzintervalle berechnen
summary_data <- survey_joined %>%
  group_by(experience, female) %>%
  summarise(
    mean_importance = mean(importance, na.rm = TRUE),
    sd = sd(importance, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    ci = 1.96 * se  # für 95%-Konfidenzintervall
  )

# Plot erstellen, Wichtigkeit Fertility Benefits Frauen vs. Männer
ggplot(summary_data, aes(x = factor(experience), y = mean_importance, fill = factor(female))) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_importance - ci, ymax = mean_importance + ci),
                position = position_dodge(width = 0.8), width = 0.2) +
  labs(
    x = "Experience with fertility treatment (0 = No, 1 = Yes)",
    y = "Mean Importance of Fertility Benefits",
    fill = "Gender (0 = Men, 1 = Women)"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 12))

# Verteilung von Geschlecht prüfen
cat("\n--- Verteilung von female ---\n")
print(table(survey_joined$female))

# Verteilung der Interaktion prüfen
cat("\n--- Verteilung der Interaktion female x experience ---\n")
survey_joined <- survey_joined %>%
  mutate(female_experience_interaction = female * experience)
print(table(survey_joined$female_experience_interaction))

# Verteilung der Erfahrung nach Geschlecht prüfen
male_experience <- survey_joined %>% filter(female == 0)
female_experience <- survey_joined %>% filter(female == 1)

# Verteilung der Erfahrung bei Männern prüfen
cat("\n--- Verteilung der Erfahrung bei Männern (female = 0) ---\n")
print(summary(male_experience$experience))

# Verteilung der Erfahrung bei Frauen prüfen
cat("\n--- Verteilung der Erfahrung bei Frauen (female = 1) ---\n")
print(summary(female_experience$experience))

# Anzahl Männer je Erfahrungslevel prüfen
male_experience_summary <- male_experience %>%
  group_by(experience) %>%
  summarise(count = n())

cat("\n--- Anzahl Männer nach Erfahrung ---\n")
print(male_experience_summary)



# 4.2 Ordered Logit Modell
# Wichtigkeit als geordneter Faktor
survey_joined$importance_factor <- factor(survey_joined$importance, ordered = TRUE)
model_ordered_logit <- polr(importance_factor ~ assoc_pos + awareness + repr_age + female +
                              innovation + experience + edu_group + health_group + children,
                            data = survey_joined, Hess = TRUE)
cat("\n--- Ordered Logit Summary ---\n")
print(summary(model_ordered_logit))

# Berechnung der p-Werte für das Ordered Logit Modell
ordered_logit_summary <- summary(model_ordered_logit)
model_ordered_logit_pvalues <- pnorm(abs(ordered_logit_summary$coefficients[, "t value"]),
                                     lower.tail = FALSE) * 2
cat("\n--- Ordered Logit p-Werte ---\n")
print(model_ordered_logit_pvalues)

# Berechnung von McFadden's R²
cat("\n--- McFadden's R² (Ordered Logit) ---\n")
loglik_full <- logLik(model_ordered_logit)  # Log-Likelihood des vollständigen Modells
loglik_null <- logLik(polr(importance_factor ~ 1, data = survey_joined, Hess = TRUE))  # Nullmodell (nur Konstante)

mcfadden_r2 <- 1 - (as.numeric(loglik_full) / as.numeric(loglik_null))
cat("McFadden's R²:", round(mcfadden_r2, 4), "\n")

# Interaktion zwischen female und experience für Ordered Logit erstellen
survey_joined <- survey_joined %>%
  mutate(female_experience_interaction = female * experience)

# Ordered Logit Modell mit der Interaktion
model_ordered_logit_interaction <- polr(importance_factor ~ assoc_pos + awareness + repr_age + female +
                                          innovation + experience + edu_group + health_group + children +
                                          female_experience_interaction,
                                        data = survey_joined, Hess = TRUE)

# Ausgabe der Ergebnisse
cat("\n--- Ordered Logit Summary (mit Interaktion) ---\n")
print(summary(model_ordered_logit_interaction))

# Berechnung der p-Werte für das neue Ordered Logit Modell
ordered_logit_interaction_summary <- summary(model_ordered_logit_interaction)
model_ordered_logit_interaction_pvalues <- pnorm(abs(ordered_logit_interaction_summary$coefficients[, "t value"]),
                                                 lower.tail = FALSE) * 2
cat("\n--- Ordered Logit p-Werte (mit Interaktion) ---\n")
print(model_ordered_logit_interaction_pvalues)

loglik_full <- logLik(model_ordered_logit_interaction) # Log-Likelihood des vollständigen Modells
loglik_null <- logLik(polr(importance_factor ~ 1, data = survey_joined, Hess = TRUE)) # Nullmodell
mcfadden_r2 <- 1 - (as.numeric(loglik_full) / as.numeric(loglik_null))
cat("McFadden's R²:", round(mcfadden_r2, 4), "\n")


# 4.3 Logit-Modell (Job Choice)

# Funktion zur Durchführung einer Logit-Regression für ein einzelnes Szenario
run_logit_for_scenario <- function(data, scenario, predictors) {
  # Filter für das spezifische Szenario
  scenario_data <- data %>%
    dplyr::select(all_of(c(scenario, predictors))) %>%
    rename(choice = !!scenario) %>%  # Umbenennen für Konsistenz
    mutate(choice = as.numeric(choice) - 1)  # Umkodierung: 1 -> 0, 2 -> 1
  
  # Logit-Regression für das Szenario
  model <- glm(choice ~ ., data = scenario_data, family = binomial(link = "logit"))
  
  # Modellzusammenfassung ausgeben
  cat("\n--- Logit Regression for", scenario, "---\n")
  print(summary(model))
  
  # Breusch-Pagan-Test auf Heteroskedastizität
  cat("\n--- Breusch-Pagan-Test (Heteroskedastizität) ---\n")
  print(bptest(model))
  
  # Robuste Standardfehler berechnen
  cat("\n--- Robuste Standardfehler (Logit) ---\n")
  robust_se <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  print(robust_se)
  
  # Odds Ratios berechnen
  cat("\n--- Odds Ratios (Logit) ---\n")
  odds_ratios <- exp(coef(model))
  print(odds_ratios)
  
  # Konfidenzintervalle der Odds Ratios
  cat("\n--- Konfidenzintervalle der Odds Ratios (Logit) ---\n")
  conf_intervals <- exp(confint(model))
  print(conf_intervals)
  
  # Pseudo-R² berechnen
  cat("\n--- Pseudo-R² (Logit) ---\n")
  pseudo_r2 <- pscl::pR2(model)
  print(pseudo_r2)
  
  # Rückgabe des Modells (für spätere Verwendung)
  return(list(model = model, robust_se = robust_se, odds_ratios = odds_ratios,
              conf_intervals = conf_intervals, pseudo_r2 = pseudo_r2))
}

# Liste der Szenarien und Prädiktoren
scenarios <- c("A905", "A906", "A907", "A908", "A909", "A910")
predictors <- c("assoc_pos", "awareness", "repr_age", "female", 
                "innovation", "experience", "edu_group", "health_group", "children")

# Regressionen für alle Szenarien durchführen und Ergebnisse speichern
logit_results <- lapply(scenarios, function(scenario) {
  run_logit_for_scenario(survey_joined, scenario, predictors)
}) 


###########################################
## 5. Deskriptive Statistiken
###########################################

# 5.1 Durchschnittsalter (basierend auf A401, wobei jede Kategorie einen Mittelwert erhält)
average_age_desc <- survey_joined %>%
  filter(!is.na(A401)) %>%
  summarise(average_age = mean(case_when(
    A401 == 1 ~ 21,
    A401 == 2 ~ 29.5,
    A401 == 3 ~ 39.5,
    A401 == 4 ~ 49.5,
    A401 == 5 ~ 59.5,
    A401 == 6 ~ 70
  ), na.rm = TRUE))
cat("\n--- Durchschnittsalter ---\n")
print(average_age_desc)

# Alterskategorien in beschreibende Labels umwandeln
survey_joined <- survey_joined %>%
  mutate(age_group = case_when(
    A401 == 1 ~ "18-24",
    A401 == 2 ~ "25-34",
    A401 == 3 ~ "35-44",
    A401 == 4 ~ "45-54",
    A401 == 5 ~ "55-64",
    A401 == 6 ~ "65+"
  ))

# Altersgruppen als Faktor (für korrekte Reihenfolge)
survey_joined$age_group <- factor(survey_joined$age_group, 
                                  levels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"))

# Histogramm der Altersverteilung mit Altersstufen
library(ggplot2)
ggplot(survey_joined, aes(x = age_group)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Altersverteilung", x = "Altersstufen", y = "Häufigkeit") +
  theme_minimal()

# Tabelle mit Häufigkeiten der Alterskategorien
age_distribution <- table(survey_joined$age_group)
print(age_distribution)


# Häufigkeiten der Likert-Skala berechnen
importance_counts <- table(survey_joined$importance_factor)

# Häufigkeiten anzeigen
print(importance_counts)

# Prozentuale Verteilung berechnen der Wichtigkeit von FErtility Benefits Likert Skala 
importance_percent <- prop.table(importance_counts) * 100
print(round(importance_percent, 2))


# 5.2 Geschlechtsverteilung
gender_distribution_desc <- survey_joined %>%
  filter(!is.na(A402)) %>%
  group_by(A402) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(gender = case_when(
    A402 == 1 ~ "weiblich",
    A402 == 2 ~ "männlich",
    A402 == 3 ~ "divers",
    A402 == 4 ~ "keine Angabe"
  ))
cat("\n--- Geschlechtsverteilung ---\n")
print(gender_distribution_desc)

# 5.3 Ranking-Daten: Häufigkeit für jeden Benefit pro Placierung
ranking_summary_desc <- survey_joined %>%
  dplyr::select(A904_01, A904_02, A904_03, A904_04, A904_05) %>%
  pivot_longer(cols = c(A904_01, A904_02, A904_03, A904_04, A904_05),
               names_to = "rank_position",
               values_to = "benefit") %>%
  mutate(
    rank_position = case_when(
      rank_position == "A904_01" ~ "Platz 1",
      rank_position == "A904_02" ~ "Platz 2",
      rank_position == "A904_03" ~ "Platz 3",
      rank_position == "A904_04" ~ "Platz 4",
      rank_position == "A904_05" ~ "Platz 5"
    ),
    benefit_name = case_when(
      benefit == 1 ~ "Fertility Benefits",
      benefit == 2 ~ "4-Tage-Woche",
      benefit == 3 ~ "Kinderbetreuungszuschuss",
      benefit == 4 ~ "Sabbatical",
      benefit == 5 ~ "100% remote work"
    )
  ) %>%
  group_by(rank_position, benefit_name) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(rank_position, desc(count))
cat("\n--- Ranking Zusammenfassung ---\n")
print(ranking_summary_desc)

# 5.4 Gründe für Wichtigkeit von Fertility Benefits (A902)
fertility_reasons_desc <- survey_joined %>%
  filter(!is.na(A902)) %>%
  group_by(A902) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(reason = case_when(
    A902 == 1 ~ "wichtig für Familienplanung",
    A902 == 2 ~ "Unterstützung bei Fruchtbarkeitsproblemen",
    A902 == 3 ~ "finanzielle Unterstützung für kostspielige Behandlungen",
    A902 == 4 ~ "Erhöhung der Arbeitgeberattraktivität",
    A902 == 5 ~ "Verbesserung der Work-Life Balance",
    A902 == 6 ~ "keine Relevanz für Lebenssituation",
    A902 == 7 ~ "kein Interesse an Fruchtbarkeitsbehandlungen",
    A902 == 8 ~ "das Thema ist mir zu persönlich",
    A902 == 9 ~ "Thema unabhängig von Lebenssituation nicht relevant",
    A902 == 10 ~ "anderer Grund"
  ))
cat("\n--- Gründe für Fertility Benefits ---\n")
print(fertility_reasons_desc)

# 5.5 Wichtigkeit von Social Freezing vs. Kinderwunsch (A903)
social_freezing_importance_desc <- survey_joined %>%
  filter(!is.na(A903)) %>%
  group_by(A903) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(preference = case_when(
    A903 == 1 ~ "Kinderwunschbehandlung",
    A903 == 2 ~ "Social Freezing",
    A903 == 3 ~ "Weder noch",
    A903 == 4 ~ "beides gleichermaßen wichtig"
  ))
cat("\n--- Social Freezing vs. Kinderwunsch ---\n")
print(social_freezing_importance_desc)

# 5.6 Beschäftigungsstatus (A601)
employment_status_desc <- survey_joined %>%
  filter(!is.na(A601)) %>%
  group_by(A601) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(status = case_when(
    A601 == 1 ~ "Arbeitnehmer",
    A601 == 2 ~ "Arbeitgeber",
    A601 == 3 ~ "Selbständig",
    A601 == 4 ~ "Schüler",
    A601 == 5 ~ "Student",
    A601 == 6 ~ "Arbeitslos",
    A601 == 7 ~ "Andere"
  ))
cat("\n--- Beschäftigungsstatus ---\n")
print(employment_status_desc)

# 5.7 Sektor (A602)
employment_sector_desc <- survey_joined %>%
  filter(!is.na(A602)) %>%
  group_by(A602) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(sector = case_when(
    A602 == 1 ~ "Baugewerbe",
    A602 == 2 ~ "Dienstleistung",
    A602 == 3 ~ "Gastgewerbe",
    A602 == 4 ~ "Gesundheits- und Sozialwesen",
    A602 == 5 ~ "Handel",
    A602 == 6 ~ "Handwerk",
    A602 == 7 ~ "Information und Kommunikation",
    A602 == 8 ~ "Infrastruktur",
    A602 == 9 ~ "Kunst, Unterhaltung und Erholung",
    A602 == 10 ~ "Verarbeitendes Gewerbe",
    A602 == 11 ~ "noch studierend",
    A602 == 12 ~ "Anderes",
    A602 == 13 ~ "nicht beschäftigt"
  ))
cat("\n--- Beschäftigungssektor ---\n")
print(employment_sector_desc)

# 5.8 Familienstand (A404)
marital_status_desc <- survey_joined %>%
  filter(!is.na(A404)) %>%
  group_by(A404) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(status = case_when(
    A404 == 1 ~ "Ledig",
    A404 == 2 ~ "verheiratet",
    A404 == 3 ~ "verwitwet",
    A404 == 4 ~ "geschieden",
    A404 == 5 ~ "in einer Beziehung",
    A404 == 6 ~ "keine Angabe"
  ))
cat("\n--- Familienstand ---\n")
print(marital_status_desc)

# Erstelle ein neues Long-Format für die Jobangebote aus survey_joined
job_data_summary <- survey_joined %>%
  dplyr::select(A905, A906, A907, A908, A909, A910) %>%  # Wähle die relevanten Spalten aus
  pivot_longer(cols = c(A905, A906, A907, A908, A909, A910),
               names_to = "job_offer",
               values_to = "choice") %>%
  # Kodierung der Entscheidung und Beschreibung der Jobangebote
  mutate(choice = as.numeric(choice) - 1,  # Umkodierung: 1 -> 0 und 2 -> 1
         job_offer_description = case_when(
           job_offer == "A905" ~ "Job 1: 55,000 salary vs. job 2: 52,500 + fertility benefits",
           job_offer == "A906" ~ "Job 1: 4-Tage-Woche vs. Job 2: Fertility Benefits",
           job_offer == "A907" ~ "Job 1: 100% Remote vs. Job 2: Fertility Benefits",
           job_offer == "A908" ~ "Job 1: Berlin vs. Job 2: Neubrandenburg + Fertility Benefits",
           job_offer == "A909" ~ "Job 1: 55.000 Gehalt vs. Job 2: 54.000 + Fertility Benefits",
           job_offer == "A910" ~ "Job 1: Kinderbetreuungszuschuss vs. Job 2: Fertility Benefits"
         ),
         choice_description = case_when(
           choice == 0 ~ "Job 1",
           choice == 1 ~ "Job 2"
         ))

# Gruppierung nach Jobangebot und Entscheidung, und Berechnung der Häufigkeit
job_offer_summary <- job_data_summary %>%
  group_by(job_offer_description, choice_description) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(job_offer_description, desc(count))

# Ausgabe der Häufigkeiten
cat("\n--- Häufigkeiten der Jobangebote ---\n")
print(job_offer_summary)

# Visualisierung der Ergebnisse
library(ggplot2)
ggplot(job_offer_summary, aes(x = reorder(job_offer_description, -count), y = count, fill = choice_description)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Frequency of the selected job offers",
       x = "Job Offer",
       y = "Number of decisions",
       fill = "Selected job") +
  theme_minimal()


calculate_summary <- function(variable, name) {
  cat("\n--- Summary Statistics for", name, "---\n")
  if (is.numeric(variable)) {
    cat("Min:", min(variable, na.rm = TRUE), "\n")
    cat("Max:", max(variable, na.rm = TRUE), "\n")
    cat("Mean:", mean(variable, na.rm = TRUE), "\n")
    cat("Std. Dev:", sd(variable, na.rm = TRUE), "\n")
  } else {
    cat("Variable is not numeric, cannot calculate min/max/mean/std.dev.\n")
    cat("Value Counts:\n")
    print(table(variable, useNA = "ifany"))
  }
}

# 1) Assoziationen (Freitext - keine numerische Zusammenfassung möglich)
cat("\n--- Assoziationen (Freitext) ---\n")
cat("Freitext-Variablen können nicht numerisch zusammengefasst werden.\n")

# 2) Bekanntheit Fertility Benefits (A301)
calculate_summary(survey_joined$awareness, "Awareness (Dummy)")

# 3) Alter (A401 und Reproduktives Alter)
calculate_summary(survey_joined$A401, "Age (Raw - A401)")
calculate_summary(survey_joined$repr_age, "Reproductive Age (Dummy)")

# 4) Geschlecht (A402 und Dummy)
calculate_summary(survey_joined$A402, "Gender (Raw - A402)")
calculate_summary(survey_joined$female, "Gender (Dummy - Female)")

# 5) Bildungsabschluss (A403 und Gruppiert)
calculate_summary(survey_joined$A403, "Education Level (Raw - A403)")
calculate_summary(survey_joined$edu_group, "Education Level (Grouped)")

# 6) Familienstand (A404)
calculate_summary(survey_joined$A404, "Marital Status (Raw - A404)")

# 7) Kinder (A405 und Dummy)
calculate_summary(survey_joined$A405, "Children (Raw - A405)")
calculate_summary(survey_joined$children, "Children (Dummy)")

# 8) Innovation (A501_01)
calculate_summary(survey_joined$A501_01, "Innovation (Raw - A501_01)")

# 9) Gesundheitszustand (A502 und Gruppiert)
calculate_summary(survey_joined$A502, "Health Status (Raw - A502)")
calculate_summary(survey_joined$health_group, "Health Status (Grouped)")

# 10) Beschäftigungsstatus (A601)
calculate_summary(survey_joined$A601, "Employment Status (Raw - A601)")

# 11) Branchensektor (A602)
calculate_summary(survey_joined$A602, "Sector (Raw - A602)")

# 12) Erfahrungen mit Unfruchtbarkeit (A701 und Dummy)
calculate_summary(survey_joined$A701, "Infertility Experience (Raw - A701)")
calculate_summary(survey_joined$experience, "Infertility Experience (Dummy)")

# 13) Wichtigkeit Fertility Benefits (A901 und Dummy)
calculate_summary(survey_joined$A901_01, "Importance (Raw - A901_01)")
calculate_summary(survey_joined$importance, "Importance (Dummy)")

# Erstelle eine Frequenz-Tabelle für die Variable 'importance'- FErtility Benefits Likert Skala
importance_freq <- table(survey_joined$importance)

# Ausgabe der absoluten Häufigkeiten
print(importance_freq)

# Optional: Berechne auch relative Häufigkeiten (Prozente)
importance_prop <- prop.table(importance_freq) * 100
print(importance_prop)


# 14 JObangebote verschiedene Szenarien

calculate_summary(survey_joined$A905, "Job-Szenario 1 (A905)")
calculate_summary(survey_joined$A906, "Job-Szenario 2 (A906)")
calculate_summary(survey_joined$A907, "Job-Szenario 3 (A907)")
calculate_summary(survey_joined$A908, "Job-Szenario 4 (A908)")
calculate_summary(survey_joined$A909, "Job-Szenario 5 (A909)")
calculate_summary(survey_joined$A910, "Job-Szenario 6 (A910)")

# 15) Interaktion (Geschlecht * Erfahrung)
calculate_summary(survey_joined$female_experience_interaction, "Female * Experience Interaction")

# 16) assoziationen
calculate_summary(survey_joined$assoc_pos, "Positive Associations (Dummy)")

# 17) Wichtikeit Benefits 
calculate_summary(survey_joined$A903, "Importance: Social Freezing vs. Kinderwunsch (A903)")

# 18) Gründe für Fertiltiy Benefits
calculate_summary(survey_joined$A902, "Reasons for Fertility Benefits (A902)")

# 19) Ranking 
calculate_summary(survey_joined$A904_01, "Ranking Platz 1")
calculate_summary(survey_joined$A904_02, "Ranking Platz 2")
calculate_summary(survey_joined$A904_03, "Ranking Platz 3")
calculate_summary(survey_joined$A904_04, "Ranking Platz 4")
calculate_summary(survey_joined$A904_05, "Ranking Platz 5")

# 20) Kontrollfragen
# Summary Statistics für die originalen Kontrollfragen
calculate_summary(survey_joined$A202, "Control Question 1 (A202 - Raw)")
calculate_summary(survey_joined$A203, "Control Question 2 (A203 - Raw)")
calculate_summary(survey_joined$A204, "Control Question 3 (A204 - Raw)")

### Explorativer Teil - nicht mehr Teil der Hauptanalyse ###

# Standardisierung der abhängigen und relevanten unabhängigen Variablen
survey_joined <- survey_joined %>%
  mutate(
    importance_standardized = scale(importance, center = TRUE, scale = TRUE),
    innovation_standardized = scale(innovation, center = TRUE, scale = TRUE)
  )

# OLS Regression ohne repr_age
model_ols_explorative <- lm(importance_standardized ~ assoc_pos + awareness + female +
                              innovation_standardized + experience + edu_group +
                              health_group + children, data = survey_joined)

cat("\n--- Exploratives OLS Regression Summary (ohne repr_age) ---\n")
print(summary(model_ols_explorative))

# Robuste Standardfehler (HC1)
robust_se_explorative <- vcovHC(model_ols_explorative, type = "HC1")
cat("\n--- Robuste Standardfehler (Exploratives OLS, ohne repr_age) ---\n")
print(coeftest(model_ols_explorative, vcov = robust_se_explorative))

# Konfidenzintervalle
cat("\n--- Konfidenzintervalle (Exploratives OLS, ohne repr_age) ---\n")
print(confint(model_ols_explorative))

# Multikollinearitätsprüfung (VIF)
cat("\n--- VIF (Exploratives OLS, ohne repr_age) ---\n")
print(vif(model_ols_explorative))

# Adjusted R²
cat("\n--- Adjusted R² (Exploratives OLS, ohne repr_age) ---\n")
cat("Adjusted R²:", summary(model_ols_explorative)$adj.r.squared, "\n")

# Interaktion zwischen female und experience erstellen (nicht standardisiert)
survey_joined <- survey_joined %>%
  mutate(
    female_experience_interaction = female * experience
  )

# OLS Regression ohne repr_age, aber mit Interaktion
model_ols_interaction_explorative <- lm(importance_standardized ~ assoc_pos + awareness + female +
                                          innovation_standardized + experience + edu_group + health_group +
                                          children + female_experience_interaction, data = survey_joined)

cat("\n--- Exploratives OLS Regression Summary (mit Interaktion, ohne repr_age) ---\n")
print(summary(model_ols_interaction_explorative))

# Robuste Standardfehler (HC1) für das Interaktionsmodell
robust_se_interaction_explorative <- vcovHC(model_ols_interaction_explorative, type = "HC1")
cat("\n--- Robuste Standardfehler (mit Interaktion, ohne repr_age) ---\n")
print(coeftest(model_ols_interaction_explorative, vcov = robust_se_interaction_explorative))

# Konfidenzintervalle
cat("\n--- Konfidenzintervalle (mit Interaktion, ohne repr_age) ---\n")
print(confint(model_ols_interaction_explorative))

# Multikollinearitätsprüfung (VIF)
cat("\n--- VIF (mit Interaktion, ohne repr_age) ---\n")
print(vif(model_ols_interaction_explorative))

# Adjusted R²
cat("\n--- Adjusted R² (mit Interaktion, ohne repr_age) ---\n")
cat("Adjusted R²:", summary(model_ols_interaction_explorative)$adj.r.squared, "\n")



# Exploratives Ordered Logit-Modell ohne repr_age
model_ordered_logit_explorative <- polr(importance_factor ~ assoc_pos + awareness + female +
                                          innovation + experience + edu_group + health_group + children,
                                        data = survey_joined, Hess = TRUE)
cat("\n--- Exploratives Ordered Logit Summary ---\n")
print(summary(model_ordered_logit_explorative))

# Berechnung der p-Werte für das explorative Ordered Logit-Modell
ordered_logit_summary_explorative <- summary(model_ordered_logit_explorative)
model_ordered_logit_pvalues_explorative <- pnorm(abs(ordered_logit_summary_explorative$coefficients[, "t value"]),
                                                 lower.tail = FALSE) * 2
cat("\n--- Ordered Logit p-Werte (Explorativ) ---\n")
print(model_ordered_logit_pvalues_explorative)

# Berechnung von McFadden's R² für das Modell ohne Interaktion
cat("\n--- McFadden's R² (Explorativ ohne Interaktion) ---\n")
loglik_full <- logLik(model_ordered_logit_explorative)  # Log-Likelihood des vollständigen Modells
loglik_null <- logLik(polr(importance_factor ~ 1, data = survey_joined, Hess = TRUE))  # Nullmodell (nur Konstante)

mcfadden_r2 <- 1 - (as.numeric(loglik_full) / as.numeric(loglik_null))
cat("McFadden's R²:", round(mcfadden_r2, 4), "\n")


# Interaktion zwischen female und experience erstellen
survey_joined <- survey_joined %>%
  mutate(female_experience_interaction = female * experience)

# Exploratives Ordered Logit-Modell ohne repr_age, aber mit der Interaktion
model_ordered_logit_explorative_interaction <- polr(importance_factor ~ assoc_pos + awareness + female +
                                                      innovation + experience + edu_group + health_group + children +
                                                      female_experience_interaction,
                                                    data = survey_joined, Hess = TRUE)

# Ausgabe der Modellzusammenfassung
cat("\n--- Exploratives Ordered Logit Summary mit Interaktion ---\n")
print(summary(model_ordered_logit_explorative_interaction))

# Berechnung der p-Werte für das explorative Ordered Logit-Modell mit Interaktion
ordered_logit_summary_explorative_interaction <- summary(model_ordered_logit_explorative_interaction)
model_ordered_logit_pvalues_explorative_interaction <- pnorm(abs(ordered_logit_summary_explorative_interaction$coefficients[, "t value"]),
                                                             lower.tail = FALSE) * 2
cat("\n--- Ordered Logit p-Werte (Explorativ mit Interaktion) ---\n")
print(model_ordered_logit_pvalues_explorative_interaction)

# Berechnung von McFadden's R² für das Modell mit Interaktion
cat("\n--- McFadden's R² (Explorativ mit Interaktion) ---\n")
loglik_full_interaction <- logLik(model_ordered_logit_explorative_interaction)  # Log-Likelihood des vollständigen Modells
loglik_null_interaction <- logLik(polr(importance_factor ~ 1, data = survey_joined, Hess = TRUE))  # Nullmodell (nur Konstante)

mcfadden_r2_interaction <- 1 - (as.numeric(loglik_full_interaction) / as.numeric(loglik_null_interaction))
cat("McFadden's R²:", round(mcfadden_r2_interaction, 4), "\n")


### Funktion zur Durchführung einer Logit-Regression für ein einzelnes Szenario ohne repr_age
run_explorative_logit_for_scenario <- function(data, scenario, predictors) {
  # Filter für das spezifische Szenario
  scenario_data <- data %>%
    dplyr::select(all_of(c(scenario, predictors))) %>%
    rename(choice = !!scenario) %>%
    mutate(choice = as.numeric(choice) - 1)  # Umkodierung: 1 -> 0, 2 -> 1
  
  # Logit-Regression für das Szenario
  model <- glm(choice ~ ., data = scenario_data, family = binomial(link = "logit"))
  
  # Modellzusammenfassung ausgeben
  cat("\n--- Explorative Logit Regression for", scenario, "---\n")
  print(summary(model))
  
  # Breusch-Pagan-Test auf Heteroskedastizität
  cat("\n--- Breusch-Pagan-Test (Heteroskedastizität) ---\n")
  print(bptest(model))
  
  # Robuste Standardfehler berechnen
  cat("\n--- Robuste Standardfehler (Logit) ---\n")
  robust_se <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  print(robust_se)
  
  # Odds Ratios berechnen
  cat("\n--- Odds Ratios (Logit) ---\n")
  odds_ratios <- exp(coef(model))
  print(odds_ratios)
  
  # Konfidenzintervalle der Odds Ratios
  cat("\n--- Konfidenzintervalle der Odds Ratios (Logit) ---\n")
  conf_intervals <- exp(confint(model))
  print(conf_intervals)
  
  # Pseudo-R² berechnen
  cat("\n--- Pseudo-R² (Logit) ---\n")
  pseudo_r2 <- pscl::pR2(model)
  print(pseudo_r2)
  
  # Rückgabe des Modells (für spätere Verwendung)
  return(list(model = model, robust_se = robust_se, odds_ratios = odds_ratios,
              conf_intervals = conf_intervals, pseudo_r2 = pseudo_r2))
}

# Liste der Szenarien und Prädiktoren (ohne repr_age)
scenarios <- c("A905", "A906", "A907", "A908", "A909", "A910")
predictors_explorative <- c("assoc_pos", "awareness", "female", 
                            "innovation", "experience", "edu_group", "health_group", "children")

# Explorative Logit-Regressionen für alle Szenarien durchführen und Ergebnisse speichern
logit_results_explorative <- lapply(scenarios, function(scenario) {
  run_explorative_logit_for_scenario(survey_joined, scenario, predictors_explorative)
})



### Austausch der Variable repr_age durch employment status ###

# Variable 'employment_status' erstellen
survey_joined <- survey_joined %>%
  mutate(employment_status = case_when(
    A601 == 1 ~ "Arbeitnehmer",
    A601 == 5 ~ "Student",
    TRUE ~ NA_character_
  ))

# Filter für 'Arbeitnehmer' und 'Student' durchführen
survey_joined_employee <- survey_joined %>% filter(employment_status == "Arbeitnehmer")
survey_joined_student <- survey_joined %>% filter(employment_status == "Student")

###OLS ###

survey_joined <- survey_joined %>%
  mutate(
    importance_standardized = scale(importance, center = TRUE, scale = TRUE),
    innovation_standardized = scale(innovation, center = TRUE, scale = TRUE)
  )

model_ols_standardized <- lm(importance_standardized ~ assoc_pos + awareness + female +
                               innovation_standardized + experience + edu_group + health_group + children +
                               employment_status,
                             data = survey_joined)

cat("\n--- OLS Regression Summary (standardisierte Variablen, ohne Interaktion) ---\n")
print(summary(model_ols_standardized))

# Robuste Standardfehler berechnen (HC1)
robust_se_standardized <- vcovHC(model_ols_standardized, type = "HC1")
cat("\n--- Robuste Standardfehler (ohne Interaktion) ---\n")
print(coeftest(model_ols_standardized, vcov = robust_se_standardized))

# Konfidenzintervalle für die Koeffizienten
cat("\n--- Konfidenzintervalle (ohne Interaktion) ---\n")
print(confint(model_ols_standardized))

# Multikollinearitätsprüfung (VIF)
cat("\n--- VIF (ohne Interaktion) ---\n")
print(vif(model_ols_standardized))

# Breusch-Pagan-Test auf Heteroskedastizität
cat("\n--- Breusch-Pagan-Test (ohne Interaktion) ---\n")
print(bptest(model_ols_standardized))

cat("\n--- Adjusted R^2 (ohne Interaktion) ---\n")
print(summary(model_ols_standardized)$adj.r.squared)


# OLS Interaktion `female x experience` und emplyment status
model_ols_standardized_interaction <- lm(importance_standardized ~ assoc_pos + awareness + female +
                                           innovation_standardized + experience + edu_group + health_group +
                                           children + employment_status + female_experience_interaction,
                                         data = survey_joined)

cat("\n--- OLS Regression Summary (standardisierte Variablen, mit Interaktion) ---\n")
print(summary(model_ols_standardized_interaction))

# Robuste Standardfehler berechnen (HC1)
robust_se_standardized_interaction <- vcovHC(model_ols_standardized_interaction, type = "HC1")
cat("\n--- Robuste Standardfehler (mit Interaktion) ---\n")
print(coeftest(model_ols_standardized_interaction, vcov = robust_se_standardized_interaction))

# Konfidenzintervalle für die Koeffizienten
cat("\n--- Konfidenzintervalle (mit Interaktion) ---\n")
print(confint(model_ols_standardized_interaction))

# Multikollinearitätsprüfung (VIF)
cat("\n--- VIF (mit Interaktion) ---\n")
print(vif(model_ols_standardized_interaction))

# Breusch-Pagan-Test auf Heteroskedastizität
cat("\n--- Breusch-Pagan-Test (mit Interaktion) ---\n")
print(bptest(model_ols_standardized_interaction))

cat("\n--- Adjusted R^2 (mit Interaktion) ---\n")
print(summary(model_ols_standardized_interaction)$adj.r.squared)


### Ordered Logit Regression mit Beschäftigungsstatus, ohne reproductive age mit employment status ###
model_ordered_logit_comparison <- polr(importance_factor ~ assoc_pos + awareness + female +
                                         innovation + experience + edu_group + health_group + children +
                                         employment_status,
                                       data = survey_joined, Hess = TRUE)

cat("\n--- Ordered Logit Regression Summary (Vergleich aller Beschäftigungsstatus) ---\n")
print(summary(model_ordered_logit_comparison))


# Pseudo-R² berechnen
cat("\n--- Pseudo-R² (Ordered Logit Vergleich) ---\n")
pseudo_r2_ordered_logit <- pscl::pR2(model_ordered_logit_comparison)
print(pseudo_r2_ordered_logit)


### Ordered Logit Regression mit Interaktion und employment status, ohne reproductive age ###
model_ordered_logit_comparison_interaction <- polr(importance_factor ~ assoc_pos + awareness + female +
                                                     innovation + experience + edu_group + health_group + children +
                                                     employment_status + female_experience_interaction,
                                                   data = survey_joined, Hess = TRUE)

cat("\n--- Ordered Logit Regression Summary (Vergleich aller Beschäftigungsstatus mit Interaktion) ---\n")
print(summary(model_ordered_logit_comparison_interaction))

# Pseudo-R² berechnen
cat("\n--- Pseudo-R² (Ordered Logit Vergleich mit Interaktion) ---\n")
loglik_full_interaction <- logLik(model_ordered_logit_comparison_interaction)
loglik_null_interaction <- logLik(polr(importance_factor ~ 1, data = survey_joined, Hess = TRUE))
mcfadden_r2_interaction <- 1 - (as.numeric(loglik_full_interaction) / as.numeric(loglik_null_interaction))
cat("McFadden's R² (mit Interaktion):", round(mcfadden_r2_interaction, 4), "\n")



### Durchführung einer Logit Regression für ein einzelnes Szenario mit Vergleich der Beschäftigungsstatus und ohne reproductive age ###
# Funktion zur Durchführung einer Logit Regression für ein einzelnes Szenario mit Vergleich der Beschäftigungsstatus
run_logit_for_scenario_comparison <- function(data, scenario, predictors) {
  # Filter für das spezifische Szenario
  scenario_data <- data %>%
    dplyr::select(all_of(c(scenario, predictors))) %>%
    rename(choice = !!rlang::sym(scenario)) %>%  # Szenario-Spalte in 'choice' umbenennen
    mutate(choice = as.numeric(choice) - 1)  # Umkodierung: 1 -> 0, 2 -> 1
  
  # Logit-Regression
  model <- glm(choice ~ ., data = scenario_data, family = binomial(link = "logit"))
  
  # Modellzusammenfassung
  cat("\n--- Logit Regression für", scenario, "---\n")
  print(summary(model))
  
  # Robuste Standardfehler berechnen
  cat("\n--- Robuste Standardfehler (Logit Vergleich) ---\n")
  robust_se <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  print(robust_se)
  
  # Odds Ratios berechnen
  cat("\n--- Odds Ratios (Logit Vergleich) ---\n")
  odds_ratios <- exp(coef(model))
  print(odds_ratios)
  
  # Konfidenzintervalle der Odds Ratios
  cat("\n--- Konfidenzintervalle der Odds Ratios (Logit Vergleich) ---\n")
  conf_intervals <- exp(confint(model))
  print(conf_intervals)
  
  # Pseudo-R² berechnen
  cat("\n--- Pseudo-R² (Logit Vergleich) ---\n")
  pseudo_r2 <- pscl::pR2(model)
  print(pseudo_r2)
  
  # Rückgabe des Modells und Zusatzinfos
  return(list(model = model, robust_se = robust_se, odds_ratios = odds_ratios,
              conf_intervals = conf_intervals, pseudo_r2 = pseudo_r2))
}

# Liste der Szenarien und Prädiktoren
scenarios <- c("A905", "A906", "A907", "A908", "A909", "A910")
predictors <- c("assoc_pos", "awareness", "female", "innovation", "experience", "edu_group", "health_group", "children", "employment_status")

# Logit Regression für jedes Szenario durchführen
logit_results_comparison <- lapply(scenarios, function(scenario) {
  run_logit_for_scenario_comparison(survey_joined, scenario, predictors)
})


#### Korrelationsanalyse HealthGroup Other und Edu_Groups ####

# 1. Erstelle den Dummy für health_groupOther
library(dplyr)

survey_joined <- survey_joined %>%
  mutate(health_group_other_dummy = if_else(health_group == "Other", 1, 0))

# 2. Umkodierung von edu_group in eine numerische (ordinal) Variable
# Annahme: edu_group hat die Levels "Niedrig", "Mittel" und "Hoch"
survey_joined <- survey_joined %>% 
  mutate(edu_group_numeric = as.numeric(factor(edu_group, levels = c("Niedrig", "Mittel", "Hoch"))))

# 3. Korrelationsanalyse zwischen health_group_other_dummy und edu_group_numeric
cor_test_edu <- cor.test(survey_joined$health_group_other_dummy, 
                         survey_joined$edu_group_numeric, 
                         method = "pearson")
print("Korrelation zwischen health_group_other_dummy und edu_group_numeric:")
print(cor_test_edu)

# 4. Grafische Darstellung des Zusammenhangs
library(ggplot2)

ggplot(survey_joined, aes(x = edu_group_numeric, y = health_group_other_dummy)) +
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.7) +
  stat_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Zusammenhang zwischen Bildungsniveau und Health Group 'Other'",
       x = "Bildungsniveau (ordinal: 1 = Niedrig, 2 = Mittel, 3 = Hoch)",
       y = "Dummy: Health Group 'Other' (1 = ja, 0 = nein)") +
  theme_minimal()


#### Getestet, aber nicht in der Arbeit verwendet ###

# Funktion zur Durchführung einer Logit-Regression für ein einzelnes Szenario mit Interaktion
run_logit_for_scenario_with_interaction <- function(data, scenario, predictors) {
  # Interaktion erstellen (female * experience)
  data <- data %>%
    mutate(female_experience_interaction = female * experience)
  
  # Filter für das spezifische Szenario
  scenario_data <- data %>%
    dplyr::select(all_of(c(scenario, predictors, "female_experience_interaction"))) %>%
    rename(choice = !!scenario) %>%  # Umbenennen für Konsistenz
    mutate(choice = as.numeric(choice) - 1)  # Umkodierung: 1 -> 0, 2 -> 1
  
  # Logit-Regression für das Szenario mit Interaktion
  model <- glm(choice ~ ., data = scenario_data, family = binomial(link = "logit"))
  
  # Modellzusammenfassung ausgeben
  cat("\n--- Logit Regression for", scenario, "(mit Interaktion) ---\n")
  print(summary(model))
  
  # Breusch-Pagan-Test auf Heteroskedastizität
  cat("\n--- Breusch-Pagan-Test (Heteroskedastizität) ---\n")
  print(bptest(model))
  
  # Robuste Standardfehler berechnen
  cat("\n--- Robuste Standardfehler (Logit mit Interaktion) ---\n")
  robust_se <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  print(robust_se)
  
  # Odds Ratios berechnen
  cat("\n--- Odds Ratios (Logit mit Interaktion) ---\n")
  odds_ratios <- exp(coef(model))
  print(odds_ratios)
  
  # Konfidenzintervalle der Odds Ratios
  cat("\n--- Konfidenzintervalle der Odds Ratios (Logit mit Interaktion) ---\n")
  conf_intervals <- exp(confint(model))
  print(conf_intervals)
  
  # Pseudo-R² berechnen
  cat("\n--- Pseudo-R² (Logit mit Interaktion) ---\n")
  pseudo_r2 <- pscl::pR2(model)
  print(pseudo_r2)
  
  # Rückgabe des Modells (für spätere Verwendung)
  return(list(model = model, robust_se = robust_se, odds_ratios = odds_ratios,
              conf_intervals = conf_intervals, pseudo_r2 = pseudo_r2))
}

# Liste der Szenarien und Prädiktoren
scenarios <- c("A905", "A906", "A907", "A908", "A909", "A910")
predictors <- c("assoc_pos", "awareness", "repr_age", "female", 
                "innovation", "experience", "edu_group", "health_group", "children")

# Regressionen für alle Szenarien mit Interaktion durchführen und Ergebnisse speichern
logit_results_interaction <- lapply(scenarios, function(scenario) {
  run_logit_for_scenario_with_interaction(survey_joined, scenario, predictors)
})

# OLS-Regression ohne innovation und repr_age
model_ols_no_innovation_repr_age <- lm(importance ~ assoc_pos + awareness + female +
                                         experience + edu_group + health_group + children,
                                       data = survey_joined)

cat("\n--- OLS Regression Summary (ohne innovation und repr_age) ---\n")
print(summary(model_ols_no_innovation_repr_age))

# Robuste Standardfehler
robust_se <- vcovHC(model_ols_no_innovation_repr_age, type = "HC1")
cat("\n--- Robuste Standardfehler (OLS) ---\n")
print(coeftest(model_ols_no_innovation_repr_age, vcov = robust_se))

# Konfidenzintervalle
cat("\n--- Konfidenzintervalle (OLS) ---\n")
print(confint(model_ols_no_innovation_repr_age))

# VIF-Prüfung
cat("\n--- VIF (OLS) ---\n")
print(vif(model_ols_no_innovation_repr_age))

### Interaktion erstellen ####
survey_joined <- survey_joined %>%
  mutate(female_experience_interaction = female * experience)

# OLS-Regression ohne innovation und repr_age, aber mit Interaktion
model_ols_no_innovation_repr_age_interaction <- lm(importance ~ assoc_pos + awareness + female +
                                                     experience + edu_group + health_group + children +
                                                     female_experience_interaction,
                                                   data = survey_joined)

cat("\n--- OLS Regression Summary (ohne innovation und repr_age mit Interaktion) ---\n")
print(summary(model_ols_no_innovation_repr_age_interaction))

# Robuste Standardfehler
robust_se <- vcovHC(model_ols_no_innovation_repr_age_interaction, type = "HC1")
cat("\n--- Robuste Standardfehler (OLS mit Interaktion) ---\n")
print(coeftest(model_ols_no_innovation_repr_age_interaction, vcov = robust_se))

# Konfidenzintervalle
cat("\n--- Konfidenzintervalle (OLS mit Interaktion) ---\n")
print(confint(model_ols_no_innovation_repr_age_interaction))

# VIF-Prüfung
cat("\n--- VIF (OLS mit Interaktion) ---\n")
print(vif(model_ols_no_innovation_repr_age_interaction))


### Austausch der Variable repr_age ###

# Variable 'employment_status' erstellen
survey_joined <- survey_joined %>%
  mutate(employment_status = case_when(
    A601 == 1 ~ "Arbeitnehmer",
    A601 == 5 ~ "Student",
    TRUE ~ NA_character_
  ))

# Filter für 'Arbeitnehmer' und 'Student' durchführen
survey_joined_employee <- survey_joined %>% filter(employment_status == "Arbeitnehmer")
survey_joined_student <- survey_joined %>% filter(employment_status == "Student")
