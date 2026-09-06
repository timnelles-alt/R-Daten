# ==============================================================================
# MASTER-SKRIPT: SUBSTITUTIONSÄNGSTE DURCH KI AM ARBEITSPLATZ
# ==============================================================================

# 1. PAKETE LADEN
library(tidyverse)
library(psych)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(car)
library(ggcorrplot)
library(lmtest)

# 2. DATEN IMPORTIEREN & PIPELINE-BEREINIGUNG
ds <- read.csv("data.csv", 
               sep = "\t", 
               quote = "\"", 
               dec = ",", 
               fileEncoding = "UTF-16LE", 
               header = TRUE)

ds_clean <- ds %>% 
  filter(STATUS == "complete", FR01 %in% c(1, 2))

# Fehlende Werte (-9) global durch echte NAs ersetzen
ds_clean[ds_clean == -9] <- NA

ds_clean <- ds_clean %>%
  rename(
    Erwerbsstatus       = FR01,
    Geschlecht          = FR23,
    Alter               = FR24_01,
    Berufserfahrung     = FR25_01,
    Bildungsabschluss   = FR26,
    Branche             = FR27,
    Unternehmensgroesse = FR28,
    Regionstyp          = FR29,
    Einkommen           = FR30
  ) %>%
  var_labels(
    FR34_01 = "Aufgaben: Fest vorgegebene Abläufe",
    FR34_02 = "Aufgaben: Repetitive Tätigkeiten",
    FR34_03 = "Aufgaben: Wenig Spielraum für Spontaneität",
    FR35_01 = "KI-Nutzung: Häufigkeit im beruflichen Alltag",
    FR35_02 = "KI-Nutzung: Bandbreite verschiedener KI-Tools",
    FR35_03 = "KI-Nutzung: Freiwilligkeit der Nutzung",
    FR36_01 = "KI-Einstellung: Vertrauen in korrekte Ergebnisse",
    FR36_02 = "KI-Einstellung: Fairness und Unvoreingenommenheit",
    FR36_03 = "KI-Einstellung: Stabilität und Verlässlichkeit",
    FR36_04 = "KI-Einstellung: Sichere Unterstützung im Arbeitsalltag",
    FR37_01 = "Angst: Sorge vor Arbeitsplatzverlust",
    FR37_02 = "Angst: Sorge um Ersetzung durch KI",
    FR37_03 = "Angst: KI als Bedrohung wahrgenommen",
    FR37_04 = "Angst: Negative Beeinflussung der beruflichen Zukunft",
    FR37_05 = "Angst: Sorge um langfristige Relevanz des Berufs"
  )

# Alters-Ausreißer bereinigen
ds_clean$Alter[ds_clean$Alter > 70] <- NA

# 3. FAKTOREN ALS ECHTE KATEGORIALE VARIABLEN DEFINIEREN
ds_clean$Erwerbsstatus_Name <- factor(ds_clean$Erwerbsstatus, levels = 1:2, 
                                      labels = c("Angestellte", "Beamte"))

ds_clean$Geschlecht_Name <- factor(ds_clean$Geschlecht, levels = 1:3, 
                                   labels = c("Männlich", "Weiblich", "Divers"))

ds_clean$Bildung_Name <- factor(ds_clean$Bildungsabschluss, levels = 1:6, 
                                labels = c("Haupt-/Realschule", "Abitur/Fachabitur", "Berufsausbildung", "Bachelor", "Master/Diplom", "Promotion"))

ds_clean$Branche_Name <- factor(ds_clean$Branche, levels = 1:6, 
                                labels = c("Dienstleistung/Handel", "IT/Medien", "Industrie/Handwerk", "Gesundheit/Soziales", "Öffentl. Verwaltung", "Bildung/Wissenschaft"))

ds_clean$Unternehmensgroesse_Name <- factor(ds_clean$Unternehmensgroesse, levels = 1:4, 
                                            labels = c("Klein (<50)", "Mittel (50-499)", "Groß (500-1999)", "Konzern (>=2000)"))

ds_clean$Regionstyp_Name <- factor(ds_clean$Regionstyp, levels = 1:2, 
                                   labels = c("Urban", "Ländlich"))

ds_clean$Einkommen_Name <- factor(ds_clean$Einkommen, levels = 1:6, 
                                  labels = c("<1.500€", "1.500€-2.500€", "2.500€-4.000€", "4.000€-6.000€", ">=6.000€", "Keine Angabe"))

# 4. SKALENINDIZES BERECHNEN (Mittelwertindizes)
ds_clean <- ds_clean %>%
  mutate(
    Routinegrad = rowMeans(across(c(FR34_01, FR34_02, FR34_03)), na.rm = TRUE),
    KI_Nutzungshaeufigkeit = FR35_01, 
    Systemvertrauen = rowMeans(across(c(FR36_01, FR36_02, FR36_03, FR36_04)), na.rm = TRUE),
    Substitutionsangst = rowMeans(across(c(FR37_01, FR37_02, FR37_03, FR37_04, FR37_05)), na.rm = TRUE)
  )

mean_alter <- mean(ds_clean$Alter, na.rm = TRUE)

# ==============================================================================
# 5. DESKRIPTIVE STATISTIKEN & AUTOMATISCHER WORD-EXPORT
# ==============================================================================
mean_alter
sd(ds_clean$Alter, na.rm = TRUE)
frq(ds_clean$Geschlecht_Name)
frq(ds_clean$Bildung_Name)
frq(ds_clean$Branche_Name)
frq(ds_clean$Unternehmensgroesse_Name)
frq(ds_clean$Einkommen_Name)

# Auswahl der kategorialen Variablen für den Export (JETZT VOLLSTÄNDIG)
stichproben_merkmale <- ds_clean %>% 
  select(Erwerbsstatus_Name, Geschlecht_Name, Bildung_Name, Branche_Name, Unternehmensgroesse_Name, Einkommen_Name)

# Automatische Erstellung der soziodemografischen Gesamttabelle für Word
view_df(stichproben_merkmale, 
        show.frq = TRUE, 
        show.prc = TRUE, 
        file = "Deskriptive_Gesamttabelle_Bachelorarbeit.doc")

# ==============================================================================
# 6. ABBILDUNGEN FÜR DIE DESKRIPTIVE STATISTIK
# ==============================================================================

# ABBILDUNG 1: ALTERSHISTOGRAMM
ggplot(ds_clean, aes(x = Alter)) +
  geom_histogram(binwidth = 5, boundary = 20, fill = "#2C3E50", color = "white", alpha = 0.9, na.rm = TRUE) +
  geom_vline(aes(xintercept = mean_alter), color = "#E74C3C", linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = mean_alter + 0.8, y = Inf, label = paste0("M = ", round(mean_alter, 1), " Jahre"), color = "#E74C3C", fontface = "italic", vjust = 2, hjust = 0) +
  scale_x_continuous(breaks = seq(20, 70, by = 5)) +
  theme_classic(base_size = 13) +
  labs(x = "Alter in Jahren", y = "Anzahl der Befragten") +
  theme(axis.line = element_line(linewidth = 0.6, color = "black"), axis.title = element_text(face = "bold"), panel.grid.major.y = element_line(color = "gray95"))

# ABBILDUNG 2: BALKENDIAGRAMM UNTERNEHMENSGRÖSSE
plot_data_groesse <- ds_clean %>% filter(!is.na(Unternehmensgroesse_Name))
ggplot(plot_data_groesse, aes(y = fct_rev(Unternehmensgroesse_Name))) + 
  geom_bar(fill = "#2C3E50", color = "white", alpha = 0.9, width = 0.6) +
  stat_count(geom = "text", aes(label = after_stat(count)), hjust = -0.4, size = 4, fontface = "bold", color = "#2C3E50") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_classic(base_size = 13) + 
  labs(x = "Anzahl der Befragten (n)", y = "Unternehmensgröße") +
  coord_cartesian(clip = "off") + 
  theme(axis.line = element_line(linewidth = 0.6, color = "black"), axis.title = element_text(face = "bold"), axis.text.y = element_text(color = "black"), panel.grid.major.x = element_line(color = "gray95"))

# ==============================================================================
# 7. RELIABILITÄTSANALYSEN (CRONBACHS ALPHA)
# ==============================================================================
psych::alpha(ds_clean %>% select(FR34_01, FR34_02, FR34_03))
psych::alpha(ds_clean %>% select(FR35_01, FR35_02, FR35_03))
psych::alpha(ds_clean %>% select(FR36_01, FR36_02, FR36_03, FR36_04))
psych::alpha(ds_clean %>% select(FR37_01, FR37_02, FR37_03, FR37_04, FR37_05))

# ==============================================================================
# 8. BIVARIATE KORRELATIONEN & KORRELOGRAMM (ABBILDUNG 3)
# ==============================================================================
corr_daten <- ds_clean %>% select(Substitutionsangst, Routinegrad, KI_Nutzungshaeufigkeit, Systemvertrauen)
matrix_ergebnis <- corr.test(corr_daten, use = "pairwise", method = "pearson")

saubere_namen <- c("Substitutionsangst", "Routinegrad", "KI-Nutzungshäufigkeit", "Systemvertrauen")
rownames(matrix_ergebnis$r) <- saubere_namen; colnames(matrix_ergebnis$r) <- saubere_namen
rownames(matrix_ergebnis$p) <- saubere_namen; colnames(matrix_ergebnis$p) <- saubere_namen

ggcorrplot(matrix_ergebnis$r, p.mat = matrix_ergebnis$p, sig.level = 0.05, insig = "blank", type = "lower", show.diag = FALSE, lab = TRUE, lab_size = 4.5, colors = c("#2C3E50", "white", "#E74C3C"), legend.title = "Pearson r", title = "", ggtheme = theme_minimal(base_size = 13)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black"), axis.text.y = element_text(face = "bold", color = "black"), panel.grid.major = element_blank())

# ==============================================================================
# 9. HIERARCHISCHE REGRESSIONSANALYSEN & GEPRÜFTE REGRESSIONSDIAGNOSTIK
# ==============================================================================
modell_1 <- lm(Substitutionsangst ~ Alter + Geschlecht_Name + Einkommen_Name + Unternehmensgroesse_Name, data = ds_clean)
summary(modell_1)

modell_2_final <- lm(Substitutionsangst ~ Alter + Geschlecht_Name + Einkommen_Name + Unternehmensgroesse_Name + Bildung_Name + Branche_Name + Routinegrad + KI_Nutzungshaeufigkeit + Systemvertrauen, data = ds_clean)
summary(modell_2_final)

# Voraussetzungen der Regression prüfen
vif(modell_2_final)
bptest(modell_2_final)
dwtest(modell_2_final)
shapiro.test(residuals(modell_2_final))
plot(modell_2_final)

# ERSTELLUNG DER SOWI-TABELLE DIREKT FÜR WORD
tab_model(modell_1, modell_2_final, 
          show.ci = FALSE, 
          show.se = TRUE, 
          p.style = "stars", 
          file = "Regressions_Tabelle_Bachelorarbeit.doc")

# ABBILDUNG 4: AUFGERÄUMTER KOEFFIZIENTENPLOT (Fokus rein auf die Haupthypothesen)
plot_model(modell_2_final, 
           type = "est", 
           sort.est = TRUE, 
           show.values = TRUE, 
           value.offset = 0.4, 
           colors = "#2C3E50", 
           vline.color = "black", 
           title = "", 
           dot.size = 2.5, 
           line.size = 0.8,
           terms = c("Routinegrad", "KI_Nutzungshaeufigkeit", "Systemvertrauen")) + 
  theme_classic(base_size = 13) + 
  labs(x = "", y = "Regressionskoeffizienten (B) mit 95%-Konfidenzintervallen") + 
  theme(axis.text.y = element_text(face = "bold", color = "black"), 
        axis.text.x = element_text(color = "black"), 
        axis.title.x = element_text(face = "bold", margin = margin(t = 10)), 
        axis.line.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        panel.grid.major.x = element_line(color = "gray95", linetype = "dashed"))

# ==============================================================================
# 10. STATISTISCHER GRUPPENVERGLEICH (WELCH-T-TEST)
# ==============================================================================
leveneTest(Substitutionsangst ~ Erwerbsstatus_Name, data = ds_clean)
t.test(Substitutionsangst ~ Erwerbsstatus_Name, data = ds_clean, var.equal = FALSE)

# Deskriptive Kennzahlen für das Textkapitel
ds_clean %>% 
  group_by(Erwerbsstatus_Name) %>% 
  summarise(Anzahl = n(), 
            Mittelwert = mean(Substitutionsangst, na.rm = TRUE), 
            Standardabweichung = sd(Substitutionsangst, na.rm = TRUE))

# ABBILDUNG 5: BOXPLOT (STATUS-VERGLEICH)
ggplot(ds_clean, aes(x = Erwerbsstatus_Name, y = Substitutionsangst, color = Erwerbsstatus_Name, fill = Erwerbsstatus_Name)) + 
  geom_boxplot(alpha = 0.2, outlier.shape = NA, width = 0.5, color = "black", linewidth = 0.6) + 
  geom_jitter(width = 0.15, alpha = 0.5, size = 1.5) +        
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3.5, fill = "white", color = "black", stroke = 1) +
  scale_color_manual(values = c("#2C3E50", "#E74C3C")) + 
  scale_fill_manual(values = c("#2C3E50", "#E74C3C")) +
  theme_classic(base_size = 13) + 
  labs(x = "Erwerbsstatus", y = "Substitutionsangst (Skalenmittelwert)") + 
  theme(legend.position = "none", axis.text = element_text(color = "black", face = "bold"), axis.title = element_text(face = "bold"))

