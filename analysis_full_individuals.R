# Load survey and dplyr packages
#+ message = FALSE, warning=FALSE
library(tidyverse)
library(survey)
library(sjPlot)
#'
options(survey.lonely.psu='adjust')

# Display Version Information
cat("R package versions:\n")
for (p in c("base", "survey","dplyr")) {
  cat(p, ": ", as.character(packageVersion(p)), "\n")
}

# Lendo a base --------------------------------------------------------------------------------
df <- read_rds(file = "df.rds")
glimpse(df)

# DataPrep ------------------------------------------------------------------------------------
One <-
  df |>
  # remove duplicates
  dplyr::distinct(SEQN, .keep_all = TRUE) |>
  # adjusting physical activity - where is NA to change to zero
  dplyr::mutate(
    PAQ610 = tidyr::replace_na(PAQ610, 0),
    PAD615 = tidyr::replace_na(PAD615, 0),
    PAQ620 = tidyr::replace_na(PAQ620, 0),
    PAQ625 = tidyr::replace_na(PAQ625, 0),
    PAD630 = tidyr::replace_na(PAD630, 0),
    PAQ640 = tidyr::replace_na(PAQ640, 0),
    PAD645 = tidyr::replace_na(PAD645, 0),
    PAQ655 = tidyr::replace_na(PAQ655, 0),
    PAD660 = tidyr::replace_na(PAD660, 0),
    PAQ670 = tidyr::replace_na(PAQ670, 0),
    PAD675 = tidyr::replace_na(PAD675, 0)
  ) |>
  # created physical activity outcomes
  dplyr::mutate(
    # age_class
    AGE = case_when(RIDAGEYR >= 65 & RIDAGEYR < 80 ~ "< 80 years",
                    RIDAGEYR >= 80 ~ ">= 80 years"),
    # PA work
    PAW = PAQ610 * PAD615 + PAQ625 * PAD630,
    # PA transport
    PAT = PAQ640 * PAD645,
    # PA leisure
    PAL = PAQ655 * PAD660 + PAQ670 * PAD675,
    PATOTAL = PAW + PAT + PAL,
    PA_CLASS = case_when(PATOTAL >= 150 ~ "ATIVO",
                         PATOTAL < 150 ~ "INATIVO")
  ) |>
  # adjusting physical functioning (disability) parameters
  dplyr::mutate(WALKING_ROOMS = PFQ061H) |>
  dplyr::mutate(STANDINGUP = PFQ061I) |>
  dplyr::mutate(EATING = PFQ061K) |>
  dplyr::mutate(DRESSING = PFQ061L) |>
  # To create the variable INCAPAZ - PRIMARY OUTCOME
  mutate(WALKING_ROOMS_NOVO = case_when(WALKING_ROOMS == 1 ~ 0,
                                        WALKING_ROOMS >=2 & WALKING_ROOMS <=4 ~ 1),
         STANDINGUP_NOVO = case_when(STANDINGUP == 1 ~ 0,
                                     STANDINGUP >=2 & WALKING_ROOMS <=4 ~ 1),
         EATING_NOVO = case_when(EATING == 1 ~ 0,
                                 EATING >=2 & EATING <=4 ~ 1),
         DRESSING_NOVO = case_when(DRESSING == 1 ~ 0,
                                   DRESSING >=2 & WALKING_ROOMS <=4 ~ 1),
         INCAPAZ = WALKING_ROOMS_NOVO + STANDINGUP_NOVO + EATING_NOVO + DRESSING_NOVO,
         INCAPAZ_CLASSE = case_when(INCAPAZ < 1 ~ 0, # no disability
                                    INCAPAZ >= 1 & INCAPAZ <= 16 ~ 1),
         OBESITY = case_when(BMXBMI >= 30 ~ "OBESO",
                             BMXBMI < 30 ~ "NORMAL"),
         ENERGY = DR1TKCAL,
         PTN = DR1TPROT,
         PTNKG = PTN/BMXWT,
         CHO = DR1TCARB,
         FAT = DR1TTFAT,
         ENERGY_KG = ENERGY/BMXWT,
         ENERGY_PT_MODEL = ENERGY - PTN * 4,
         ENERGY_STATUS = case_when(RIAGENDR == 1 & ENERGY < 800 ~ "UNLIKELY",
                                   RIAGENDR == 1 & ENERGY > 4000 ~ "UNLIKELY",
                                   RIAGENDR == 1 & ENERGY >= 800 & ENERGY <=4000 ~ "LIKELY",
                                   RIAGENDR == 2 & ENERGY < 500 ~ "UNLIKELY",
                                   RIAGENDR == 2 & ENERGY > 3500 ~ "UNLIKELY",
                                   RIAGENDR == 2 & ENERGY >= 500 & ENERGY <=3500 ~ "LIKELY"),
         # create protein consumption status
         PTN_STATUS = case_when(PTNKG < 0.8 ~ "A_BAIXO",
                                PTNKG >= 0.8 & PTNKG < 1.2 ~ "B_ADEQUADO",
                                PTNKG >= 1.2  & PTNKG < 1.6 ~ "C_MODERADO",
                                PTNKG >= 1.6 ~ "D_ELEVADO"),
         # create prOtein consumptoin status RDA
         PTN_RDA = case_when(PTNKG < 0.8 ~ "A_BAIXO",
                             PTNKG >=0.8 ~ "B_ADEQUADO"),
         # Saulo/Hamilton protein consumption status
         PTN_STATUS_ROSCHEL = case_when(PTNKG < 0.8 ~ "A_BAIXO",
                                        PTNKG >= 0.8 & PTNKG < 1.2 ~ "B_ADEQUADO",
                                        PTNKG >= 1.2 ~ "D_ELEVADO"),
         # classes de ptn apenas no grupo LOW (abaixo da RDA)
         PTN_LOW_RDA = case_when(PTNKG < 0.3 ~ "A_ate0.3",
                                 PTNKG >= 0.3 & PTNKG < 0.6 ~ "B_0.3_a_0.6",
                                 PTNKG >= 0.6 ~ "D_acima0.6"),
         # classes de ptn apenas no grupo LOW (abaixo da RDA)
         PTN_MID_HIGH = case_when(PTNKG < 1.2 ~ "A_ate1.2",
                                  PTNKG >= 1.2 & PTNKG < 1.6 ~ "B_1.2_a_1.6",
                                  PTNKG >= 1.6 ~ "D_acima1.4"),
         # Peso de 8 anos
         MEC8YR = WTMEC2YR * 1/4,
         inAnalysis = (RIDAGEYR >= 65 &
                         !is.na(INCAPAZ_CLASSE) &
                         !is.na(OBESITY) &
                         !is.na(PTN_RDA) &
                         ENERGY_STATUS == "LIKELY") &
           DIQ010 < 3 & # Diabetes (1 = yes; 2 = no)
           BPQ020 < 3 & # HAS (1 = yes; 2 = no)
           MCQ160B < 3 & # ICC (1 = yes; 2 = no)
           MCQ160E < 3 # heart attack (1 = yes; 2 = no)
  )

#' ## Define survey design
# Define survey design for overall dataset
NHANES_all <- svydesign(data=One, id=~SDMVPSU, strata=~SDMVSTRA, weights=~MEC8YR, nest=TRUE)

# Create a survey design object for the subset of interest: adults aged 20 and over with a valid depression score
# Subsetting the original survey design object ensures we keep the design information about the number of clusters and strata
NHANES <- subset(NHANES_all, inAnalysis)

# TESTING MODELS
# To avoid scientific notation
options(scipen=999)

# Models to test "RDA vs Non-RDA" -------------------------------------------------------------

## crude logistic regression -  Consumo de PTN (RDA - >= 0.8 ptn = adequado)
# rda_crude <- glm(as.factor(functioning) ~ ptn_rda, family = binomial, data = df_ajust)
rda_crude_svy <-
  survey::svyglm(
    formula = as.factor(INCAPAZ_CLASSE) ~ as.factor(PTN_RDA),
    design = NHANES,
    family = binomial(link = "logit")
  )

# Summary
summary(rda_crude_svy)
cbind(odds = exp(rda_crude_svy$coefficients), exp(confint(rda_crude_svy)))
sjPlot::tab_model(rda_crude_svy)

## Adjusted logistic regression -  Consumo de PTN (RDA - >= 0.8 ptn = adequado)
# rda_adjusted <- glm(as.factor(functioning) ~ ptn_rda + idade + sexo + as_factor(raca) + pad + energy_kg_model, family = binomial, data = df_ajust)
rda_adjusted_svy <-
  survey::svyglm(
    formula = as.factor(INCAPAZ_CLASSE) ~ as.factor(PTN_RDA) + RIAGENDR + AGE + RIDRETH1 + PA_CLASS + ENERGY_PT_MODEL + DIQ010 + BPQ020 + MCQ160B + MCQ160E,
    design = NHANES,
    family = binomial(link = "logit")
  )

# Summary
summary(rda_adjusted_svy)
cbind(odds = exp(rda_adjusted_svy$coefficients), exp(confint(rda_adjusted_svy)))
sjPlot::tab_model(rda_adjusted_svy)

# Models to test "Low, adequate and high protein" (Roschel and Saulo)----------------- --------
## crude logistic regression -  Consumo de PTN (ROSCHEL/SAULO)
rs_crude_svy <-
  survey::svyglm(
    formula = as.factor(INCAPAZ_CLASSE) ~ PTN_STATUS_ROSCHEL,
    design = NHANES,
    family = binomial(link = "logit")
  )

# Summary
summary(rs_crude_svy)
cbind(odds = exp(rs_crude_svy$coefficients), exp(confint(rs_crude_svy)))
sjPlot::tab_model(rs_crude_svy)

## Adjusted logistic regression -  Consumo de PTN (ROSCHEL/SAULO)
rs_adjusted_svy <-
  survey::svyglm(
    formula = as.factor(INCAPAZ_CLASSE) ~ as.factor(PTN_STATUS_ROSCHEL) + RIAGENDR + AGE + RIDRETH1 + PA_CLASS + ENERGY_PT_MODEL + DIQ010 + BPQ020 + MCQ160B + MCQ160E,
    design = NHANES,
    family = binomial(link = "logit")
  )

# Summary
summary(rs_adjusted_svy)
cbind(odds = exp(rs_adjusted_svy$coefficients), exp(confint(rs_adjusted_svy)))
sjPlot::tab_model(rs_adjusted_svy)
