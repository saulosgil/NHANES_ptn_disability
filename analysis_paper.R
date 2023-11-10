#' Example R code to replicate NCHS Data Brief No.303, Figures 1
#' Prevalence of Depression Among Adults Aged 20 and Over: United States, 2013-2016

#' Brody DJ, Pratt LA, Hughes JP. Prevalence of Depression Among Adults Aged 20 and Over: United
#' States, 2013-2016. NCHS Data Brief. No 303. Hyattsville, MD: National Center for Health Statistics. 2018.

#' Available at: https://www.cdc.gov/nchs/products/databriefs/db303.htm

#' ------------------------------------------------------------------------------------------------------------

# Load survey and dplyr packages
#+ message = FALSE, warning=FALSE
library(tidyverse)
library(survey)
library(GGally)
library(sjPlot)
#'
options(survey.lonely.psu='adjust')

# Display Version Information
cat("R package versions:\n")
for (p in c("base", "survey","dplyr")) {
  cat(p, ": ", as.character(packageVersion(p)), "\n")
}

#' # Data preparation
# Download & Read Transport Files
# Demographic (DEMO)

# Demographic ---------------------------------------------------------------------------------
# 99-00
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DEMO.XPT", tf <- tempfile(), mode="wb")
DEMO_99 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "SDMVSTRA",
                                       "SDMVPSU",
                                       "RIDRETH1",
                                       "WTMEC4YR")]

# 01-02
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/DEMO_B.XPT", tf <- tempfile(), mode="wb")
DEMO_01 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "SDMVSTRA",
                                       "SDMVPSU",
                                       "RIDRETH1",
                                       "WTMEC4YR")]

# 03-04
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DEMO_C.XPT", tf <- tempfile(), mode="wb")
DEMO_03 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "SDMVSTRA",
                                       "SDMVPSU",
                                       "RIDRETH1",
                                       "WTMEC2YR")]

# 05-06
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.XPT", tf <- tempfile(), mode="wb")
DEMO_05 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "SDMVSTRA",
                                       "SDMVPSU",
                                       "RIDRETH1",
                                       "WTMEC2YR")]

# Dietary interview ---------------------------------------------------------------------------
# 99-00
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DRXTOT.XPT", tf <- tempfile(), mode="wb")
DIET_99 <- foreign::read.xport(tf)[, c("SEQN",
                                       "DRXTKCAL",
                                       "DRXTPROT",
                                       "DRXTCARB",
                                       "DRXTTFAT",
                                       "DRXTCALC")]

# 01-02
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/DRXTOT_B.XPT", tf <- tempfile(), mode="wb")
DIET_01 <- foreign::read.xport(tf)[, c("SEQN",
                                       "DRXTKCAL",
                                       "DRXTPROT",
                                       "DRXTCARB",
                                       "DRXTTFAT",
                                       "DRXTCALC")]

# 03-04
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DR1TOT_C.XPT", tf <- tempfile(), mode="wb")
DIET_03 <- foreign::read.xport(tf)[, c("SEQN",
                                       "DR1TKCAL",
                                       "DR1TPROT",
                                       "DR1TCARB",
                                       "DR1TTFAT",
                                       "DR1TCALC")]

# 05-06
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DR1TOT_D.XPT", tf <- tempfile(), mode="wb")
DIET_05 <- foreign::read.xport(tf)[, c("SEQN",
                                       "DR1TKCAL",
                                       "DR1TPROT",
                                       "DR1TCARB",
                                       "DR1TTFAT",
                                       "DR1TCALC")]

# Body measures -------------------------------------------------------------------------------
# 99-00
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/BMX.XPT", tf <- tempfile(), mode="wb")
BODY_99 <- foreign::read.xport(tf)[, c("SEQN",
                                       "BMXWT",
                                       "BMXHT",
                                       "BMXBMI")]

# 01-02
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/BMX_B.XPT", tf <- tempfile(), mode="wb")
BODY_01 <- foreign::read.xport(tf)[, c("SEQN",
                                       "BMXWT",
                                       "BMXHT",
                                       "BMXBMI")]

# 03-04
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/BMX_C.XPT", tf <- tempfile(), mode="wb")
BODY_03 <- foreign::read.xport(tf)[, c("SEQN",
                                       "BMXWT",
                                       "BMXHT",
                                       "BMXBMI")]

# 05-06
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/BMX_D.XPT", tf <- tempfile(), mode="wb")
BODY_05 <- foreign::read.xport(tf)[, c("SEQN",
                                       "BMXWT",
                                       "BMXHT",
                                       "BMXBMI")]

# QUESTIONARY ---------------------------------------------------------------------------------
# physical functioning ------------------------------------------------------------------------

download.file("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/PFQ.XPT", tf <- tempfile(), mode="wb")
Q_99 <- foreign::read.xport(tf)[, c("SEQN",
                                    "PFQ060A",
                                    "PFQ060B",
                                    "PFQ060C",
                                    "PFQ060D",
                                    "PFQ060E",
                                    "PFQ060F",
                                    "PFQ060G",
                                    "PFQ060H",
                                    "PFQ060I",
                                    "PFQ060J",
                                    "PFQ060K",
                                    "PFQ060L")]


# 01-02
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/PFQ_B.XPT", tf <- tempfile(), mode="wb")
Q_01 <- foreign::read.xport(tf)[, c("SEQN",
                                    "PFQ060A",
                                    "PFQ060B",
                                    "PFQ060C",
                                    "PFQ060D",
                                    "PFQ060E",
                                    "PFQ060F",
                                    "PFQ060G",
                                    "PFQ060H",
                                    "PFQ060I",
                                    "PFQ060J",
                                    "PFQ060K",
                                    "PFQ060L")]

# 03-04
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/PFQ_C.XPT", tf <- tempfile(), mode="wb")
Q_03 <- foreign::read.xport(tf)[, c("SEQN",
                                    "PFQ061A",
                                    "PFQ061B",
                                    "PFQ061C",
                                    "PFQ061D",
                                    "PFQ061E",
                                    "PFQ061F",
                                    "PFQ061G",
                                    "PFQ061H",
                                    "PFQ061I",
                                    "PFQ061J",
                                    "PFQ061K",
                                    "PFQ061L")]

# 05-06
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/PFQ_D.XPT", tf <- tempfile(), mode="wb")
Q_05 <- foreign::read.xport(tf)[, c("SEQN",
                                    "PFQ061A",
                                    "PFQ061B",
                                    "PFQ061C",
                                    "PFQ061D",
                                    "PFQ061E",
                                    "PFQ061F",
                                    "PFQ061G",
                                    "PFQ061H",
                                    "PFQ061I",
                                    "PFQ061J",
                                    "PFQ061K",
                                    "PFQ061L")]

# atividade física ------------------------------------------------------------------------

download.file("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/PAQIAF.XPT", tf <- tempfile(), mode="wb")
ATV_99 <- foreign::read.xport(tf)[, c("SEQN",
                                      "PADACTIV",
                                      "PADLEVEL",
                                      "PADTIMES",
                                      "PADDURAT",
                                      "PADMETS")]


# 01-02
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/PAQIAF_B.XPT", tf <- tempfile(), mode="wb")
ATV_01 <- foreign::read.xport(tf)[, c("SEQN",
                                      "PADACTIV",
                                      "PADLEVEL",
                                      "PADTIMES",
                                      "PADDURAT",
                                      "PADMETS")]

# 03-04
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/PAQIAF_C.XPT", tf <- tempfile(), mode="wb")
ATV_03 <- foreign::read.xport(tf)[, c("SEQN",
                                      "PADACTIV",
                                      "PADLEVEL",
                                      "PADTIMES",
                                      "PADDURAT",
                                      "PADMETS")]

# 05-06
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/PAQIAF_D.XPT", tf <- tempfile(), mode="wb")
ATV_05 <- foreign::read.xport(tf)[, c("SEQN",
                                      "PADACTIV",
                                      "PADLEVEL",
                                      "PADTIMES",
                                      "PADDURAT",
                                      "PADMETS")]

# Diabetes ------------------------------------------------------------------------

download.file("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DIQ.XPT", tf <- tempfile(), mode="wb")
DM_99 <- foreign::read.xport(tf)[, c("SEQN",
                                     "DIQ010")]


# 01-02
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/DIQ_B.XPT", tf <- tempfile(), mode="wb")
DM_01 <- foreign::read.xport(tf)[, c("SEQN",
                                     "DIQ010")]

# 03-04
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DIQ_C.XPT", tf <- tempfile(), mode="wb")
DM_03 <- foreign::read.xport(tf)[, c("SEQN",
                                     "DIQ010")]

# 05-06
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DIQ_D.XPT", tf <- tempfile(), mode="wb")
DM_05 <- foreign::read.xport(tf)[, c("SEQN",
                                     "DIQ010")]

# HAS -----------------------------------------------------------------------------------------
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/BPQ.XPT", tf <- tempfile(), mode="wb")
HAS_99 <- foreign::read.xport(tf)[, c("SEQN",
                                      "BPQ020")]


# 01-02
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/BPQ_B.XPT", tf <- tempfile(), mode="wb")
HAS_01 <- foreign::read.xport(tf)[, c("SEQN",
                                      "BPQ020")]

# 03-04
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/BPQ_C.XPT", tf <- tempfile(), mode="wb")
HAS_03 <- foreign::read.xport(tf)[, c("SEQN",
                                      "BPQ020")]

# 05-06
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/BPQ_D.XPT", tf <- tempfile(), mode="wb")
HAS_05 <- foreign::read.xport(tf)[, c("SEQN",
                                      "BPQ020")]

# CVD_CANCER -----------------------------------------------------------------------------------------
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/MCQ.XPT", tf <- tempfile(), mode="wb")
CVD_CANCER_99 <- foreign::read.xport(tf)[, c("SEQN",
                                             "MCQ160B",
                                             "MCQ160C",
                                             "MCQ160E",
                                             "MCQ220")]


# 01-02
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/MCQ_B.XPT", tf <- tempfile(), mode="wb")
CVD_CANCER_01 <- foreign::read.xport(tf)[, c("SEQN",
                                             "MCQ160B",
                                             "MCQ160C",
                                             "MCQ160E",
                                             "MCQ220")]

# 03-04
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/MCQ_C.XPT", tf <- tempfile(), mode="wb")
CVD_CANCER_03 <- foreign::read.xport(tf)[, c("SEQN",
                                             "MCQ160B",
                                             "MCQ160C",
                                             "MCQ160E",
                                             "MCQ220")]

# 05-06
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/MCQ_D.XPT", tf <- tempfile(), mode="wb")
CVD_CANCER_05 <- foreign::read.xport(tf)[, c("SEQN",
                                             "MCQ160B",
                                             "MCQ160C",
                                             "MCQ160E",
                                             "MCQ220")]

# LABORATORY ---------------------------------------------------------------------------------
# Append Files
DEMO <- dplyr::bind_rows(DEMO_99,
                         DEMO_01,
                         DEMO_03,
                         DEMO_05)

DEMO |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"


DIET <-  dplyr::bind_rows(DIET_99,
                          DIET_01,
                          DIET_03,
                          DIET_05)

DIET |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

BODY <-  dplyr::bind_rows(BODY_99,
                          BODY_01,
                          BODY_03,
                          BODY_05)

BODY |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

QUEST <- dplyr::bind_rows(Q_99,
                          Q_01,
                          Q_03,
                          Q_05)

QUEST |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

ATV <- dplyr::bind_rows(ATV_99,
                        ATV_01,
                        ATV_03,
                        ATV_05)

ATV |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

DM <- dplyr::bind_rows(DM_99,
                       DM_01,
                       DM_03,
                       DM_05)

DM |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

HAS <- dplyr::bind_rows(HAS_99,
                        HAS_01,
                        HAS_03,
                        HAS_05)

HAS |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

CVD_CANCER <- dplyr::bind_rows(CVD_CANCER_99,
                               CVD_CANCER_01,
                               CVD_CANCER_03,
                               CVD_CANCER_05)

CVD_CANCER |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

# Merge DEMO and DIET files

DEMO_DIET <-
  dplyr::left_join(DEMO, DIET, by="SEQN")

# Merge DEMO_DIET and BODY

DEMO_DIET_BODY <-
  dplyr::left_join(DEMO_DIET, BODY, by="SEQN")

# Merge DEMO_DIET_BODY and QUEST

DEMO_DIET_BODY_QUEST <-
  dplyr::left_join(DEMO_DIET_BODY, QUEST, by="SEQN")

# Merge DEMO_DIET_BODY_QUEST and ATV

DEMO_DIET_BODY_QUEST_ATV <-
  dplyr::left_join(DEMO_DIET_BODY_QUEST, ATV, by="SEQN")

# Merge DEMO_DIET_BODY_QUEST_ATV and DM

DEMO_DIET_BODY_QUEST_ATV_DM <-
  dplyr::left_join(DEMO_DIET_BODY_QUEST_ATV, DM, by="SEQN")

# Merge DEMO_DIET_BODY_QUEST_ATV_DM and HAS

DEMO_DIET_BODY_QUEST_ATV_DM_HAS <-
  dplyr::left_join(DEMO_DIET_BODY_QUEST_ATV_DM, HAS, by="SEQN")

# Merge DEMO_DIET_BODY_QUEST_ATV_DM_HAS and CVD_CANCER

DEMO_DIET_BODY_QUEST_ATV_DM_HAS_CVD_CANCER <-
  dplyr::left_join(DEMO_DIET_BODY_QUEST_ATV_DM_HAS, CVD_CANCER, by="SEQN")


df_bruto <- DEMO_DIET_BODY_QUEST_ATV_DM_HAS_CVD_CANCER

df <- df_bruto |>
  dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "YES" NEED TREATMENT!

# Created new variables -----------------------------------------------------------------------

###### salvando data.frame para explorar

readr::write_csv2(x = df, file = "df.csv")

# Lendo a base --------------------------------------------------------------------------------

df <- read.csv2(file = "df.csv")

# DataPrep ------------------------------------------------------------------------------------

One_1 <-
  df |>
  # input 0 in NA due to different variable names
  dplyr::mutate(DRXTPROT = tidyr::replace_na(DRXTPROT, 0),
                DR1TPROT = tidyr::replace_na(DR1TPROT, 0),
                DR1TCARB = tidyr::replace_na(DR1TCARB, 0),
                DRXTCARB = tidyr::replace_na(DRXTCARB, 0),
                DRXTKCAL = tidyr::replace_na(DRXTKCAL, 0),
                DR1TKCAL = tidyr::replace_na(DR1TKCAL, 0),
                DRXTTFAT = tidyr::replace_na(DRXTTFAT, 0),
                DR1TTFAT = tidyr::replace_na(DR1TTFAT, 0),
                DRXTCALC = tidyr::replace_na(DRXTCALC, 0),
                DR1TCALC = tidyr::replace_na(DR1TCALC, 0),
                PFQ060A = tidyr::replace_na(PFQ060A, 0),
                PFQ061A = tidyr::replace_na(PFQ061A, 0),
                PFQ060B = tidyr::replace_na(PFQ060B, 0),
                PFQ061B = tidyr::replace_na(PFQ061B, 0),
                PFQ060C = tidyr::replace_na(PFQ060C, 0),
                PFQ061C = tidyr::replace_na(PFQ061C, 0),
                PFQ060D = tidyr::replace_na(PFQ060D, 0),
                PFQ061D = tidyr::replace_na(PFQ061D, 0),
                PFQ060E = tidyr::replace_na(PFQ060E, 0),
                PFQ061E = tidyr::replace_na(PFQ061E, 0),
                PFQ060F = tidyr::replace_na(PFQ060F, 0),
                PFQ061F = tidyr::replace_na(PFQ061F, 0),
                PFQ060G = tidyr::replace_na(PFQ060G, 0),
                PFQ061G = tidyr::replace_na(PFQ061G, 0),
                PFQ060H = tidyr::replace_na(PFQ060H, 0),
                PFQ061H = tidyr::replace_na(PFQ061H, 0),
                PFQ060I = tidyr::replace_na(PFQ060I, 0),
                PFQ061I = tidyr::replace_na(PFQ061I, 0),
                PFQ060J = tidyr::replace_na(PFQ060J, 0),
                PFQ061J = tidyr::replace_na(PFQ061J, 0),
                PFQ060K = tidyr::replace_na(PFQ060K, 0),
                PFQ061K = tidyr::replace_na(PFQ061K, 0),
                PFQ060L = tidyr::replace_na(PFQ060L, 0),
                PFQ061L = tidyr::replace_na(PFQ061L, 0),
                PADACTIV = tidyr::replace_na(PADACTIV, 0),
                PADLEVEL = tidyr::replace_na(PADLEVEL, 0),
                PADTIMES = tidyr::replace_na(PADTIMES, 0),
                PADDURAT = tidyr::replace_na(PADDURAT, 0),
                PADMETS = tidyr::replace_na(PADMETS, 0),
                DIQ010 = tidyr::replace_na(DIQ010, 0),
                BPQ020 = tidyr::replace_na(BPQ020, 0),
                MCQ160B = tidyr::replace_na(MCQ160B, 0),
                MCQ160C = tidyr::replace_na(MCQ160C, 0),
                MCQ160E = tidyr::replace_na(MCQ160E, 0),
                MCQ220 = tidyr::replace_na(MCQ220, 0)
  )

# adjusting physical function parameters

One_2 <-
  One_1 |>
  dplyr::mutate(MONEY_FUNCTION = PFQ060A + PFQ061A) |>
  dplyr::mutate(WALKING_MILE = PFQ060B + PFQ061B) |>
  dplyr::mutate(WALKING_STEPS = PFQ060C + PFQ061C) |>
  dplyr::mutate(STOOPING = PFQ060D + PFQ061D) |>
  dplyr::mutate(LIFTING = PFQ060E + PFQ061E) |>
  dplyr::mutate(HOUSE_CHORE = PFQ060F + PFQ061F) |>
  dplyr::mutate(PREP_MEALS = PFQ060G + PFQ061G) |>
  dplyr::mutate(WALKING_ROOMS = PFQ060H + PFQ061H) |>
  dplyr::mutate(STANDINGUP = PFQ060I + PFQ061I) |>
  dplyr::mutate(BED_DIFFICULT = PFQ060J + PFQ061J) |>
  dplyr::mutate(EATING = PFQ060K + PFQ061K) |>
  dplyr::mutate(DRESSING = PFQ060L + PFQ061L)

# To create the variable INCAPAZ - PRIMARY OUTCOME
One_3 <-
  One_2 |>
  mutate(WALKING_ROOMS_NOVO = case_when(WALKING_ROOMS == 0 ~ 1000,
                                        WALKING_ROOMS == 1 ~ 0,
                                        WALKING_ROOMS >=2 & WALKING_ROOMS <=4 ~ 1,
                                        WALKING_ROOMS >= 5 & WALKING_ROOMS < 10 ~ 100),
         STANDINGUP_NOVO = case_when(STANDINGUP == 0 ~ 1000,
                                     STANDINGUP == 1 ~ 0,
                                     STANDINGUP >=2 & WALKING_ROOMS <=4 ~ 1,
                                     STANDINGUP >= 5 & STANDINGUP < 10 ~ 100),
         EATING_NOVO = case_when(EATING == 0 ~ 1000,
                                 EATING == 1 ~ 0,
                                 EATING >=2 & EATING <=4 ~ 1,
                                 EATING >= 5 & EATING < 10 ~ 100),
         DRESSING_NOVO = case_when(DRESSING == 0 ~ 1000,
                                   DRESSING == 1 ~ 0,
                                   DRESSING >=2 & WALKING_ROOMS <=4 ~ 1,
                                   DRESSING >= 5 & DRESSING < 10 ~ 100),
         INCAPAZ = WALKING_ROOMS_NOVO + STANDINGUP_NOVO + EATING_NOVO + DRESSING_NOVO,
         INCAPAZ_CLASSE = case_when(INCAPAZ <= 1 ~ 0, # no disability
                                    INCAPAZ > 1 & INCAPAZ <= 16 ~ 1, # disability
                                    INCAPAZ > 12 & INCAPAZ <=300 ~ 3))

# To create the variable METPA

One_4 <-
  One_3 |>
  # calculate HOMA-IR
  dplyr::mutate(METPA = PADTIMES * PADDURAT * PADMETS) |>
  dplyr::mutate(pad_sem =  METPA * 0.2333) |>
  dplyr::mutate(pad_class = case_when(pad_sem >= 450 ~ "ativo",
                                      pad_sem < 450 ~ "inativo"))
# Adjusting sex, age and ethnicity

One_5 <-
  One_4 |>
  mutate(GENDER = case_when(RIAGENDR == 1 ~ "male",
                              RIAGENDR == 2 ~ "female"),
         AGE = case_when(RIDAGEYR >= 65 & RIDAGEYR < 80 ~ "< 80 years",
                              RIDAGEYR >= 80 ~ ">= 80 years"),
         RIDRETH1 = as_factor(RIDRETH1))

# Creating apendicular lean mass, osteoporose and protein uptake variables

One_6 <-
  One_5 |>
  # create apendicular lean mass
  dplyr::mutate(# create obesity class
                OBESITY = case_when(BMXBMI >= 30 ~ "OBESO",
                                    BMXBMI < 30 ~ "NORMAL"),
                # create protein consumption variable of distinct assessments
                PTN = DR1TPROT + DRXTPROT,
                # create relative protein consumption variable
                PTNKG = PTN/BMXWT,
                # create CHO consumption variable
                CHO = DR1TCARB + DRXTCARB,
                # create FAT consumption variable
                FAT = DRXTTFAT + DR1TTFAT,
                # create CALCIUM consuption variable
                CAL = DRXTCALC + DR1TCALC,
                # create ENERGY consumption variable
                ENERGY = DRXTKCAL + DR1TKCAL,
                # create RELATIVE ENERGY - ENERGY/KG
                ENERGY_KG = ENERGY/BMXWT,
                # create ENERGY_PT_MODEL
                ENERGY_PT_MODEL = ENERGY - PTN * 4,
                # create energy class for unlikely data
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
                # create pritein consumptoin status RDA
                PTN_RDA = case_when(PTNKG < 0.8 ~ "A_BAIXO",
                                    PTNKG >=0.8 ~ "B_ADEQUADO"),
                # Saulo/Hamilton protein consumption status
                PTN_STATUS_ROSCHEL = case_when(PTNKG < 0.8 ~ "A_BAIXO",
                                               PTNKG >= 0.8 & PTNKG < 1.2 ~ "B_ADEQUADO",
                                               PTNKG >= 1.2 ~ "D_ELEVADO"),
                # Peso de 8 anos
                MEC8YR = case_when(SDDSRVYR <= 2 ~ 2/4 * WTMEC4YR,
                                   (SDDSRVYR > 2 ~ 1/4 * WTMEC2YR)),
                inAnalysis = (RIDAGEYR >= 65 &
                              !is.na(INCAPAZ_CLASSE) &
                              !is.na(PTN_STATUS) &
                              INCAPAZ_CLASSE != 3 &
                              ENERGY_STATUS == "LIKELY" &
                              DIQ010 < 3 & # Diabetes
                              BPQ020 < 3 & BPQ020 >= 1 & # HAS
                              MCQ160B < 3 & # ICC
                              MCQ160E < 3 # heart attack
                              )
  )


# Final database

One <- One_6

#' ## Define survey design
# Define survey design for overall dataset
NHANES_all <- svydesign(data=One, id=~SDMVPSU, strata=~SDMVSTRA, weights=~MEC8YR, nest=TRUE)

# Create a survey design object for the subset of interest: adults aged 20 and over with a valid depression score
# Subsetting the original survey design object ensures we keep the design information about the number of clusters and strata
NHANES <- subset(NHANES_all, inAnalysis)

#### extrai as variaveis para comparar os resultados do glm vs svyglm
# creating database from subset - the author called of NHANES

id <- as_tibble(NHANES$variables$SEQN)
sexo <- as_tibble(NHANES$variables$RIAGENDR)
raca <- as_tibble(NHANES$variables$RIDRETH1)
idade <- as_tibble(NHANES$variables$RIDAGEYR)
peso <- as_tibble(NHANES$variables$BMXWT)
bmi <- as_tibble(NHANES$variables$BMXBMI)
energy <- as_tibble(NHANES$variables$ENERGY)
cho <- as_tibble(NHANES$variables$CHO)
fat <- as_tibble(NHANES$variables$FAT)
ptn <- as.tibble(NHANES$variables$PTN)
ptn_kg <- as_tibble(NHANES$variables$PTNKG)
ptn_status <- as_tibble(NHANES$variables$PTN_STATUS)
ptn_status_roschel <- as_tibble(NHANES$variables$PTN_STATUS_ROSCHEL)
functioning <- as.tibble(NHANES$variables$INCAPAZ_CLASSE)
obesity <- as_tibble(NHANES$variables$OBESITY)
ptn_rda <- as_tibble(NHANES$variables$PTN_RDA)
energy_kg <- as_tibble(NHANES$variables$ENERGY_KG)
energy_pt_model <- as_tibble(NHANES$variables$ENERGY_PT_MODEL)
pad <- as_tibble(NHANES$variables$pad_class)
weight <- as_tibble(NHANES$variables$MEC8YR)
diabetes <- as_tibble(NHANES$variables$DIQ010)
has <- as_tibble(NHANES$variables$BPQ020)
icc <- as_tibble(NHANES$variables$MCQ160B)
dac <- as_tibble(NHANES$variables$MCQ160C)
ha <- as_tibble(NHANES$variables$MCQ160E) # heart attack
cancer <- as_tibble(NHANES$variables$MCQ220)

# create database to analyse
df_ajust <-
  bind_cols(id,
          sexo,
          raca,
          idade,
          peso,
          bmi,
          obesity,
          energy,
          energy_kg,
          energy_pt_model,
          cho,
          fat,
          ptn,
          ptn_kg,
          ptn_rda,
          ptn_status,
          ptn_status_roschel,
          functioning,
          pad,
          weight,
          diabetes,
          has,
          icc,
          dac,
          ha,
          cancer)|>
  rename("id" = value...1,
         "sexo" = value...2,
         "raca" = value...3,
         "idade" = value...4,
         "peso" = value...5,
         "bmi" = value...6,
         "obesity" = value...7,
         "energy" = value...8,
         "energy_kg" = value...9,
         "energy_kg_model" = value...10,
         "cho" = value...11,
         "fat" = value...12,
         "ptn" = value...13,
         "ptn_kg" = value...14,
         "ptn_rda" = value...15,
         "ptn_status" = value...16,
         "ptn_status_roschel" = value...17,
         "functioning" = value...18,
         "pad" = value...19,
         "weight" = value...20,
         "diabetes" = value...21,
         "has" = value...22,
         "icc" = value...23,
         "dac" = value...24,
         "ha" = value...25,
         "cancer" = value...26) |>
  mutate(
    raca = as.factor(raca),
    functioning = as.factor(functioning),
    diabetes = as.factor(diabetes),
    has = as.factor(has),
    icc = as.factor(icc),
    dac = as.factor(dac),
    ha = as.factor(ha),
    cancer = as.factor(cancer)
  )

# Exploratory analysis ------------------------------------------------------------------------

# General Descriptive and distribution analysis

glimpse(NHANES$variables)

skimr::skim(NHANES$variables)

# TESTING MODELS
# TO avoid scientific notation
options(scipen=999)

# Models to test "RDA vs Non-RDA" and "Low, adequate and high protein" ------------------------

## crude logistic regression -  Consumo de PTN (RDA - >= 0.8 ptn = adequado)
# rda_crude <- glm(as.factor(functioning) ~ ptn_rda, family = binomial, data = df_ajust)

rda_crude_svy <-
  survey::svyglm(
    formula = as.factor(INCAPAZ_CLASSE) ~ PTN_RDA,
    design = NHANES,
    family = binomial(link = "logit")
  )

# sjPlot::tab_model(rda_crude, show.intercept = FALSE)

summary(rda_crude_svy)
cbind(odds = exp(rda_crude_svy$coefficients), exp(confint(rda_crude_svy)))

## Adjusted logistic regression -  Consumo de PTN (RDA - >= 0.8 ptn = adequado)
# rda_adjusted <- glm(as.factor(functioning) ~ ptn_rda + idade + sexo + as_factor(raca) + pad + energy_kg_model, family = binomial, data = df_ajust)

rda_adjusted_svy <-
  survey::svyglm(
    formula = as.factor(INCAPAZ_CLASSE) ~ PTN_RDA + GENDER + AGE + RIDRETH1 + pad_class + ENERGY_PT_MODEL + DIQ010 + BPQ020 + MCQ160B + MCQ160E,
    design = NHANES,
    family = binomial(link = "logit")
  )

# sjPlot::tab_model(rda_adjusted_svy, show.intercept = FALSE)

summary(rda_adjusted_svy)
cbind(odds = exp(rda_adjusted_svy$coefficients), exp(confint(rda_adjusted_svy)))

# ## crude logistic regression -  Consumo de PTN (STU PHILLIPS)
# # stu_crude <- glm(as.factor(functioning) ~ ptn_status, family = binomial, data = df_ajust)
#
# stu_crude_svy <- survey::svyglm(formula = as.factor(INCAPAZ_CLASSE) ~ PTN_STATUS,
#                                    design = NHANES_all,
#                                    family = binomial(link = "logit"))
#
# # sjPlot::tab_model(stu_crude, show.intercept = FALSE)
#
# summary(stu_crude_svy)
# cbind(odds = exp(stu_crude_svy$coefficients),exp(confint(stu_crude_svy)))
#
# ## Adjusted logistic regression -  Consumo de PTN (STU PHILLIPS)
# # stu_adjusted <- glm(as.factor(functioning) ~ ptn_status + idade + sexo + raca + pad + energy_kg_model, family = binomial, data = df_ajust)
#
# stu_adjusted_svy <- survey::svyglm(formula = as.factor(INCAPAZ_CLASSE) ~ PTN_STATUS + GENDER + AGE + RIDRETH1 + pad_class + ENERGY_PT_MODEL,
#                                 design = NHANES_all,
#                                 family = binomial(link = "logit"))
#
# # sjPlot::tab_model(stu_adjusted, show.intercept = FALSE)
#
# summary(stu_adjusted_svy)
# cbind(odds = exp(stu_adjusted_svy$coefficients),exp(confint(stu_adjusted_svy)))

## crude logistic regression -  Consumo de PTN (ROSCHEL/SAULO)
# stu_crude <- glm(as.factor(functioning) ~ ptn_status, family = binomial, data = df_ajust)

rs_crude_svy <-
  survey::svyglm(
    formula = as.factor(INCAPAZ_CLASSE) ~ PTN_STATUS_ROSCHEL,
    design = NHANES,
    family = binomial(link = "logit")
  )

# sjPlot::tab_model(stu_crude, show.intercept = FALSE)

summary(rs_crude_svy)
cbind(odds = exp(rs_crude_svy$coefficients), exp(confint(rs_crude_svy)))

## Adjusted logistic regression -  Consumo de PTN (ROSCHEL/SAULO)
# stu_adjusted <- glm(as.factor(functioning) ~ ptn_status + idade + sexo + raca + pad + energy_kg_model, family = binomial, data = df_ajust)

rs_adjusted_svy <-
  survey::svyglm(
    formula = as.factor(INCAPAZ_CLASSE) ~ PTN_STATUS_ROSCHEL + GENDER + AGE + RIDRETH1 + pad_class + ENERGY_PT_MODEL + DIQ010 + BPQ020 + MCQ160B + MCQ160E,
    design = NHANES,
    family = binomial(link = "logit")
  )

# sjPlot::tab_model(stu_adjusted, show.intercept = FALSE)

summary(rs_adjusted_svy)
cbind(odds = exp(rs_adjusted_svy$coefficients), exp(confint(rs_adjusted_svy)))
