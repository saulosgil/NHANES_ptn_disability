nrow(NHANES$variables)
unique(NHANES$variables$PTN_MID_HIGH)

# continuos data
NHANES$variables |>
  filter(PTN_MID_HIGH == "D_acima1.4") |> # "D_acima1.4"  "D_acima1.4   "D_acima1.4
  select(RIDAGEYR, BMXBMI, ENERGY, CHO, FAT, PTN, PTNKG) |>
  skimr::skim()

NHANES$variables |>
  filter(PTN_MID_HIGH == "D_acima1.4") |>
  select(RIDAGEYR, BMXBMI, ENERGY, CHO, FAT, PTN, PTNKG) |>
  summarise(mean = mean(ENERGY),
            df = sd(ENERGY))

NHANES$variables |>
  filter(PTN_MID_HIGH == "D_acima1.4") |>
  select(RIDAGEYR, BMXBMI, ENERGY, CHO, FAT, PTN, PTNKG) |>
  summarise(mean = mean(CHO),
            df = sd(CHO))

# Categorical data

NHANES$variables |>
  filter(PTN_MID_HIGH == "D_acima1.4") |>
  mutate(AGE = as_factor(AGE),
         GENDER = as_factor(GENDER),
         DIQ010 = as_factor(DIQ010),
         BPQ020 = as_factor(BPQ020),
         MCQ160B = as_factor(MCQ160B),
         MCQ160E = as_factor(MCQ160E),
         pad_class = as_factor(pad_class)) |>
  select(AGE, GENDER, RIDRETH1, BPQ020, DIQ010, MCQ160B, MCQ160E, pad_class) |>
  skimr::skim()


BPQ020  # HAS
DIQ010  # Diabetes
MCQ160B # ICC
MCQ160E # heart attack
round(88  /238*100)
round(37  /238*100)
round(17  /238*100)
round(21  /238*100)
round(65  /238*100)
round(  /238*100)
round(  /238*100)

238 - sum(c(45,7,148,26,12))
sum(c(19,3,62,11,5))
