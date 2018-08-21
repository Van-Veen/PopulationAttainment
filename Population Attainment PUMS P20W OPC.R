
library(data.table)
library(questionr)
library(magrittr)

# DONE. Bundle graduate/professional degrees into one category 
# DONE. Include unattainment as a toggle (at end, recode V2 to Attainment/Unattainment)
# DONE. Add in education category: Some college no degree
# DONE. Add in education category: HS Equivalency onlya
# DONE. For the two new education categories, figure out way to depict whether they are currently enrolled or not 
# DONE. remove clean pct calcs, let power bi do housekeeping
# 7. Add national rates
# 8. Add benchmark rates
# DONE. Add All groupings and calculations
# DONE. Add hispanic/latinx grouping

# The three functions below are used to calculate population attainment. In the end, the heavy lifting is done by the
# 'add_education' function, which incorporates the other two functions internally. We'll use this function to loop 
# through an array of file paths to process and calculate attainment for multiple years of PUMS data

prep_df = function(fp){
  
  DF = fread(fp)
  
  DF = DF[, COHORT := ifelse(AGEP >= 25 & AGEP <= 64, 1, 0)]
  DF = DF[ , DEGREE := ifelse(SCHL %in% c(20:24), 1, 0)]
  
  DF = DF[, SEX := ifelse(SEX == 1, "Male", "Female")]
  
  DF = DF[, AGE_GROUP := NA]
  DF = DF[, AGE_GROUP := ifelse(AGEP >= 18 & AGEP <= 24, "Age 18 - 24", AGE_GROUP)]
  DF = DF[, AGE_GROUP := ifelse(AGEP >= 25 & AGEP <= 34, "Age 25 - 34", AGE_GROUP)]
  DF = DF[, AGE_GROUP := ifelse(AGEP >= 35 & AGEP <= 44, "Age 35 - 44", AGE_GROUP)]
  DF = DF[, AGE_GROUP := ifelse(AGEP >= 45 & AGEP <= 54, "Age 45 - 54", AGE_GROUP)]
  DF = DF[, AGE_GROUP := ifelse(AGEP >= 55 & AGEP <= 64, "Age 55 - 64", AGE_GROUP)]
  
  DF = DF[, RACE := NA] 
  DF = DF[, RACE := ifelse(RAC1P == 1, "White", RACE)]
  DF = DF[, RACE := ifelse(RAC1P == 2, "Black or African American", RACE)]
  DF = DF[, RACE := ifelse(RAC1P == 5, "American Indian or Alaskan Native", RACE)]
  DF = DF[, RACE := ifelse(RAC1P == 6, "Asian", RACE)]
  DF = DF[, RACE := ifelse(RAC1P == 7, "Native Hawaiian or Pacific Islander", RACE)]
  DF = DF[, RACE := ifelse(RAC1P == 9, "More than One Race", RACE)]
  
  DF = DF[, ETHNICITY := NA]
  DF = DF[, ETHNICITY := ifelse(HISP != 1, "Hispanic/Latinx", "Not Hispanic/Latinx")]
  
  DF = DF[, EDUCATION := NA]
  DF = DF[, EDUCATION := ifelse(SCHL == 20, "Associate's Degree", EDUCATION)]
  DF = DF[, EDUCATION := ifelse(SCHL == 21, "Bachelor's Degree", EDUCATION)]
  DF = DF[, EDUCATION := ifelse(SCHL == 22, "Graduate/Professional Degree", EDUCATION)]
  DF = DF[, EDUCATION := ifelse(SCHL == 23, "Graduate/Professional Degree", EDUCATION)]
  DF = DF[, EDUCATION := ifelse(SCHL == 24, "Graduate/Professional Degree", EDUCATION)]
  DF = DF[, EDUCATION := ifelse(SCHL %in% c(18,19) & SCHG %in% c(15, 16), "Some College/No Degree - Currently Enrolled", EDUCATION)]
  DF = DF[, EDUCATION := ifelse(SCHL %in% c(19, 19) & is.na(SCHG), "Some College/No Degree - Not Enrolled", EDUCATION)]
  DF = DF[, EDUCATION := ifelse(SCHL == 17 & SCHG %in% c(15, 16), "High School Equivalency - Currently Enrolled", EDUCATION)]
  DF = DF[, EDUCATION := ifelse(SCHL == 17 & is.na(SCHG), "High School Equivalency - Not Enrolled", EDUCATION)]
  
  #DF = DF[COHORT == 1]
  
  return(DF)
  
}

process_rates = function(fp, year){
  DF = prep_df(fp)
  
  ALL = DF[COHORT == 1, wtd.table(DEGREE, weights = PWGTP)] %>% 
    prop.table(.) %>% 
    data.table(.) %>% 
    .[, V2 := as.character(V1)] %>% 
    .[, V1 := "All"] %>% 
    .[, Group := "All"] %>% 
    .[, Year := year]
  
  SEX = DF[COHORT == 1, wtd.table(SEX, DEGREE, weights = PWGTP)] %>% 
    prop.table(., margin = 1) %>% 
    data.table(.) %>% 
    .[, Group := "Sex"] %>% 
    .[, Year := year]
  
  AGE = DF[COHORT == 1 & !is.na(AGE_GROUP) & AGE_GROUP != "Age 18 - 24", wtd.table(AGE_GROUP, DEGREE, weights = PWGTP)] %>% 
    prop.table(., margin = 1) %>% 
    data.table(.) %>% 
    .[, Group := "Age"] %>% 
    .[, Year := year]
  
  RACE = DF[COHORT == 1 & !is.na(RACE), wtd.table(RACE, DEGREE, weights = PWGTP)] %>% 
    prop.table(., margin = 1) %>% 
    data.table(.) %>% 
    .[, Group := "Race"] %>% 
    .[, Year := year]
  
  ETHNICITY = DF[COHORT == 1 & !is.na(ETHNICITY), wtd.table(ETHNICITY, DEGREE, weights = PWGTP)] %>% 
    prop.table(., margin = 1) %>% 
    data.table(.) %>% 
    .[, Group := "Ethnicity"] %>% 
    .[, Year := year]
  
  ATT = rbind(ALL, SEX, RACE, AGE, ETHNICITY) %>% 
    .[, Chart1 := 1]
  
  return(ATT)
  
}

add_education = function(fp, year){
  
  DF = prep_df(fp)
  
  ALL = DF[COHORT == 1, wtd.table(EDUCATION, weights = PWGTP)] %>% 
    data.table(.) %>% 
    .[, N := N/as.numeric(DF[COHORT == 1, wtd.table(COHORT, weights = PWGTP)])] %>% 
    setnames(., names(.), c("Education", "Ed_rate")) %>% 
    .[, V1 := "All"] %>% 
    .[, V2 := 1]
    
  FEMALE = DF[SEX == "Female", wtd.table(EDUCATION, weights = PWGTP)] %>% 
    data.table(.) %>% 
    .[, N := N/as.numeric(DF[SEX == "Female", wtd.table(SEX, weights = PWGTP)])] %>% 
    setnames(., names(.), c("Education", "Ed_rate")) %>% 
    .[, V1 := "Female"] %>% 
    .[, V2 := 1]
  
  MALE = DF[SEX == "Male", wtd.table(EDUCATION, weights = PWGTP)] %>% 
    data.table(.) %>% 
    .[, N := N/as.numeric(DF[SEX == "Male", wtd.table(SEX, weights = PWGTP)])] %>% 
    setnames(., names(.), c("Education", "Ed_rate")) %>% 
    .[, V1 := "Male"] %>% 
    .[, V2 := 1]
  
  AMIND = DF[RACE == "American Indian or Alaskan Native", wtd.table(EDUCATION, weights = PWGTP)] %>% 
    data.table(.) %>% 
    .[, N := N/as.numeric(DF[RACE == "American Indian or Alaskan Native", wtd.table(RACE, weights = PWGTP)])] %>% 
    setnames(., names(.), c("Education", "Ed_rate")) %>% 
    .[, V1 := "American Indian or Alaskan Native"] %>% 
    .[, V2 := 1]
  
  ASIAN = DF[RACE == "Asian", wtd.table(EDUCATION, weights = PWGTP)] %>% 
    data.table(.) %>% 
    .[, N := N/as.numeric(DF[RACE == "Asian", wtd.table(RACE, weights = PWGTP)])] %>% 
    setnames(., names(.), c("Education", "Ed_rate")) %>% 
    .[, V1 := "Asian"] %>% 
    .[, V2 := 1]
  
  BLACK = DF[RACE == "Black or African American", wtd.table(EDUCATION, weights = PWGTP)] %>% 
    data.table(.) %>% 
    .[, N := N/as.numeric(DF[RACE == "Black or African American", wtd.table(RACE, weights = PWGTP)])] %>% 
    setnames(., names(.), c("Education", "Ed_rate")) %>% 
    .[, V1 := "Black or African American"] %>% 
    .[, V2 := 1] 
  
  MTO = DF[RACE == "More than One Race", wtd.table(EDUCATION, weights = PWGTP)] %>% 
    data.table(.) %>% 
    .[, N := N/as.numeric(DF[RACE == "More than One Race", wtd.table(RACE, weights = PWGTP)])] %>% 
    setnames(., names(.), c("Education", "Ed_rate")) %>% 
    .[, V1 := "More than One Race"] %>% 
    .[, V2 := 1] 
  
  PI = DF[RACE == "Native Hawaiian or Pacific Islander", wtd.table(EDUCATION, weights = PWGTP)] %>% 
    data.table(.) %>% 
    .[, N := N/as.numeric(DF[RACE == "Native Hawaiian or Pacific Islander", wtd.table(RACE, weights = PWGTP)])] %>% 
    setnames(., names(.), c("Education", "Ed_rate")) %>% 
    .[, V1 := "Native Hawaiian or Pacific Islander"] %>% 
    .[, V2 := 1] 
  
  WHITE = DF[RACE == "White", wtd.table(EDUCATION, weights = PWGTP)] %>% 
    data.table(.) %>% 
    .[, N := N/as.numeric(DF[RACE == "White", wtd.table(RACE, weights = PWGTP)])] %>% 
    setnames(., names(.), c("Education", "Ed_rate")) %>% 
    .[, V1 := "White"] %>% 
    .[, V2 := 1]
  
  HISP = DF[ETHNICITY == "Hispanic/Latinx", wtd.table(EDUCATION, weights = PWGTP)] %>% 
    data.table(.) %>% 
    .[, N := N/as.numeric(DF[ETHNICITY == "Hispanic/Latinx", wtd.table(ETHNICITY, weights = PWGTP)])] %>% 
    setnames(., names(.), c("Education", "Ed_rate")) %>% 
    .[, V1 := "Hispanic/Latinx"] %>% 
    .[, V2 := 1]
  
  NOT_HISP = DF[ETHNICITY == "Not Hispanic/Latinx", wtd.table(EDUCATION, weights = PWGTP)] %>% 
    data.table(.) %>% 
    .[, N := N/as.numeric(DF[ETHNICITY == "Not Hispanic/Latinx", wtd.table(ETHNICITY, weights = PWGTP)])] %>% 
    setnames(., names(.), c("Education", "Ed_rate")) %>% 
    .[, V1 := "Not Hispanic/Latinx"] %>% 
    .[, V2 := 1]
  
  AGE1824 = DF[AGE_GROUP == "Age 18 - 24", wtd.table(EDUCATION, weights = PWGTP)] %>% 
    data.table(.) %>% 
    .[, N := N/as.numeric(DF[AGE_GROUP == "Age 18 - 24", wtd.table(AGE_GROUP, weights = PWGTP)])] %>% 
    setnames(., names(.), c("Education", "Ed_rate")) %>% 
    .[, V1 := "Age 18 - 24"] %>% 
    .[, V2 := 1]
  
  AGE2534 = DF[AGE_GROUP == "Age 25 - 34", wtd.table(EDUCATION, weights = PWGTP)] %>% 
    data.table(.) %>% 
    .[, N := N/as.numeric(DF[AGE_GROUP == "Age 25 - 34", wtd.table(AGE_GROUP, weights = PWGTP)])] %>% 
    setnames(., names(.), c("Education", "Ed_rate")) %>% 
    .[, V1 := "Age 25 - 34"] %>% 
    .[, V2 := 1]
  
  AGE3544 = DF[AGE_GROUP == "Age 35 - 44", wtd.table(EDUCATION, weights = PWGTP)] %>% 
    data.table(.) %>% 
    .[, N := N/as.numeric(DF[AGE_GROUP == "Age 35 - 44", wtd.table(AGE_GROUP, weights = PWGTP)])] %>% 
    setnames(., names(.), c("Education", "Ed_rate")) %>% 
    .[, V1 := "Age 35 - 44"] %>% 
    .[, V2 := 1]
  
  AGE4554 = DF[AGE_GROUP == "Age 45 - 54", wtd.table(EDUCATION, weights = PWGTP)] %>% 
    data.table(.) %>% 
    .[, N := N/as.numeric(DF[AGE_GROUP == "Age 45 - 54", wtd.table(AGE_GROUP, weights = PWGTP)])] %>% 
    setnames(., names(.), c("Education", "Ed_rate")) %>% 
    .[, V1 := "Age 45 - 54"] %>% 
    .[, V2 := 1]
  
  AGE5564 = DF[AGE_GROUP == "Age 55 - 64", wtd.table(EDUCATION, weights = PWGTP)] %>% 
    data.table(.) %>% 
    .[, N := N/as.numeric(DF[AGE_GROUP == "Age 55 - 64", wtd.table(AGE_GROUP, weights = PWGTP)])] %>% 
    setnames(., names(.), c("Education", "Ed_rate")) %>% 
    .[, V1 := "Age 55 - 64"] %>% 
    .[, V2 := 1]
  
  ED_RATES = rbind(ALL, FEMALE, MALE, AMIND, ASIAN, BLACK, MTO, PI, WHITE, HISP, NOT_HISP,AGE1824, AGE2534, AGE3544, AGE4554, AGE5564) %>% 
    .[, V2 := as.character(V2)]
  
  RATES = process_rates(fp, year) %>% .[, V2 := as.character(V2)]
  
  FINAL = merge(RATES, ED_RATES, by = c("V1", "V2"), all = T)
  
  setnames(FINAL, c("V1", "V2", "N"), c("Category", "Attainment", "Att_rate"))
  
  FINAL = FINAL[, Attainment := ifelse(Attainment == 1, "Attainment", "Unattainment")] %>% 
    .[!is.na(Education) & Education != "Associate's Degree", Chart1 := 0]

  return(FINAL)
  
}


# file paths to loop through
fp16 = "C:/Users/jstewart/Downloads/csv_pri16/ss16pri.csv"
fp15 = "C:/Users/jstewart/Downloads/csv_pri15/ss15pri.csv"
fp14 = "C:/Users/jstewart/Downloads/csv_pri14/ss14pri.csv"
fp13 = "C:/Users/jstewart/Downloads/csv_pri13/ss13pri.csv"
fp12 = "C:/Users/jstewart/Downloads/csv_pri12/ss12pri.csv"

all_fps = c(fp16, fp15, fp14, fp13, fp12)
fp_yrs = seq(2016, 2012, by = -1)


# looping through all the file paths, process data, and combining each iteration into a final data set
for(i in 1:length(all_fps)){
  
  temp = add_education(all_fps[i], fp_yrs[i])
  
  if(i == 1){
    FINAL = temp
  }else{
    FINAL = rbind(FINAL, temp)
  }
  
}


FINAL1 = FINAL[Chart1 == 1] %>% 
  .[, -c("Education", "Ed_rate"), with = F] %>% 
  .[Attainment == "Unattainment", Attainment := "Non-Attainment"]

slicer = FINAL1[, c("Group", "Category"), with = F] %>% 
  unique(.) %>% 
  .[Category == "Age 18 - 24", Group := "Age"]


FINAL2 = FINAL[!is.na(Education)] %>% .[, -c("Attainment", "Chart1"), with = F] %>% 
  .[Category == "Age 18 - 24", Group := "Age"] %>% 
  .[Category == "Age 18 - 24", Year := rep(2016:2012, each = 7)]


#fwrite(FINAL, "J:/Partners_and_projects/State/DLT - Dept of Labor and Training/Dashboards/P20W Dashboard/Indicators/Higher Ed (OPC)/PUMS Attainment2.csv")
fwrite(FINAL1, "J:/Partners_and_projects/State/DLT - Dept of Labor and Training/Dashboards/P20W Dashboard/Indicators/Higher Ed (OPC)/PUMS Attainment General.csv")
fwrite(FINAL2, "J:/Partners_and_projects/State/DLT - Dept of Labor and Training/Dashboards/P20W Dashboard/Indicators/Higher Ed (OPC)/PUMS Attainment by Education.csv")
fwrite(slicer, "J:/Partners_and_projects/State/DLT - Dept of Labor and Training/Dashboards/P20W Dashboard/Indicators/Higher Ed (OPC)/edSlicer.csv")


