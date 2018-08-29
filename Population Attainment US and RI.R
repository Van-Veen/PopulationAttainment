
# Processing Rhode Island State Data First

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
  
  temp_RI = add_education(all_fps[i], fp_yrs[i])
  
  if(i == 1){
    FINAL_RI = temp_RI
  }else{
    FINAL_RI = rbind(FINAL_RI, temp_RI)
  }
  
}


FINAL_RI_1 = FINAL_RI[Chart1 == 1] %>% 
  .[, -c("Education", "Ed_rate"), with = F] %>% 
  .[Attainment == "Unattainment", Attainment := "Non-Attainment"]

slicer_RI = FINAL_RI_1[, c("Group", "Category"), with = F] %>% 
  unique(.) %>% 
  .[Category == "Age 18 - 24", Group := "Age"]


FINAL_RI_2 = FINAL_RI[!is.na(Education)] %>% .[, -c("Attainment", "Chart1"), with = F] %>% 
  .[Category == "Age 18 - 24", Group := "Age"] %>% 
  .[Category == "Age 18 - 24", Year := rep(2016:2012, each = 7)]


# Then we move on to US Files
# NOTE: The processing here takes about 35-45 minutes. Plan to work on something else while this
# program runs in the background.


# base file paths for US files
US16 = "C:/Users/jstewart/Downloads/csv_pus2016/"
US15 = "C:/Users/jstewart/Downloads/csv_pus2015/"
US14 = "C:/Users/jstewart/Downloads/csv_pus2014/"
US13 = "C:/Users/jstewart/Downloads/csv_pus2013/"
US12 = "C:/Users/jstewart/Downloads/csv_pus2012/"

# Year indicator for each file
fp_yrs = seq(2016, 2012, by = -1)

# putting all US file paths into one array
all_fps = c(US16, US15, US14, US13, US12)

# Looping through each file path, constructing each year of data, calculating rates and saving the output
# to a final table

# NOTE: Each year file is pretty large and this loop will take some time. Plan to work on something else
# or go get a coffee or something. I listened to Fugazi's 'Steady Diet of Nothing' while this ran, and it
# was a very pleasant experience.

for(i in 1:length(all_fps)){
  
  temp = add_education(all_fps[i], fp_yrs[i], needs_construction = T)
  
  if(i == 1){
    FINAL = temp
  }else{
    FINAL = rbind(FINAL, temp)
  }
  
}

FINAL_US_1 = FINAL[Chart1 == 1] %>% 
  .[, -c("Education", "Ed_rate"), with = F] %>% 
  .[Attainment == "Unattainment", Attainment := "Non-Attainment"]

FINAL_US_1_all = FINAL_US_1[Category == "All"] %>% 
  .[Category == "All", Category := "U.S. Average"]

FINAL_US_2 = FINAL[!is.na(Education)] %>% .[, -c("Attainment", "Chart1"), with = F] %>% 
  .[Category == "Age 18 - 24", Group := "Age"] %>% 
  .[Category == "Age 18 - 24", Year := rep(2016:2012, each = 7)]



FINAL_1_ALL = rbind(FINAL_RI_1, FINAL_US_1_all) %>% 
  .[Category == "All", Category := "RI Average"]

#FINAL_RI_x = data.table("Category" = rep("U.S. Average", 35), "Att_rate" = rep(NA, 35), "Group" = rep("All", 35),
#                        "Year" = rep(2016:2012, each = 7), "Education" = rep(unique(FINAL_RI_2$Education), 5), "Ed_rate" = rep(NA, 35))

FINAL_US_X = FINAL_US_2[Group == "All"] %>% 
  .[Category == "All", Category := "U.S. Average"]


FINAL_RI_2 = FINAL_RI_2[Group == "All", Category := "RI Average"] %>% 
  rbind(., FINAL_US_X)


slicer_US = FINAL_RI_2[, c("Group", "Category"), with = F] %>% 
  unique(.) %>% 
  .[Category == "Age 18 - 24", Group := "Age"]


fwrite(FINAL_1_ALL, "J:/Partners_and_projects/State/DLT - Dept of Labor and Training/Dashboards/P20W Dashboard/Indicators/Higher Ed (OPC)/PUMS Attainment General.csv")
fwrite(FINAL_RI_2, "J:/Partners_and_projects/State/DLT - Dept of Labor and Training/Dashboards/P20W Dashboard/Indicators/Higher Ed (OPC)/PUMS Attainment by Education.csv")
fwrite(slicer_US, "J:/Partners_and_projects/State/DLT - Dept of Labor and Training/Dashboards/P20W Dashboard/Indicators/Higher Ed (OPC)/edSlicer.csv")



FINAL_1_ALL[Group == "All", table(Category)]
FINAL_RI_2[Group == "All", table(Category)]
slicer_US[Group == "All", table(Category)]
