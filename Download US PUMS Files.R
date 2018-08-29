


download_base_fp = "https://www2.census.gov/programs-surveys/acs/data/pums/"
survey_years = seq(2016, 2012)

all_download_paths = paste(download_base_fp, survey_years, "/5-Year/csv_pus.zip", sep = "")

# Constructing destination paths
destination_base_path = "C:/Users/jstewart/Desktop/US_PUMS"
all_destination_paths = paste(destination_base_path, "_", survey_years, ".zip", sep = "")


# Looping through to download each file
for(i in 1:length(all_download_paths)){
  
  download.file(all_download_paths[i], all_destination_paths[i])
  
  
  
}

download.file("https://www2.census.gov/programs-surveys/acs/data/pums/2016/5-Year/csv_hak.zip", destfile = destination_file)


# Working with one year. Each year file has the files broken down into more than one csv

library(data.table)
library(magrittr)

test_fp = "C://Users/jstewart/Downloads/csv_pus2016/"

# The PUMS data for the US are very large and are broken into multiple .csv files for each survey year. The construct_year
# function identifies the .csv files for a selected file path and reads in each one, reading only the necessary columns and 
# combining them into one unified data.table object. We can pass the side effects of this function to the processing functions
# that we wrote for Rhode Island state data. 

construct_year = function(file_path){
  
  all_files = list.files(file_path) %>% 
    .[!grepl("\\.pdf", .)]
  
  keeper_columns = c("AGEP", "SCHL", "SEX", "RAC1P", "HISP", "SCHG", "PWGTP")
  
  for(i in 1:length(all_files)){
    
    file_i = paste(file_path, all_files[i], sep = "")
    
    DF = fread(file_i, select = keeper_columns)
    
    if(i == 1){
      FINAL_DF = DF
    }else{
      FINAL_DF = rbind(FINAL_DF, DF)
    }
    
    
  }
  
  return(FINAL_DF)
  
}


temp = add_education(test_fp, year = 2016,  needs_construction = T)



