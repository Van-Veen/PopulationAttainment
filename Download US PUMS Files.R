


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



