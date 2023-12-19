generate_folders <- function(dir_path){
  check_and_generate_folder(dir_path, 'admin2_surveys')
  check_and_generate_folder(dir_path, 'issue_surveys')
  check_and_generate_folder(dir_path, 'lhz_surveys')
  check_and_generate_folder(dir_path, 'output')
  check_and_generate_folder(dir_path, 'backup_folder')
  check_and_generate_folder(dir_path, 'SMART_surveys_to_update')
  
  check_and_generate_folder(dir_path, 'admin2_surveys/aggregate_surveys')
  check_and_generate_folder(dir_path, 'admin2_surveys/individual_surveys')
  check_and_generate_folder(dir_path, 'lhz_surveys/aggregate_surveys')
  check_and_generate_folder(dir_path, 'lhz_surveys/individual_surveys')
  
  check_and_generate_folder(dir_path, 'admin2_surveys/aggregate_surveys/backup_folder')
  check_and_generate_folder(dir_path, 'admin2_surveys/aggregate_surveys/csv_files')
  check_and_generate_folder(dir_path, 'admin2_surveys/aggregate_surveys/smart_surveys')
  check_and_generate_folder(dir_path, 'admin2_surveys/individual_surveys/backup_folder')
  check_and_generate_folder(dir_path, 'admin2_surveys/individual_surveys/csv_files')
  check_and_generate_folder(dir_path, 'admin2_surveys/individual_surveys/smart_surveys')
  
  check_and_generate_folder(dir_path, 'lhz_surveys/individual_surveys/backup_folder')
  check_and_generate_folder(dir_path, 'lhz_surveys/individual_surveys/csv_files')
  check_and_generate_folder(dir_path, 'lhz_surveys/individual_surveys/smart_surveys')
  check_and_generate_folder(dir_path, 'lhz_surveys/aggregate_surveys/smart_surveys')
}


##Function to check if the folder exist and if not create it
check_and_generate_folder <- function(dir_path, name){
  if (file.exists(paste(dir_path, name, sep='')) == FALSE){
    dir.create(file.path(paste(dir_path, name, sep='')))
  }
}


