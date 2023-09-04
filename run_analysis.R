library(dplyr)
library(readr)
library(tibble)
library(tidyr)
library(stringr)

## decompress zip file into tempdir()
zipfile <- "~/R/Coursera/CleaningAssignment/getdata_projectfiles_UCI HAR Dataset.zip"
temp_dir <- tempdir()
unzip(zipfile, exdir = temp_dir)
read_txt_as_tbl <- function(txt_file) {
      tbl <- read_table(txt_file, col_names = FALSE) %>%
            as_tibble()
      return(tbl)
}
folders <- c(
      "UCI HAR Dataset/test",
      "UCI HAR Dataset/train")

## Read X_, y_, and subject_ folders into tibbles and assign names
data_list1 <- lapply(folders, function(folder) {
      folder_path <- file.path(temp_dir, folder)
      txt_files <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE)
      tbl_list <- lapply(txt_files, read_txt_as_tbl)
      names(tbl_list) <- tools::file_path_sans_ext(basename(txt_files))
      return(tbl_list)
})

## Read 'features' into vector
features_path <- normalizePath("~/R/Coursera/CleaningAssignment/UCI HAR Dataset/features.txt")
features <- read.table(features_path, header = FALSE)
features_labels <- features$V2 
features_labels <- sub("^\\d+\\s+", "", features_labels)


file <- "UCI HAR Dataset/activity_labels.txt"

folders <- c(
      "UCI HAR Dataset/test/Inertial Signals",
      "UCI HAR Dataset/train/Inertial Signals"
)

read_calc <- function(txt_file) {
      tbl <- read_table(txt_file, col_names = FALSE)
      tbl[] <- lapply(tbl, as.numeric)
      variable_name <- gsub("_test\\.txt$|_train\\.txt$", "", basename(txt_file))
      variable_name <- gsub("_", " ", variable_name, fixed = TRUE)
      tbl$mean <- rowMeans(tbl)
      tbl$sd <- apply(tbl, 1, sd)
      colnames(tbl)[ncol(tbl)-2] <- paste0(variable_name, " Mean")
      colnames(tbl)[ncol(tbl)-1] <- paste0(variable_name, " SD")
      tbl <- tbl %>%
            select(matches("Mean|SD"))
      return(tbl)
}

## Read Inertial Signals folder files, calculate Mean and SD, store as tbl_df
mean_sd_list <- lapply(folders, function(folder) {
      folder_path <- file.path(temp_dir, folder)
      txt_files <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE)
      tbl_list <- lapply(txt_files, read_calc)
      tbl_names <- tools::file_path_sans_ext(basename(txt_files))
      names(tbl_list) <- tbl_names
      tbl_list <- lapply(tbl_list, function(tbl) tbl[, c(1, 2)])
      return(tbl_list)
})

colnames_set <- c("ID", "Activity", features_labels)

## Merge test and train first-level data sets, name columns
## Add Group column to identify test vs. train
merged_test <- bind_cols(data_list1[[1]]$subject_test,
                         data_list1[[1]]$y_test,
                         data_list1[[1]]$X_test)
            colnames(merged_test) <- colnames_set
            for (tbl in mean_sd_list[[1]]) {
                  test_data_cols <- tbl[, (names(tbl))]
                  merged_test <- bind_cols(merged_test, test_data_cols)
            }
            merged_test <- merged_test %>% 
                  add_column(Group = "test", .before = 3, .name_repair = "minimal")
            
merged_train <- bind_cols(data_list1[[2]]$subject_train,
                          data_list1[[2]]$y_train,
                          data_list1[[2]]$X_train)
            colnames(merged_train) <- colnames_set
            for (tbl in mean_sd_list[[2]]) {
                  train_data_cols <- tbl[, (names(tbl))]
                  merged_train <- bind_cols(merged_train, train_data_cols)
            }
            merged_train <- merged_train %>% 
                  add_column(Group = "train", .before = 3, .name_repair = "minimal")

## Stack all the rows of the 'test' and 'train' data into a combined data set
merged_all <- bind_rows(merged_test,
                        merged_train)

## Read 'activity_labels' into merged_all
activities <- data.frame(
      code = 1:6, 
      words = c("walking", "walking_upstairs", "walking_downstairs", "sitting", "standing", "laying")
)
merged_all <- 
      left_join(merged_all, activities, by = c("Activity" = "code")) %>%
      rename(Activity_Desc = words) %>%
      select(-Activity) %>%
      rename(Activity = Activity_Desc) %>%
      relocate(Activity, .before = 2)

## group merged_all by unique combinations of Subject, Activity, and Group
## create columns with Mean and SD for each value for each group
grouped_all <- merged_all %>%
      group_by(ID, Activity, Group)
grouped_calc <- grouped_all %>%
      select(4:582) %>%
      summarize(
            across(1:579, mean, .names = "Mean_{.col}"),
            across(1:579, sd, .names = "SD_{.col}")
      )

## systematically rename cols to descriptive names based on text patterns
desc_names <- function(column_name) {
      col_name <- gsub("Mean_t", "Mean Time Domain ", column_name)
      col_name <- gsub("Mean_f", "Mean Frequency Domain ", col_name)
      col_name <- gsub("SD_t ", "SD Time Domain", column_name)
      col_name <- gsub("SD_f", "SD Frequency Domain", col_name)
      col_name <- gsub("BodyBody", " Body", col_name)
      col_name <- gsub("[Bb]ody", " Body", col_name)
      col_name <- gsub("[Gg]ravity", "Gravity ", col_name)
      col_name <- gsub("[Aa]cc", " Acceleration", col_name)
      col_name <- gsub("Jerk", " Jerk", col_name)
      col_name <- gsub("Mag", " Magnitude", col_name)
      col_name <- gsub("[Gg]yro", " Gyroscope", col_name)
      col_name <- gsub("angle\\(([^)]+)\\)", "Angle \\1", col_name)
      col_name <- gsub("total", "Total", col_name)
      col_name <- gsub("[-_]", " ", col_name)
      col_name <- gsub("\\(\\)", "", col_name) # Remove empty parentheses
      col_name <- gsub("bandsEnergy", "Bands Energy", col_name)
      col_name <- gsub("([XYZ])", "(\\1 Axis)", col_name)
      col_name <- gsub("([XYZxyz]), ([XYZxyz])", "(\\1 Axis, \\2 Axis)", col_name)
      col_name <- gsub("([XYZxyz]), ([0-9])", "(\\1, \\2)", col_name)
      
      return(col_name)
}

grouped_calc <- grouped_calc %>%
      rename_all(~desc_names(.))

desc_names <- function(column_name) {
      col_name <- gsub(" t ", " Time Domain ", column_name)
      col_name <- gsub(" f ", " Frequency Domain ", col_name)
      
      return(col_name)
}

final_data <- grouped_calc %>%
      rename_all(~desc_names(.))

## create second tidy data set with avg of each col for each activity/subject
activity_subj_averages <- final_data %>%
      group_by(Activity, ID) %>%
      summarize(across(.fns = mean))%>%
      select(-Group)

##produce .txt file of the second tidy dataset
file_path <- "submission.txt"
submission <- write.table(activity_subj_averages, 
                          file = file_path, 
                          sep = "\t", 
                          row.names = FALSE)

## for documentation purposes, create a markdown list of variable names
variable_names <- names(final_data)
variable_list <- paste0("* ", variable_names)
variable_list_text <- paste(variable_list, collapse = "\n")
cat("## Variable List\n\n", variable_list_text, file = "variable_list.md")