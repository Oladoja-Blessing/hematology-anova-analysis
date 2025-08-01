# Load libraries
library(dplyr)
library(tidyr)
library(janitor)
library(plotrix)
library(agricolae)
library(tibble)

# STEP 1: Clean column names to make them tidy
df_nw_clean <- df_nw %>% 
  janitor::clean_names()  # converts to snake_case

# STEP 2: Create summary table with mean ± SE per treatment
df_reg <- df_nw_clean %>%
  group_by(treatment) %>%
  summarise(across(
    plt:hct,  # update this to match the cleaned names
    list(mean = ~mean(.x, na.rm = TRUE),
         se = ~plotrix::std.error(.x, na.rm = TRUE)),
    .names = "{.col}_{.fn}"
  )) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  {
    cols_to_unite <- names(.)[!names(.) %in% "treatment"]
    cols_grouped <- unique(gsub("_(mean|se)$", "", cols_to_unite))
    out <- .
    for (col in cols_grouped) {
      mean_col <- paste0(col, "_mean")
      se_col <- paste0(col, "_se")
      new_col <- col
      out <- unite(out, !!new_col, all_of(c(mean_col, se_col)), sep = "±", remove = TRUE)
    }
    out
  }

# STEP 3: Function to perform ANOVA + HSD and extract group letters
get_group_letters <- function(data, var) {
  form <- as.formula(paste(var, "~ treatment"))
  aov_model <- aov(form, data = data)
  hsd_result <- HSD.test(aov_model, "treatment")
  hsd_result$groups %>%
    rownames_to_column("treatment") %>%
    select(treatment, group = groups) %>%
    rename(!!paste0(var, "_group") := group)
}

# STEP 4: Apply the function to each variable
param_cols <- names(df_nw_clean)[names(df_nw_clean) != "treatment"]

group_tables <- lapply(param_cols, function(var) get_group_letters(df_nw_clean, var))

# STEP 5: Merge all group letters into one table
group_df <- Reduce(function(x, y) left_join(x, y, by = "treatment"), group_tables)

# STEP 6: Merge summary stats with HSD groupings
df_final <- df_reg %>%
  left_join(group_df, by = "treatment") %>%
  rename(Treatment = treatment)  # Optional: revert to original case

# STEP 7: Save to CSV
write.csv(df_final, "Final_table_with_groups.csv", row.names = FALSE)
