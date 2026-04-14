# Year9
This is the Cleaning for Year 5
title: "FFCWS Year 5 Threat Predictor Cleaning"
output: html_notebook
---

```{r}
###### Load Data
obj_name <- load("31622-0005-Data.rda")
if (length(obj_name) > 1) obj_name <- obj_name[1]
y5_raw <- get(obj_name)

print(getwd())
print(dim(y5_raw))
print(names(y5_raw)[1:50])
```

```{r}
###### Set Working Directory and Seed
try(setwd("C:/Users/<user>/Downloads/ICPSR_31622/DS0005"), silent = TRUE)
set.seed(1234)
```




```{r}
###### Select Variables
find_case_insensitive <- function(df, vars) {
  nm <- names(df)
  nm_low <- tolower(nm)
  out <- setNames(rep(NA_character_, length(vars)), vars)
  for (v in vars) {
    idx <- which(nm_low == tolower(v))
    if (length(idx) >= 1) out[v] <- nm[idx[1]]
  }
  out
}

ctspc_psych_vars <- c("p4g6","p4g10","p4g8","p4g14","p4g9")
ctspc_phys_vars  <- c("p4g7","p4g4","p4g11","p4g13","p4g3")
comm_viol_vars   <- c("p4h1","p4h2","p4h3","p4h4","p4h5","p4h6","p4h7")
neigh_threat_vars <- c("m4i0n5","m4i0o","o4p3")
ipv_required_vars <- c("m4d7h","m4d7i","m4d7n","m4d9h","m4d9i","m4d9n")
ipv_optional_vars <- c("m4d10","m4d10e")

admin_required_vars <- c("idnum")
admin_optional_vars <- c("ch4act5","ch4inttype_mod","cm4mint","cm4natsm","cm4citsm","p4natwt","p4citywt","m4natwt","m4citywt")

required_vars <- c(admin_required_vars, ctspc_psych_vars, ctspc_phys_vars, comm_viol_vars, neigh_threat_vars, ipv_required_vars)
optional_vars <- c(admin_optional_vars, ipv_optional_vars)

required_matched <- find_case_insensitive(y5_raw, required_vars)
optional_matched <- find_case_insensitive(y5_raw, optional_vars)

missing_required <- names(required_matched)[is.na(required_matched)]
if (length(missing_required) > 0) {
  stop(paste("Missing required variables:", paste(missing_required, collapse = ", ")))
}

vars_to_keep <- unique(c(unname(required_matched), unname(optional_matched[!is.na(optional_matched)])))
y5_clean <- y5_raw %>% dplyr::select(all_of(vars_to_keep))

all_canonical <- c(required_vars, optional_vars)
all_matched <- c(required_matched, optional_matched)
rename_map <- setNames(names(all_matched[!is.na(all_matched)]), unname(all_matched[!is.na(all_matched)]))
names(y5_clean) <- rename_map[names(y5_clean)]
```

```{r}
###### Rename Variables
rename_lookup <- c(
  # Admin
  idnum = "id",
  ch4act5 = "any_y5_activity_component",
  ch4inttype_mod = "y5_inhome_completion_status",
  cm4mint = "mother_interviewed_y5",
  cm4natsm = "mother_national_sample_flag_y5",
  cm4citsm = "mother_city_sample_flag_y5",
  p4natwt = "pcg_y5_national_weight",
  p4citywt = "pcg_y5_city_weight",
  m4natwt = "mother_y5_national_weight",
  m4citywt = "mother_y5_city_weight",

  # CTSPC psychological aggression (corrected)
  p4g6 = "pcg_ctspc_shouted_yelled_or_screamed",
  p4g10 = "pcg_ctspc_threatened_to_spank_hit",
  p4g8 = "pcg_ctspc_swear_curse_at_child",
  p4g14 = "pcg_ctspc_called_child_dumb_lazy_or_other_name",
  p4g9 = "pcg_ctspc_said_send_away_or_kick_out",

  # CTSPC physical assault (corrected)
  p4g7 = "pcg_ctspc_spanked_bottom_with_bare_hand",
  p4g4 = "pcg_ctspc_hit_bottom_with_hard_object",
  p4g11 = "pcg_ctspc_slapped_hand_arm_leg",
  p4g13 = "pcg_ctspc_pinched_child",
  p4g3 = "pcg_ctspc_shook_child",

  # Community violence (corrected)
  p4h1 = "pcg_commviol_saw_someone_hit_beaten_up",
  p4h2 = "pcg_commviol_child_hit_beaten_up_by_someone",
  p4h3 = "pcg_commviol_saw_someone_attacked_with_weapon",
  p4h4 = "pcg_commviol_child_attacked_with_weapon",
  p4h5 = "pcg_commviol_saw_someone_shot",
  p4h6 = "pcg_commviol_child_shot_at",
  p4h7 = "pcg_commviol_saw_someone_killed_by_violence",

  # Neighborhood threat
  m4i0n5 = "mother_neigh_gangs_problem",
  m4i0o = "mother_afraid_child_outside_due_to_violence",
  o4p3 = "obs_graffiti_on_block",

  # IPV
  m4d7h = "mother_partner_slap_kick_hit_current",
  m4d7i = "mother_partner_hit_with_fist_or_object_current",
  m4d7n = "mother_partner_push_grab_shove_current",
  m4d9h = "mother_partner_slap_kick_hit_past",
  m4d9i = "mother_partner_hit_with_fist_or_object_past",
  m4d9n = "mother_partner_push_grab_shove_past",
  m4d10 = "mother_partner_forced_sex",
  m4d10e = "mother_partner_prevented_from_leaving"
)

present_old <- intersect(names(rename_lookup), names(y5_clean))
rename_spec <- setNames(present_old, rename_lookup[present_old])
y5_clean <- y5_clean %>% dplyr::rename(!!!rename_spec)
```

```{r}
###### Raw Response Patterns
ctspc_psych_items <- c(
  "pcg_ctspc_shouted_yelled_or_screamed",
  "pcg_ctspc_threatened_to_spank_hit",
  "pcg_ctspc_swear_curse_at_child",
  "pcg_ctspc_called_child_dumb_lazy_or_other_name",
  "pcg_ctspc_said_send_away_or_kick_out"
)

ctspc_phys_items <- c(
  "pcg_ctspc_spanked_bottom_with_bare_hand",
  "pcg_ctspc_hit_bottom_with_hard_object",
  "pcg_ctspc_slapped_hand_arm_leg",
  "pcg_ctspc_pinched_child",
  "pcg_ctspc_shook_child"
)

comm_viol_items <- c(
  "pcg_commviol_saw_someone_hit_beaten_up",
  "pcg_commviol_child_hit_beaten_up_by_someone",
  "pcg_commviol_saw_someone_attacked_with_weapon",
  "pcg_commviol_child_attacked_with_weapon",
  "pcg_commviol_saw_someone_shot",
  "pcg_commviol_child_shot_at",
  "pcg_commviol_saw_someone_killed_by_violence"
)

neigh_threat_items <- c(
  "mother_neigh_gangs_problem",
  "mother_afraid_child_outside_due_to_violence",
  "obs_graffiti_on_block"
)

ipv_required_items <- c(
  "mother_partner_slap_kick_hit_current",
  "mother_partner_hit_with_fist_or_object_current",
  "mother_partner_push_grab_shove_current",
  "mother_partner_slap_kick_hit_past",
  "mother_partner_hit_with_fist_or_object_past",
  "mother_partner_push_grab_shove_past"
)

ipv_optional_items <- intersect(
  c("mother_partner_forced_sex", "mother_partner_prevented_from_leaving"),
  names(y5_clean)
)

ipv_items <- c(ipv_required_items, ipv_optional_items)
all_items <- c(ctspc_psych_items, ctspc_phys_items, comm_viol_items, neigh_threat_items, ipv_items)

for (v in all_items) {
  cat("\n", v, "\n", sep = "")
  print(table(y5_clean[[v]], useNA = "ifany"))
}
```

```{r}
###### Recode Missing/Illogical Values
for (v in all_items) {
  y5_clean[[paste0(v, "_code")]] <- haven::zap_labels(y5_clean[[v]])
}

clean_item_numeric <- function(x) {
  x <- haven::zap_labels(x)
  readr::parse_number(as.character(x))
}

y5_clean %<>% mutate_at(vars(one_of(all_items)), clean_item_numeric)

for (v in all_items) {
  y5_clean[[paste0(v, "_imp_eligible")]] <- y5_clean[[paste0(v, "_code")]] %in% c(-1, -2, -3, -4, -8)
}

# CTSPC valid 0:7
y5_clean <- naniar::replace_with_na_at(y5_clean, .vars = ctspc_psych_items, condition = ~ .x < 0)
y5_clean <- naniar::replace_with_na_at(y5_clean, .vars = ctspc_psych_items, condition = ~ .x > 7)
y5_clean <- naniar::replace_with_na_at(y5_clean, .vars = ctspc_phys_items, condition = ~ .x < 0)
y5_clean <- naniar::replace_with_na_at(y5_clean, .vars = ctspc_phys_items, condition = ~ .x > 7)

# Community violence valid 0:4
y5_clean <- naniar::replace_with_na_at(y5_clean, .vars = comm_viol_items, condition = ~ .x < 0)
y5_clean <- naniar::replace_with_na_at(y5_clean, .vars = comm_viol_items, condition = ~ .x > 4)

# Neighborhood valid ranges
y5_clean <- naniar::replace_with_na_at(y5_clean, .vars = "mother_neigh_gangs_problem", condition = ~ .x < 1)
y5_clean <- naniar::replace_with_na_at(y5_clean, .vars = "mother_neigh_gangs_problem", condition = ~ .x > 4)
y5_clean <- naniar::replace_with_na_at(y5_clean, .vars = "mother_afraid_child_outside_due_to_violence", condition = ~ .x < 1)
y5_clean <- naniar::replace_with_na_at(y5_clean, .vars = "mother_afraid_child_outside_due_to_violence", condition = ~ .x > 2)
y5_clean <- naniar::replace_with_na_at(y5_clean, .vars = "obs_graffiti_on_block", condition = ~ .x < 1)
y5_clean <- naniar::replace_with_na_at(y5_clean, .vars = "obs_graffiti_on_block", condition = ~ .x > 4)

# IPV required aggression valid 1:3
y5_clean <- naniar::replace_with_na_at(y5_clean, .vars = ipv_required_items, condition = ~ .x < 1)
y5_clean <- naniar::replace_with_na_at(y5_clean, .vars = ipv_required_items, condition = ~ .x > 3)

# Optional IPV binary valid 1:2 (if present)
if ("mother_partner_forced_sex" %in% names(y5_clean)) {
  y5_clean <- naniar::replace_with_na_at(y5_clean, .vars = "mother_partner_forced_sex", condition = ~ .x < 1)
  y5_clean <- naniar::replace_with_na_at(y5_clean, .vars = "mother_partner_forced_sex", condition = ~ .x > 2)
}
if ("mother_partner_prevented_from_leaving" %in% names(y5_clean)) {
  y5_clean <- naniar::replace_with_na_at(y5_clean, .vars = "mother_partner_prevented_from_leaving", condition = ~ .x < 1)
  y5_clean <- naniar::replace_with_na_at(y5_clean, .vars = "mother_partner_prevented_from_leaving", condition = ~ .x > 2)
}
```

```{r}
###### Post-clean 
for (v in all_items) {
  cat("\nCLEANED:", v, "\n")
  print(table(y5_clean[[v]], useNA = "ifany"))
}

for (v in all_items) {
  vc <- paste0(v, "_code")
  cat("\nRAW CODE:", vc, "\n")
  print(table(y5_clean[[vc]], useNA = "ifany"))
}

y5_clean_noMI <- y5_clean
```

```{r}
###### Score Items
score_and_summarize <- function(data_in) {
  d <- data_in

  for (v in all_items) {
    d[[paste0(v, "_sc")]] <- d[[v]]
  }

  # CTSPC
  for (v in c(ctspc_psych_items, ctspc_phys_items)) {
    sc <- paste0(v, "_sc")
    ord <- paste0(v, "_ord")
    mid <- paste0(v, "_mid")
    any <- paste0(v, "_any")

    d[[ord]] <- dplyr::if_else(d[[sc]] %in% 0:7, d[[sc]], NA_real_)

    d[[mid]] <- dplyr::case_when(
      is.na(d[[ord]]) ~ NA_real_,
      d[[ord]] == 0 ~ 0,
      d[[ord]] == 1 ~ 1,
      d[[ord]] == 2 ~ 2,
      d[[ord]] == 3 ~ 4,
      d[[ord]] == 4 ~ 8,
      d[[ord]] == 5 ~ 15,
      d[[ord]] == 6 ~ 25,
      d[[ord]] == 7 ~ 0,
      TRUE ~ NA_real_
    )

    d[[any]] <- dplyr::case_when(
      is.na(d[[ord]]) ~ NA_real_,
      d[[ord]] == 0 ~ 0,
      d[[ord]] %in% 1:6 ~ 1,
      d[[ord]] == 7 ~ 0,
      TRUE ~ NA_real_
    )
  }

  psych_mid_vars <- paste0(ctspc_psych_items, "_mid")
  phys_mid_vars  <- paste0(ctspc_phys_items, "_mid")
  psych_any_vars <- paste0(ctspc_psych_items, "_any")
  phys_any_vars  <- paste0(ctspc_phys_items, "_any")

  d$ctspc_psych_mid_sum <- rowSums(d[, psych_mid_vars], na.rm = TRUE)
  d$ctspc_phys_mid_sum  <- rowSums(d[, phys_mid_vars], na.rm = TRUE)
  d$ctspc_psych_any_sum <- rowSums(d[, psych_any_vars], na.rm = TRUE)
  d$ctspc_phys_any_sum  <- rowSums(d[, phys_any_vars], na.rm = TRUE)

  # Community violence
  for (v in comm_viol_items) {
    sc <- paste0(v, "_sc")
    ord <- paste0(v, "_ord")
    any <- paste0(v, "_any")

    d[[ord]] <- dplyr::if_else(d[[sc]] %in% 0:4, d[[sc]], NA_real_)

    d[[any]] <- dplyr::case_when(
      is.na(d[[ord]]) ~ NA_real_,
      d[[ord]] == 0 ~ 0,
      d[[ord]] %in% 1:4 ~ 1,
      TRUE ~ NA_real_
    )
  }

  comm_ord_vars <- paste0(comm_viol_items, "_ord")
  comm_any_vars <- paste0(comm_viol_items, "_any")

  d$community_violence_any_count <- rowSums(d[, comm_any_vars], na.rm = TRUE)
  d$community_violence_ord_sum   <- rowSums(d[, comm_ord_vars], na.rm = TRUE)

  # Neighborhood threat
  d$mother_neigh_gangs_problem_threat <- dplyr::case_when(
    d$mother_neigh_gangs_problem == 1 ~ 3,
    d$mother_neigh_gangs_problem == 2 ~ 2,
    d$mother_neigh_gangs_problem == 3 ~ 1,
    d$mother_neigh_gangs_problem == 4 ~ 0,
    TRUE ~ NA_real_
  )

  d$mother_afraid_child_outside_due_to_violence_threat <- dplyr::case_when(
    d$mother_afraid_child_outside_due_to_violence == 1 ~ 1,
    d$mother_afraid_child_outside_due_to_violence == 2 ~ 0,
    TRUE ~ NA_real_
  )

  d$obs_graffiti_on_block_threat <- dplyr::case_when(
    d$obs_graffiti_on_block == 1 ~ 0,
    d$obs_graffiti_on_block == 2 ~ 1,
    d$obs_graffiti_on_block == 3 ~ 2,
    d$obs_graffiti_on_block == 4 ~ 3,
    TRUE ~ NA_real_
  )

  d$neighborhood_threat_sum <- rowSums(
    d[, c(
      "mother_neigh_gangs_problem_threat",
      "mother_afraid_child_outside_due_to_violence_threat",
      "obs_graffiti_on_block_threat"
    )],
    na.rm = TRUE
  )

  # IPV required items
  for (v in ipv_required_items) {
    d[[paste0(v, "_rev")]] <- dplyr::case_when(
      d[[v]] == 1 ~ 2,
      d[[v]] == 2 ~ 1,
      d[[v]] == 3 ~ 0,
      TRUE ~ NA_real_
    )

    d[[paste0(v, "_any")]] <- dplyr::case_when(
      d[[v]] %in% c(1, 2) ~ 1,
      d[[v]] == 3 ~ 0,
      TRUE ~ NA_real_
    )
  }

if ("mother_partner_forced_sex" %in% names(d)) {
    d$mother_partner_forced_sex_bin <- dplyr::case_when(
      d$mother_partner_forced_sex == 1 ~ 1,
      d$mother_partner_forced_sex == 2 ~ 0,
      TRUE ~ NA_real_
    )
  }

  if ("mother_partner_prevented_from_leaving" %in% names(d)) {
    d$mother_partner_prevented_from_leaving_bin <- dplyr::case_when(
      d$mother_partner_prevented_from_leaving == 1 ~ 1,
      d$mother_partner_prevented_from_leaving == 2 ~ 0,
      TRUE ~ NA_real_
    )
  }

  d$ipv_slapkick_rev <- dplyr::coalesce(
    d$mother_partner_slap_kick_hit_current_rev,
    d$mother_partner_slap_kick_hit_past_rev
  )
  d$ipv_hitobject_rev <- dplyr::coalesce(
    d$mother_partner_hit_with_fist_or_object_current_rev,
    d$mother_partner_hit_with_fist_or_object_past_rev
  )
  d$ipv_pushshove_rev <- dplyr::coalesce(
    d$mother_partner_push_grab_shove_current_rev,
    d$mother_partner_push_grab_shove_past_rev
  )

  d$ipv_slapkick_any <- dplyr::coalesce(
    d$mother_partner_slap_kick_hit_current_any,
    d$mother_partner_slap_kick_hit_past_any
  )
  d$ipv_hitobject_any <- dplyr::coalesce(
    d$mother_partner_hit_with_fist_or_object_current_any,
    d$mother_partner_hit_with_fist_or_object_past_any
  )
  d$ipv_pushshove_any <- dplyr::coalesce(
    d$mother_partner_push_grab_shove_current_any,
    d$mother_partner_push_grab_shove_past_any
  )

  rev_sum_vars <- c("ipv_slapkick_rev", "ipv_hitobject_rev", "ipv_pushshove_rev")
  any_sum_vars <- c("ipv_slapkick_any", "ipv_hitobject_any", "ipv_pushshove_any")
  if ("mother_partner_forced_sex_bin" %in% names(d)) any_sum_vars <- c(any_sum_vars, "mother_partner_forced_sex_bin")
  if ("mother_partner_prevented_from_leaving_bin" %in% names(d)) any_sum_vars <- c(any_sum_vars, "mother_partner_prevented_from_leaving_bin")

  d$ipv_rev_sum <- rowSums(d[, rev_sum_vars], na.rm = TRUE)
  d$ipv_any_sum <- rowSums(d[, any_sum_vars], na.rm = TRUE)
    # Optional IPV binary items
  if ("mother_partner_forced_sex" %in% names(d)) {
    d$mother_partner_forced_sex_bin <- dplyr::case_when(
      d$mother_partner_forced_sex == 1 ~ 1,
      d$mother_partner_forced_sex == 2 ~ 0,
      TRUE ~ NA_real_
    )
  }

  if ("mother_partner_prevented_from_leaving" %in% names(d)) {
    d$mother_partner_prevented_from_leaving_bin <- dplyr::case_when(
      d$mother_partner_prevented_from_leaving == 1 ~ 1,
      d$mother_partner_prevented_from_leaving == 2 ~ 0,
      TRUE ~ NA_real_
    )
  }

  d$ipv_slapkick_rev <- dplyr::coalesce(
    d$mother_partner_slap_kick_hit_current_rev,
    d$mother_partner_slap_kick_hit_past_rev
  )
  d$ipv_hitobject_rev <- dplyr::coalesce(
    d$mother_partner_hit_with_fist_or_object_current_rev,
    d$mother_partner_hit_with_fist_or_object_past_rev
  )
  d$ipv_pushshove_rev <- dplyr::coalesce(
    d$mother_partner_push_grab_shove_current_rev,
    d$mother_partner_push_grab_shove_past_rev
  )

  d$ipv_slapkick_any <- dplyr::coalesce(
    d$mother_partner_slap_kick_hit_current_any,
    d$mother_partner_slap_kick_hit_past_any
  )
  d$ipv_hitobject_any <- dplyr::coalesce(
    d$mother_partner_hit_with_fist_or_object_current_any,
    d$mother_partner_hit_with_fist_or_object_past_any
  )
  d$ipv_pushshove_any <- dplyr::coalesce(
    d$mother_partner_push_grab_shove_current_any,
    d$mother_partner_push_grab_shove_past_any
  )

  rev_sum_vars <- c("ipv_slapkick_rev", "ipv_hitobject_rev", "ipv_pushshove_rev")
  any_sum_vars <- c("ipv_slapkick_any", "ipv_hitobject_any", "ipv_pushshove_any")
  if ("mother_partner_forced_sex_bin" %in% names(d)) any_sum_vars <- c(any_sum_vars, "mother_partner_forced_sex_bin")
  if ("mother_partner_prevented_from_leaving_bin" %in% names(d)) any_sum_vars <- c(any_sum_vars, "mother_partner_prevented_from_leaving_bin")

  d$ipv_rev_sum <- rowSums(d[, rev_sum_vars], na.rm = TRUE)
  d$ipv_any_sum <- rowSums(d[, any_sum_vars], na.rm = TRUE)

  # Missingness summary counts
  d$n_nonmiss_ctspc_psych <- rowSums(!is.na(d[, paste0(ctspc_psych_items, "_ord")]))
  d$n_nonmiss_ctspc_phys  <- rowSums(!is.na(d[, paste0(ctspc_phys_items, "_ord")]))
  d$n_nonmiss_comm_viol   <- rowSums(!is.na(d[, paste0(comm_viol_items, "_ord")]))
  d$n_nonmiss_neigh_threat <- rowSums(!is.na(d[, c(
    "mother_neigh_gangs_problem_threat",
    "mother_afraid_child_outside_due_to_violence_threat",
    "obs_graffiti_on_block_threat"
  )]))
  d$n_nonmiss_ipv_rev <- rowSums(!is.na(d[, rev_sum_vars, drop = FALSE]))
  d$n_nonmiss_ipv_any <- rowSums(!is.na(d[, any_sum_vars, drop = FALSE]))

  # Set summary scores to NA when all subconstruct items are missing
  d$ctspc_psych_mid_sum[d$n_nonmiss_ctspc_psych == 0] <- NA_real_
  d$ctspc_phys_mid_sum[d$n_nonmiss_ctspc_phys == 0] <- NA_real_
  d$ctspc_psych_any_sum[d$n_nonmiss_ctspc_psych == 0] <- NA_real_
  d$ctspc_phys_any_sum[d$n_nonmiss_ctspc_phys == 0] <- NA_real_

  d$community_violence_any_count[d$n_nonmiss_comm_viol == 0] <- NA_real_
  d$community_violence_ord_sum[d$n_nonmiss_comm_viol == 0] <- NA_real_

  d$neighborhood_threat_sum[d$n_nonmiss_neigh_threat == 0] <- NA_real_

  d$ipv_rev_sum[d$n_nonmiss_ipv_rev == 0] <- NA_real_
  d$ipv_any_sum[d$n_nonmiss_ipv_any == 0] <- NA_real_

  d
}
```


```{r}
score_and_summarize <- function(data_in) {
  d <- data_in

  # earlier scoring code...

  # Missingness summary counts
  d$n_nonmiss_ctspc_psych <- rowSums(!is.na(d[, paste0(ctspc_psych_items, "_ord")]))
  d$n_nonmiss_ctspc_phys  <- rowSums(!is.na(d[, paste0(ctspc_phys_items, "_ord")]))
  d$n_nonmiss_comm_viol   <- rowSums(!is.na(d[, paste0(comm_viol_items, "_ord")]))
  d$n_nonmiss_neigh_threat <- rowSums(!is.na(d[, c(
    "mother_neigh_gangs_problem_threat",
    "mother_afraid_child_outside_due_to_violence_threat",
    "obs_graffiti_on_block_threat"
  )]))
  d$n_nonmiss_ipv_rev <- rowSums(!is.na(d[, rev_sum_vars, drop = FALSE]))
  d$n_nonmiss_ipv_any <- rowSums(!is.na(d[, any_sum_vars, drop = FALSE]))

  d$ctspc_psych_mid_sum[d$n_nonmiss_ctspc_psych == 0] <- NA_real_
  d$ctspc_phys_mid_sum[d$n_nonmiss_ctspc_phys == 0] <- NA_real_
  d$ctspc_psych_any_sum[d$n_nonmiss_ctspc_psych == 0] <- NA_real_
  d$ctspc_phys_any_sum[d$n_nonmiss_ctspc_phys == 0] <- NA_real_

  d$community_violence_any_count[d$n_nonmiss_comm_viol == 0] <- NA_real_
  d$community_violence_ord_sum[d$n_nonmiss_comm_viol == 0] <- NA_real_

  d$neighborhood_threat_sum[d$n_nonmiss_neigh_threat == 0] <- NA_real_

  d$ipv_rev_sum[d$n_nonmiss_ipv_rev == 0] <- NA_real_
  d$ipv_any_sum[d$n_nonmiss_ipv_any == 0] <- NA_real_

  d
}
```

```{r}
y5_scored_noMI <- score_and_summarize(y5_clean_noMI)
y5_clean <- y5_scored_noMI
```

```{r}
###### Descriptives + Alpha Table
summary_vars <- c(
  "ctspc_psych_mid_sum",
  "ctspc_phys_mid_sum",
  "community_violence_ord_sum",
  "neighborhood_threat_sum",
  "ipv_rev_sum"
)

desc_table <- y5_clean %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(summary_vars),
      list(
        n = ~ sum(!is.na(.x)),
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        min = ~ suppressWarnings(min(.x, na.rm = TRUE)),
        max = ~ suppressWarnings(max(.x, na.rm = TRUE))
      ),
      .names = "{.col}_{.fn}"
    )
  )
print(desc_table)

alpha_safe <- function(df, vars, construct_name) {
  tmp <- df %>% dplyr::select(dplyr::all_of(vars))
  if (ncol(tmp) < 2) {
    return(tibble(construct = construct_name, alpha = NA_real_, n_items = ncol(tmp), n_complete = NA_integer_))
  }

  complete_n <- sum(stats::complete.cases(tmp))
  out <- try(psych::alpha(tmp, warnings = FALSE, check.keys = FALSE), silent = TRUE)
  if (inherits(out, "try-error")) {
    tibble(construct = construct_name, alpha = NA_real_, n_items = ncol(tmp), n_complete = complete_n)
  } else {
    tibble(construct = construct_name, alpha = unname(out$total$raw_alpha), n_items = ncol(tmp), n_complete = complete_n)
  }
}

alpha_table <- dplyr::bind_rows(
  alpha_safe(y5_clean, paste0(ctspc_psych_items, "_mid"), "CTSPC psych"),
  alpha_safe(y5_clean, paste0(ctspc_phys_items, "_mid"), "CTSPC physical"),
  alpha_safe(y5_clean, paste0(comm_viol_items, "_ord"), "Community violence"),
  alpha_safe(y5_clean, c("mother_neigh_gangs_problem_threat", "mother_afraid_child_outside_due_to_violence_threat", "obs_graffiti_on_block_threat"), "Neighborhood threat"),
  alpha_safe(y5_clean, c("ipv_slapkick_rev", "ipv_hitobject_rev", "ipv_pushshove_rev"), "IPV rev")
)

print(alpha_table)
```







```{r}
###### Simple Plots/Histograms
ggplot(y5_clean, aes(x = ctspc_psych_mid_sum)) +
  geom_histogram(bins = 30)

ggplot(y5_clean, aes(x = ctspc_phys_mid_sum)) +
  geom_histogram(bins = 30)

ggplot(y5_clean, aes(x = community_violence_ord_sum)) +
  geom_histogram(bins = 30)

ggplot(y5_clean, aes(x = neighborhood_threat_sum)) +
  geom_histogram(bins = 30)

ggplot(y5_clean, aes(x = ipv_rev_sum)) +
  geom_histogram(bins = 30)

ggplot(y5_clean, aes(x = ctspc_psych_mid_sum, y = ctspc_phys_mid_sum)) +
  geom_point()

ggplot(y5_clean, aes(x = community_violence_ord_sum, y = neighborhood_threat_sum)) +
  geom_point()

ggplot(y5_clean, aes(x = ipv_rev_sum, y = ctspc_phys_mid_sum)) +
  geom_point()
```
