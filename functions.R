# functions

# extract data ----------
extractr <- function(path, contains = NULL, month = NULL) {
  
  if (is.null(contains)) {
    f <- list.files(path)
  } else {
    f <- list.files(path)[str_detect(list.files(path), contains)]
  }
  
  if (!is.null(month)) {
    f <- f[str_detect(f, month)]
  }
  
  message(paste0("There are ", length(f), " files"))
  
  for (i in seq_along(f)) {
    
    if (i == 1) {
      
      tmp_out <- suppressMessages(vroom(paste0(path, "/", f[i])))
      
    } else {
      
      print(i)
      tmp_out <- rbind(tmp_out, suppressMessages(vroom(paste0(path, "/", f[i]), col_types = cols())))
      
    }
    
    
  }
  
  return(tmp_out %>%
           clean_names() %>%
           dplyr::rename(
             id = participant_research_id
           ) %>%
           dplyr::mutate(
             id = as.character(id)
           ) %>%
           dplyr::select(
             -c(source_identifier, device_manufacturer, device_hardware_version, device_software_version, source_version, device_model)
           ))
  
}

survyr <- function(path, contains = NULL, month = NULL) {
  
    if (is.null(contains)) {
      f <- list.files(path)
    } else {
      f <- list.files(path)[str_detect(list.files(path), contains)]
    }
    
    if (!is.null(month)) {
      f <- f[str_detect(f, month)]
    }
    
    message(paste0("There are ", length(f), " files"))
    
    for (i in seq_along(f)) {
      
      if (i == 1) {
        
        tmp_out <- suppressMessages(vroom(paste0(path, "/", f[i])))
        
      } else {
        
        print(i)
        tmp_out <- rbind(tmp_out, suppressMessages(vroom(paste0(path, "/", f[i]), col_types = cols())))
        
      }
      
      
    }
    
    return(tmp_out %>%
             clean_names() %>%
             dplyr::rename(
               id = participant_research_id
             ) %>%
             dplyr::mutate(
               id = as.character(id)
             ) %>%
             dplyr::select(
               -c(survey_key, survey_version, survey_step_type, survey_due_date,
                  survey_end_date, survey_question_start_date, survey_question_end_date)))
  
}

grab_demo_survey <- function(x) {
  
  for (i in seq_along(x)) {
    
    if (i == 1) {
      tmp_out <- suppressMessages(vroom(glue("{survey_path}/Surveys_{x[i]}.csv")))
    } else {
      tmp_out <- bind_rows(tmp_out, suppressMessages(vroom(glue("{survey_path}/Surveys_{x[i]}.csv"))))
    }
    return(tmp_out %>%
             janitor::clean_names() %>%
             dplyr::filter(survey_name == "Baseline Demographic Status - Intake") %>%
             dplyr::select(
               -c(survey_key, survey_version, survey_step_type, survey_due_date,
                  survey_end_date, survey_question_start_date, survey_question_end_date)))
    
  }
}

# quick numeric summary ------------
quick_sum <- function(x, var, group_id = FALSE) {
  
  if (group_id == FALSE) {
  x %>%
    dplyr::select(id, {{ var }}) %>%
    distinct() %>%
    summary()
  } else {
  x %>%
    dplyr::select(id, {{ var }}) %>%
    group_by(id) %>%
    summarize(
      id_mean = mean({{ var }}, na.rm = T)
    ) %>%
    pull(id_mean) %>%
    summary()
  }
  
}

# likelihood ratio test ----------
lrt_test <- function(m1, m2, df = 3) {
  
  tmp_a <- logLik(m1)
  tmp_b <- logLik(m2)
  
  tmp_stat <- -2 * (as.numeric(tmp_a)-as.numeric(tmp_b))
  
  tmp_p <- pchisq(tmp_stat, df = df, lower.tail = FALSE)
  
  list(
    m1_logLik = tmp_a,
    m2_logLik = tmp_b,
    test_stat = tmp_stat,
    p_value   = tmp_p
  )
  
}
# ggplot theme -----------
theme_max <- theme_minimal() +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle   = element_text(hjust = 0.5, color = "gray40"),
    axis.title      = element_text(face = "italic"),
    panel.grid      = element_blank(),
    axis.line       = element_line(color = "black", size = 0.5),
    legend.position = "top",
    legend.title    = element_blank()
  )

theme_set(theme_max)