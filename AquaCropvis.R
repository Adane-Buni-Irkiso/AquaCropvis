
# Loading packages and function codes----
library(ggplot2)
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
setwd("set your working directory here, so that the files will be accessed")
read_aquacrop_output <- function(file_path, columns) {
  # Read all lines from the file
  lines <- read_lines(file_path, locale = locale(encoding = "latin1"))
  
  runs <- list()
  current_run <- list()
  
  for (line in lines) {
    if (startsWith(line, '** Run number:')) {
      if (length(current_run) > 0) {
        runs <- append(runs, list(current_run))
        current_run <- list()
      }
    } else if (nchar(trimws(line)) > 0 &&
               # Use any() to check if the line starts with any of the strings
               !any(startsWith(line, c('Day', 'AquaCrop', 'Soil', 'mm', 'm'))) &&
               !startsWith(line, '   -')) {
      parts <- strsplit(trimws(line), "\\s+")[[1]]
      if (length(parts) >= 5) {  # Ensure it's a data line
        current_run <- append(current_run, list(parts))
      }
    }
  }
  
  if (length(current_run) > 0) {
    runs <- append(runs, list(current_run))
  }
  
  dfs <- list()
  for (i in seq_along(runs)) {
    run_data <- runs[[i]]
    if (length(run_data) == 0) next
    
    # Create data frame
    df <- as.data.frame(do.call(rbind, run_data), stringsAsFactors = FALSE)
    col_count <- min(length(columns), ncol(df))
    names(df)[1:col_count] <- columns[1:col_count]
    
    # Convert numeric columns
    for (col in names(df)) {
      if (col != 'Stage') {  # Stage could be text
        df[[col]] <- as.numeric(df[[col]])
      }
    }
    
    df$Run <- i
    dfs <- append(dfs, list(df))
  }
  
  if (length(dfs) > 0) {
    return(bind_rows(dfs))
  } else {
    return(NULL)
  }
}

plot_var <- function(df, variable, plot_title = NULL, y_label = NULL, fig_width = 8, fig_height = 5) {
  if (is.null(df)) {
    message("No data available to plot.")
    return(invisible())
  }
  
  p <- ggplot(df, aes(x = DAP, y = .data[[variable]], group = Run)) +
    geom_line(aes(color = as.factor(Run)), size = 0.75) +
    labs(x = "Days After Planting (DAP)",
         y = ifelse(is.null(y_label), variable, y_label),
         title = ifelse(is.null(plot_title), paste(variable, "Over Time for Each Season"), plot_title),
         color = "Season") +
    theme_minimal() +
    theme(legend.position = "right",
          plot.title = element_text(hjust = 0.5),
          panel.grid = element_line(color = "grey90")) +
    scale_color_discrete(labels = function(x) {
      run_data <- df %>% filter(Run %in% x)
      if ("Year" %in% names(run_data) && nrow(run_data) > 0) {
        sapply(x, function(r) {
          years <- unique(na.omit(run_data$Year[run_data$Run == r]))
          if (length(years) == 0) return(paste("Season", r))
          if (length(years) == 1) return(as.character(years[1]))
          return(paste(min(years), max(years), sep = "/"))
        })
      } else {
        paste("Season", x)
      }
    })
  
  print(p)
}

plot_multivar <- function(df, run_num, variables, plot_title = NULL, y_label = NULL, fig_width = 8, fig_height = 5) {
  if (is.null(df)) {
    message("No data available to plot.")
    return(invisible())
  }
  
  run_data <- df %>% filter(Run == run_num)
  if (nrow(run_data) == 0) {
    message(paste("No data available for run", run_num))
    return(invisible())
  }
  
  # Prepare data for plotting multiple variables
  plot_data <- run_data %>%
    select(DAP, all_of(variables), Run) %>%
    pivot_longer(cols = all_of(variables), names_to = "Variable", values_to = "Value")
  
  p <- ggplot(plot_data, aes(x = DAP, y = Value, color = Variable)) +
    geom_line(size = 0.75) +
    labs(x = "Days After Planting (DAP)",
         y = ifelse(is.null(y_label), "Value", y_label),
         title = ifelse(is.null(plot_title), paste("Multiple Variables - Season", run_num), plot_title),
         color = "Variable") +
    theme_minimal() +
    theme(legend.position = "right",
          plot.title = element_text(hjust = 0.5),
          panel.grid = element_line(color = "grey90"))
  
  print(p)
}

plot_dual_axis <- function(df1, var1, df2, var2, run_num, title = NULL, y1_label = NULL, y2_label = NULL, fig_width = 8, fig_height = 5) {
  if (is.null(df1) || is.null(df2)) {
    message("One or both dataframes are not available.")
    return(invisible())
  }
  
  run_data1 <- df1 %>% filter(Run == run_num)
  run_data2 <- df2 %>% filter(Run == run_num)
  
  if (nrow(run_data1) == 0 || nrow(run_data2) == 0) {
    message(paste("No data available for run", run_num, "in one or both dataframes."))
    return(invisible())
  }
  
  # Get season year for the title
  season_text <- paste("Season", run_num) # Default text
  if ("Year" %in% names(run_data1) && nrow(run_data1) > 0) {
    years <- na.omit(unique(run_data1$Year))
    if (length(years) > 0) {
      years <- as.integer(years)
      if (length(years) == 1) {
        season_text <- paste("Season", run_num, "(", years[1], ")")
      } else {
        season_text <- paste("Season", run_num, "(", min(years), "-", max(years), ")")
      }
    }
  }
  
  # Create the plot
  p <- ggplot() +
    # First variable (left axis)
    geom_line(data = run_data1, aes(x = DAP, y = .data[[var1]], color = "forestgreen"), size = 1) +
    scale_y_continuous(name = ifelse(is.null(y1_label), var1, y1_label),
                       sec.axis = sec_axis(~., name = ifelse(is.null(y2_label), var2, y2_label))) +
    # Second variable (right axis)
    geom_line(data = run_data2, aes(x = DAP, y = .data[[var2]], color = "brown"), size = 1) +
    scale_color_identity(name = "",
                         breaks = c("forestgreen", "brown"),
                         labels = c(var1, var2),
                         guide = "legend") +
    labs(x = "Days After Planting (DAP)",
         title = ifelse(is.null(title), paste(var1, "and", var2, "-", season_text), title)) +
    theme_minimal() +
    theme(legend.position = "top",
          plot.title = element_text(hjust = 0.5),
          panel.grid = element_line(color = "grey90"),
          axis.title.y.left = element_text(color = "forestgreen"),
          axis.text.y.left = element_text(color = "forestgreen"),
          axis.title.y.right = element_text(color = "brown"),
          axis.text.y.right = element_text(color = "brown"))
  
  print(p)
}
# Example 1: Water balance components for single or multiple cropping seasons----
# Read the AquaCrop output file
wabal_df <- read_aquacrop_output('Example_Wabal.OUT', wabal_columns)  # Replace with your actual file path

# Plot the variables
plot_var(wabal_df, 'Trx', 'Maximum crop transpiration', 'Trx (mm)')
plot_var(wabal_df, 'Ex', 'Maximum soil evaporation', 'Ex (mm)')
plot_var(wabal_df, 'ETx', 'Maximum Evapotranspiration', 'ETx (mm)')
## Legend-------
# DAP          : Days after planting
# Stage        : 0: before/after planting
# Stage        : 1: emergence or transplant recovery
# Stage        : 2: vegetative stage
# Stage        : 3: flowering
# Stage        : 4: yield formation and ripening
# WCTot        : Water content in total soil profile
# Rain         : Rainfall
# Irri         : Water applied by irrigation
# Surf         : Stored water on soil surface between bunds
# Infilt       : Infiltrated water in soil profile
# RO           : Surface runoff
# Drain        : Water drained out of the soil profile
# CR           : Water moved upward by capillary rise
# Zgwt         : Depth of the groundwater table (-9.90 if absent)
# Ex           : Maximum soil evaporation
# E            : Soil evaporation
# E/Ex         : Relative soil evaporation (100 E/Ex)
# Trx          : Maximum crop transpiration
# Tr           : Total transpiration of crop and weeds
# Tr/Trx       : Relative transpiration (100 Tr/Trx)
# ETx          : Evapotranspiration maximale
# ET           : Evapotranspiration
# ET/ETx       : Evapotranspiration relative (100 ET/ETx)
# Example 2: Soil Water Content at a specific layer for single or multiple cropping seasons-----
wc_columns <- c('Day', 'Month', 'Year', 'DAP', 'Stage', 'WC1(vol%)', 'WC2(vol%)', 'WC3(vol%)',
                'WC4(vol%)', 'WC5(vol%)', 'WC6(vol%)', 'WC7(vol%)', 'WC8(vol%)', 'WC9(vol%)',
                'WC10(vol%)', 'WC11(vol%)', 'WC12(vol%)')
wc_df <- read_aquacrop_output('Example_CompWC.OUT', wc_columns)# Replace the file path ending with CompWC.OUT
plot_var(wc_df, 'WC2(vol%)', 'Soil Water Content at Layer X', 'Water Content (vol%)')

##  Legend------
# DAP          : Days after planting
# Stage        : 0: before/after planting
# Stage        : 1: emergence or transplant recovery
# Stage        : 2: vegetative stage
# Stage        : 3: flowering
# Stage        : 4: yield formation and ripening
# WC1(vol%) - WC12(vol%) : Water Content (WC) at different soil layers (1,2,3,4,5.....)
# Example 3 - Soil water content comparison at multiple soil layers per one cropping season----
soil_layers <- c('WC1(vol%)', 'WC3(vol%)', 'WC6(vol%)', 'WC9(vol%)', 'WC12(vol%)')
plot_multivar(wc_df, 4, soil_layers, 'Soil Water Content at Different Depths', 'Water Content (vol%)')# # Change the season/run number as desired(1,2,3.....corresponding to consecutive years (2015,2016,2017))

# # Example 4: Water content in total soil profile for single or multiple seasons---------
prof_columns <- c('Day', 'Month', 'Year', 'DAP', 'Stage', 'WCTot', 'Wr(Zx)',
                  'Z', 'Wr', 'Wr(SAT)', 'Wr(FC)', 'Wr(exp)', 'Wr(sto)',
                  'Wr(sen)', 'Wr(PWP)')
prof_df = read_aquacrop_output('Example_Prof.OUT', prof_columns) # Replace the file path ending with Prof.OUT
plot_var(prof_df, 'WCTot', 'Water content in total soil profile', 'Water content (mm)')

## Legend-----

# DAP          : Days after planting
# Stage        : 0: before/after planting
# Stage        : 1: emergence or transplant recovery
# Stage        : 2: vegetative stage
# Stage        : 3: flowering
# Stage        : 4: yield formation and ripening
# WCTot        : Water content in total soil profile (layer: 0.00 - 1.55 m)
# Wr(Zx)       : Water content in maximum effective root zone (Zx = 1.50 m)
# Z            : Effective rooting depth
# Wr           : Water content in effective root zone
# Wr(Sat)      : Water content in effective root zone if saturated
# Wr(FC)       : Water content in effective root zone at field capacity
# Wr(exp)      : Water content in effective root zone at upper threshold for leaf expansion
# Wr(sto)      : Water content in effective root zone at upper threshold for stomatal closure
# Wr(sen)      : Water content in effective root zone at upper threshold for early canopy senescence
# Wr(PWP)      : Water content in effective root zone at permanent wilting point

# Example 5: Crop growth outputs for single or multiple seasons ----

crop_columns <- c('Day', 'Month', 'Year', 'DAP', 'Stage', 'GD', 'Z', 'StExp', 'StSto', 'StSen',
                  'StSalt', 'StWeed', 'CC', 'CCw', 'StTr', 'Kc(Tr)', 'Trx', 'Tr', 'TrW', 'Tr/Trx',
                  'WP', 'Biomass', 'HI', 'YieldPart', 'Brelative', 'WPet')
crop_df = read_aquacrop_output('Example_Crop.OUT', crop_columns)# Replace the file path ending with Crop.OUT
plot_var(crop_df, 'Biomass', 'Cumulative biomass yield', 'Biomass (ton/ha)')
plot_var(crop_df, 'YieldPart', 'Cumulative grain yield', 'Grain yield (ton/ha)')
plot_var(crop_df, 'CC', ' Canopy Cover', 'Canopy Cover (%)')
## Legend----

# DAP          : Days after planting
# Stage        : 0: before/after planting
# Stage        : 1: emergence or transplant recovery
# Stage        : 2: vegetative stage
# Stage        : 3: flowering
# Stage        : 4: yield formation and ripening
# GD           : Growing degrees
# Z            : Effective rooting depth
# StExp        : Percent water stress reducing leaf expansion
# StSto        : Percent water stress inducing stomatal closure
# StSen        : Percent water stress triggering early canopy senescence
# StSalt       : Percent salinity stress
# StWeed       : Relative cover of weeds
# CC           : Green total Canopy Cover of crop and weeds
# CCw          : Green crop Canopy Cover in weed infested field
# StTr         : Percent temperature stress affecting crop transpiration
# Kc(Tr)       : Crop coefficient for transpiration
# Trx          : Maximum total transpiration of crop and weeds
# Tr           : Total transpiration of crop and weeds
# TrW          : Crop transpiration in weed infested field
# Tr/Trx       : Relative total transpiration of crop and weeds (100 Tr/Trx)
# WP           : Crop water productivity adjusted for CO2, soil fertility and products synthesized
# Biomass      : Cumulative crop biomass
# HI           : Harvest Index adjusted for failure of pollination, inadequate photosynthesis and water stress
# Yield Part   : Crop yield (HI x Biomass)
# Brelative    : Relative biomass (Reference: no water, no soil fertility, no soil salinity stress, no weed infestation)
# WPet         : ET Water productivity for yield part (kg yield produced per m3 water evapotranspired)
# Example 6: Visualizing relationships between multiple variables on a single plot ----
# This plots variables from two different dataframes on the same graph
plot_dual_axis(
  
  wabal_df, 'Rain',           # water balance component
  wc_df, 'WC1(vol%)',       # Specific layer soil moisture content
  # prof_df,'WCTot',            # Root zone or total soil profile
  run_num=1,                  # Change the season/run number as desired
  title='Transpiration and Soil Water Content Relationship',
  y1_label='Rainfall (mm)',
  y2_label='Soil Water Content (mm)')
