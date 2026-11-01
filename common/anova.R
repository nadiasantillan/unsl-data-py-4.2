library(ggplot2)
library(dplyr)

levels_plot <- function(df, var_value, var_x, var_mean, var_sd, var_subgroup, 
                        title="", label_x="", label_y="") {
  df[,var_x] <- reorder(df[,var_x], df[,var_value], FUN = mean)
  
  df_summary <- df %>%
    group_by(.data[[var_x]], .data[[var_subgroup]]) %>%
    summarise(m = mean(.data[[var_value]]), s = sd(.data[[var_value]]))
  ggplot()+ 
    geom_line(data=df_summary, 
              aes(x=.data[[var_x]], y=.data[[var_mean]], color=.data[[var_subgroup]], group=.data[[var_subgroup]]))+   
    geom_point(data=df_summary, 
               aes( y=.data[[var_mean]], x=.data[[var_x]], color=.data[[var_subgroup]]))+
    geom_jitter(data=df, 
               aes( y=.data[[var_value]], x=.data[[var_x]], color=.data[[var_subgroup]]), width = 0.05)+
    geom_errorbar(data=df_summary, 
                  aes( ymin=.data[[var_mean]]-.data[[var_sd]], ymax=.data[[var_mean]]+.data[[var_sd]], x=.data[[var_x]], color=.data[[var_subgroup]]), 
                  width=.1)+
    labs(y=label_y, x=label_x, title = title)+ 
    theme_classic()
}
