# plot multiple VIP
ggplot_imp <- function(...,ftype = 'bar') {
  
  # combine objects
  obj <- list(...)
  
  # retreive the metric name for x axis
  metric_name <- attr(obj[[1]], "loss_name")
  
  # extend the name
  metric_lab <- paste(metric_name, 
                      "after permutations\n(higher indicates more important)")
  
  # combine the data of the inputs
  full_vip <- rbindlist(obj)[variable != "_baseline_"]
 
  # rename the labels for heading figure
  full_vip[grepl('_xgb',label), label := 'XGBoost model']
  full_vip[grepl('_lm|_glm',label), label := 'GLM model']
  
  # rename variables
  full_vip[variable == 'temm', variable := 'temperature']
  full_vip[variable == 'ln_tn', variable := 'n-total']
  full_vip[variable == 'tp', variable := 'p-total']
  full_vip[variable == 'ratio', variable := 'norg fraction']
  full_vip[variable == 'ln_pre', variable := 'precipitation']
  full_vip[variable == 'ln_ak', variable := 'available k']
  full_vip[variable == 'ln_ap', variable := 'available p']
  full_vip[variable == 'ln_an', variable := 'available n']
  full_vip[variable == 'sshg', variable := 'sunshine hours']
  full_vip[variable == 'ln_soc', variable := 'soc']
  full_vip[variable == 'ln_tk', variable := 'k-total']
  full_vip[variable == 'ct1_P', variable := 'paddy soils']
  full_vip[variable == 'ct1_U', variable := 'upland soils']
  full_vip[variable == 'ct1_PU', variable := 'mixed soils']
  full_vip[variable == 'daynumber', variable := 'ndays rain events']
  full_vip[variable == 'tpi', variable := 'p input']
  full_vip[variable == 'tni', variable := 'n input']
  full_vip[variable == 'tki', variable := 'k input']
  full_vip[variable == 'tci', variable := 'c input']
  full_vip[variable == 'ysn', variable := 'duration']
  
  # estimate mean per model
  perm_vals <- full_vip[variable != '_full_model_',list(dropout_loss = mean(dropout_loss)),by='label']
  
  # estimate mean model and paramater
  parm_mean <- full_vip[variable != '_full_model_',list(dropout_loss = mean(dropout_loss)),
                        by=c('label','variable')]
  
  # order the variables
  p <- full_vip[variable != "_full_model_"]
  
 
  # sort the variables and make start plot
  p <- p[order(variable,-dropout_loss)] |> ggplot(aes(dropout_loss, variable)) 
  
  if(length(obj) > 1) {
    p <- p + 
      facet_wrap(vars(label)) +
      geom_vline(data = perm_vals, aes(xintercept = dropout_loss, color = label),
                 size = 1, lty = 2, alpha = 0.7)
      
      if(ftype == 'bar'){
        p <- p + geom_col(data = parm_mean,
                 aes(x = dropout_loss, y =variable, color = label,fill=label),alpha = 0.2)
        
      } else {
        p <- p + geom_boxplot(aes(color = label, fill = label), alpha = 0.2)  
      }
      
  } else {
    p <- p + 
      geom_vline(data = perm_vals, aes(xintercept = dropout_loss),
                 size = 1, lty = 2, alpha = 0.7) 
      if(ftype == 'bar'){
        p <- p + geom_col(data = parm_mean,
                 aes(x = dropout_loss, y =variable,fill="#91CBD765"),alpha = 0.2)
        
      } else {
        p <- p + geom_boxplot(fill = "#91CBD765", alpha = 0.4)  
      }
      
    
  }
  p <- p + 
    labs(x = metric_lab, 
         y = NULL,  fill = NULL,  color = NULL) + theme_bw() +
      theme(legend.position = "none",
            #panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()
            )
  
  
  return(p)
}


# plot multiple histograms
ggplot_hist <- function(...) {
  
  obj <- list(...)
  
  # make empty list
  out <- list()
  
  # convert all residual objects into one data.bele
  for(i in 1:length(obj)){
    
    out[[i]] <- data.table(label = levels(obj[[i]]$`_label_`),
                           residual = obj[[i]]$`_residuals_`)
    
  }
  out <- rbindlist(out)
  out <- out[order(label)]
  
  # make a residual plot
  if(length(obj) > 1){ 
    
    p <- ggplot(out,aes(x=residual,y=..density..,fill=label)) + 
         geom_histogram(position='identity')  +
         geom_density(aes(x=residual,y=..density..))+
         facet_wrap(~label,scales = "free_y")
    
  } else {
    
    p <- ggplot(out,aes(x=residual,y=..density..,fill=label)) + 
      geom_histogram(position='identity')  +
      geom_density(aes(x=residual,y=..density..))  
    
  }
  
  p <- p + theme_bw() + theme(legend.position = 'none') 
  
  
  return(p)
}

# plot 1 to 1 plots
# plot multiple histograms
ggplot_onetoone <- function(...) {
  
  obj <- list(...)
  
  # make empty list
  out <- list()
  
  # convert all residual objects into one data.bele
  for(i in 1:length(obj)){
    
    out[[i]] <- data.table(label = levels(obj[[i]]$`_label_`),
                           obs = obj[[i]]$`_y_`,
                           pred = obj[[i]]$`_y_hat_`)
    
  }
  out <- rbindlist(out)
  out <- out[order(label)]
  out[,label := gsub('_lm','_glm',label)]
  
  # make a residual plot
  if(length(obj) > 1){ 
    
    p <- ggplot(out,aes(x=obs,y=pred,col=label)) + 
      geom_point()  + geom_abline(intercept = 0,slope = 1,lty=2)+ 
      facet_wrap(~label)
    
  } else {
    
    p <- ggplot(out,aes(x=obs,y=pred,col=label)) + 
      geom_point()  + geom_abline(intercept = 0,slope = 1,lty=2)
  
  }
  
  p <- p + theme_bw() + theme(legend.position = 'none') + 
    ylab('Predicted NUE (scaled to unit variance)') + 
    xlab('Observed NUE (scaled to unit variance)')
  
  return(p)
}


ggplot_ale <- function(...){
  
  obj <- list(...)
  
  # make empty list
  out <- list()
  
  # convert all residual objects into one data.bele
  for(i in 1:length(obj)){
    
    out[[i]] <- data.table(parm = unique(obj[[i]]$`_vname_`),
                           label = unique(obj[[i]]$`_label_`),
                           y = obj[[i]]$`_yhat_`,
                           x = obj[[i]]$`_x_`)
    
  }
  out <- rbindlist(out)
  out <- out[order(label)]
  
  # remove log from the label
  out[,parm:= gsub('ln_','',parm)]
  
  # set axes
  xaxmax <- min(3,max(out$x))
  xaxmin <- max(-3,min(out$x))
  
  # make a ALE plot
  if(length(obj) > 1){ 
    
    p <- ggplot(out,aes(x=x,y=y,col=parm)) + 
      geom_point()  + geom_smooth()+
      facet_wrap(~label,scales='free')
    
  } else {
    
    p <- ggplot(out,aes(x=x,y=y,col=parm)) + 
      geom_point()  + geom_smooth()
    
  }
 
  p <- p + theme_bw() + theme(legend.position = 'bottom') + 
    ylab('Change in average predicted NUE\n(scaled to unit variance)') + 
    xlab('Change in explanatory variable (scaled to unit variance)') +
    xlim(xaxmin,xaxmax) +
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size=10))
  
  return(p)
}


ggplot_pdp <- function(obj, x) {
  
  
  out <- as.data.table(obj$agr_profiles)
  out[,label]
  p <- 
    as_tibble() %>%
    mutate(`_label_` = stringr::str_remove(`_label_`, "^[^_]*_")) %>%
    ggplot(aes(`_x_`, `_yhat_`)) +
    geom_line(data = as_tibble(obj$cp_profiles),
              aes(x = {{ x }}, group = `_ids_`),
              size = 0.5, alpha = 0.05, color = "gray50")
  
  num_colors <- n_distinct(obj$agr_profiles$`_label_`)
  
  if (num_colors > 1) {
    p <- p + geom_line(aes(color = `_label_`), size = 1.2, alpha = 0.8)
  } else {
    p <- p + geom_line(color = "midnightblue", size = 1.2, alpha = 0.8)
  }
  
  p
}