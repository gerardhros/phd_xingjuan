# preparation NUE analysis 

  # clear environment
  rm(list=ls())
  
  # load packages
  require(data.table);require(ggplot2)
  library(mlr3)
  library(mltools);library(xgboost);library(mlr3);library(mlr3learners)
  library(mlr3measures); library(mlr3tuning);library(paradox);library(DALEX);library(ggplot2)
  library(mlr3hyperband);library(emoa)
  
  # source plot functions
  source('scripts/220627 plotfuns.R')
  
  #  load input data (i changed in excel by adding a '2' to duplicated column names)
  d1 <- fread('data/220627_jinxian_npk_climate.csv',dec=',')
  
  # make columns with numbers also numeric
  cols.num <- c('NUEagro','Ysn','Yield',"Noutput",'pH','SOC','TN','AN','TP','AP','TK','AK',
                'Clay','Silt','Sand','TCI','TPI','TKI','TNI','Ratio','PREg','Daynumber','TEMm','SSHg')
  d1[,c(cols.num) := lapply(.SD,as.numeric),.SDcols = cols.num]
  
  # remove irrelevant variables
  cols <- colnames(d1)[grepl('^log_',colnames(d1))]
  d1[,c(cols) := NULL]
  
  # setnames to lower case to simplify programming
  setnames(d1,tolower(colnames(d1)))

  # data checks and transformations
  
    # apply a log-transformation on a few variables
    d1[,ln_nue := log(nueagro+10)]
    d1[,ln_soc := log(soc)]
    d1[,ln_tn := log(tn+1)]
    d1[,ln_an := log(an)]
    d1[,ln_tk := log(tk)]
    d1[,ln_ak := log(ak)]
    d1[,ln_pre := log(preg)]
    d1[,ln_ap:= log(ap)]
    
    # apply inverse transformation
    d1[,inv_tp := 1/tp]
    
    # add categorical variables for type of fertilization
    d1[,fs := fifelse(ratio==0 ,'INI',fifelse(ratio == 100,'ONI','MIX'))]
  
    # add categorical variable for soil acidity
    d1[ph <= 4.5,ph_cat := 'extremely acidic']
    d1[ph > 4.5 & ph <= 5.5,ph_cat := 'strongly acidic']
    d1[ph > 5.5 & ph <= 6.5,ph_cat := 'acidic']
    d1[ph > 6.5, ph_cat := 'nonacidic']
  
    # add categorical variable for soil acidity
    d1[year >=1980 & year <= 1985,yeargap := '1980-1985']
    d1[year >=1986 & year <= 1990,yeargap := '1986-1990']
    d1[year >=1991 & year <= 1995,yeargap := '1991-1995']
    d1[year >=1996 & year <= 2000,yeargap := '1996-2000']
    d1[year >=2001 & year <= 2005,yeargap := '2001-2005']
    d1[year >=2006 & year <= 2010,yeargap := '2006-2010']
    d1[year >=2011 & year <= 2015,yeargap := '2011-2015']
    d1[year >=2016 & year <= 2020,yeargap := '2016-2020']
  
    # which columns are numeric
    cols <- colnames(d1[ , .SD, .SDcols = is.numeric])
  
    # save mean and sd per variable
    d1.mean <- d1[,lapply(.SD,mean,na.rm=T),.SDcols = cols]
    d1.sd <- d1[,lapply(.SD,sd,na.rm=T),.SDcols = cols]
    
    # scale
    d1[,c(cols) := lapply(.SD,scale),.SDcols = cols]
    
    # remove the variables not needed anymore
    cols <- c("fs","ph_cat","sitename","treatment","sitename1","tn","tk" ,'nueagro',
              "inv_tp", "year","treatments","treats","pm",'ct',"st",'yield','noutput',"yeargap",
              'soc','an','ak','preg','ap')
    d1[,c(cols) := NULL]
    
  # make a linear regression model, copied from Xingjuan
    
    # Perform one-hot encoding for categorical variables (to be used in XGBoost)
    cols <- c('ct1')
    d1[,c(cols) := lapply(.SD,as.factor),.SDcols = cols]
    d1 <- mltools::one_hot(d1, cols = c("ct1"))
    
    # Split in train and test set
    fr.test <- 0.20
    rows.test <- sample(1:nrow(d1), size = fr.test * nrow(d1), replace = FALSE)
    rows.train <- which(! 1:nrow(d1) %in% rows.test)
    
    # make two separate data.tables with training and testing data
    dt.train <- d1[rows.train, ]
    dt.test <- d1[rows.test, ]
    
    # make a linear model on training data
    m1 <- lm(ln_nue ~ ct1_P + ct1_PU  + ct1_U + ph  + I(ph^2) + I(ph^3)+ ln_soc + clay + ln_ap + tni  + 
                      I(ratio^2) + I(tpi^2) + tpi + ysn + ln_pre-1,data = dt.train)
    
    
  # building XGBoost -----------
    
    # make local copy
    d2 <- copy(d1)
    
    # select only relevant columns for modelling with XGBoost
    dt.train.xgb <- d2[rows.train, ]
    dt.test.xgb <- d2[rows.test, ]
    
    # set tuning methods
    target <- "ln_nue"
    tune_method <- "hyperband"
    
    # Set the task
    task.train <- TaskRegr$new(id = target, backend = dt.train.xgb, target = target)
    
    # Set the learner
    learner <-  mlr3::lrn("regr.xgboost")
    
    # Set the parameters of the learner
    learner$param_set$values <- list(
      #eval_metric = "rmsle",
      verbose = 0,
      nthread = 8,
      early_stopping_rounds = 12
    )
    
    ps.tune <- paradox::ParamSet$new(list(
      ParamInt$new("nrounds", lower = 100, upper = 1600, default = 750, tags = "budget"),
      ParamInt$new("max_depth", lower = 5, upper = 20, default = 12),
      ParamDbl$new("min_child_weight", lower = 1, upper = 5, default = 1),
      ParamDbl$new("subsample", lower = 0.5, upper = 1, default = 0.5),
      ParamDbl$new("colsample_bytree", lower = 0.5, upper = 1, default = 0.5),
      ParamDbl$new("eta", lower = 2^-9, upper = 2^-3, default = 2^-5),
      ParamDbl$new("gamma", lower = 0, upper = 5, default = 0)
    ))
    
    # Set the measures
    measures <- list(msr("regr.rmse"), msr("time_train"))
    
    # Set the resampling
    resampling <- mlr3::rsmp("cv", folds = 3L)
    
    # Set the tuning
    terminator <- mlr3tuning::trm("run_time", secs = 2 * 60 * 60)
    
    # Tune the model ---------------------------------------------------------

      tuner = tnr("hyperband", eta = 2L)
      
      inst <- TuningInstanceSingleCrit$new(
        task = task.train,
        learner = learner,
        resampling = resampling,
        measure = msr("regr.rmse"),
        search_space = ps.tune,
        terminator = terminator
      )
      
      result <- tuner$optimize(inst)
      
      # Set the best hyperparameters
      learner$param_set$values <- inst$result_learner_param_vals
      
      # Train the model
      model <- learner$train(task = task.train)
    
    
      # Interpret the model -----------------------------------------------------
      
      custom_predict <- function(model, newdata) {
        x <- predict(model, newdata = newdata)
        return(x)
      }
      
  # Create the explainers for both models
    explainer.train.xgb <- explain(model, 
                                   data = as.data.frame(dt.train.xgb[, .SD, .SDcols = !c(target)]), 
                                   y = dt.train.xgb$ln_nue, 
                                   label = "model_train_xgb")
    explainer.test.xgb <- explain(model, 
                                  data = as.data.frame(dt.test.xgb[, .SD, .SDcols = !c(target)]), 
                                  y = dt.test.xgb$ln_nue, 
                                  label = "model_test_xgb")
    cols.lm <- c('ph','ct1_P', 'ct1_PU', 'ct1_U','ln_soc','clay','tni','ln_ap','tni','ratio','tpi','ysn','ln_pre')
    explainer.train.lm <- explain(model = m1, 
                                   data = as.data.frame(dt.train[, .SD, .SDcols = cols.lm]), 
                                   y = dt.train$ln_nue, 
                                   label = "model_train_glm")
    explainer.test.lm <- explain(model = m1, 
                                  data = as.data.frame(dt.test[, .SD, .SDcols = cols.lm]), 
                                  y = dt.test$ln_nue, 
                                  label = "model_test_glm")
    
    # make VIP plot on training sets
    imp.xgb <- ingredients::feature_importance(explainer.train.xgb, 
                                               loss_function = loss_root_mean_square, 
                                               type = "difference")
    imp.lm <- ingredients::feature_importance(explainer.train.lm, 
                                               loss_function = loss_root_mean_square, 
                                               type = "difference")
    plot.vip <- ggplot_imp(imp.xgb,imp.lm)
    ggsave(plot = plot.vip,filename = 'products/plot2_vip_bar.png',width = 13.16, height = 8.90, units='cm')
    
    # make a residual plot on test
    res.xgb.test <- auditor::model_residual(explainer.test.xgb)
    res.xgb.train <- auditor::model_residual(explainer.train.xgb)
    res.lm.test <- auditor::model_residual(explainer.test.lm)
    res.lm.train <- auditor::model_residual(explainer.train.lm)  
    
    auditor::score_r2(explainer.test.lm)
    auditor::score_r2(explainer.test.xgb)
    
    plot.res <- ggplot_hist(res.xgb.train,res.xgb.test,res.lm.train,res.lm.test)
    ggsave(plot = plot.res,filename = 'products/plot2_res.png',width = 13.16, height = 8.90, units='cm')
    
    # make a 1-to-1 plot of both regressions
    plot.mp <- ggplot_onetoone(res.xgb.train,res.xgb.test,res.lm.train,res.lm.test)
    plot.mp <- ggplot_onetoone(res.xgb.test,res.lm.test)
    ggsave(plot = plot.mp,filename = 'products/plot3_1-to-1.png',width = 10.16, height = 8.90, units='cm')
    
    require(patchwork)
    pcombi <- plot.vip + plot.mp
    ggsave(plot = pcombi,filename = 'products/plot2_combi.png',width = 23, height = 8.90, units='cm')
    
    
    # plot ALE-plots
    ale.clay.xgb <- ingredients::accumulated_dependency(explainer.train.xgb, 'clay')
    ale.clay.lm <- ingredients::accumulated_dependency(explainer.train.lm, 'clay')
    ale.ph.xgb <- ingredients::accumulated_dependency(explainer.train.xgb, 'ph')
    ale.ph.lm <- ingredients::accumulated_dependency(explainer.train.lm, 'ph')
    ale.tni.xgb <- ingredients::accumulated_dependency(explainer.train.xgb, 'tni')
    ale.tni.lm <- ingredients::accumulated_dependency(explainer.train.lm, 'tni')
    ale.ap.xgb <- ingredients::accumulated_dependency(explainer.train.xgb, 'ln_ap')
    ale.ap.lm <- ingredients::accumulated_dependency(explainer.train.lm, 'ln_ap')
    ale.soc.xgb <- ingredients::accumulated_dependency(explainer.train.xgb, 'ln_soc')
    ale.soc.lm <- ingredients::accumulated_dependency(explainer.train.lm, 'ln_soc')
    ale.pre.xgb <- ingredients::accumulated_dependency(explainer.train.xgb, 'ln_pre')
    ale.pre.lm <- ingredients::accumulated_dependency(explainer.train.lm, 'ln_pre')
    #ale.tem.xgb <- ingredients::accumulated_dependency(explainer.train.xgb, 'temm')
    ale.ratio.xgb <- ingredients::accumulated_dependency(explainer.train.xgb, 'ratio')
    ale.ratio.lm <- ingredients::accumulated_dependency(explainer.train.lm, 'ratio')
    
    
    plot.ale <- ggplot_ale(ale.clay.xgb,ale.clay.lm,ale.ph.xgb,ale.ph.lm,
                           ale.tni.xgb,ale.tni.lm,ale.ap.xgb,ale.ap.lm,
                           ale.soc.xgb,ale.soc.lm, ale.pre.xgb,ale.pre.lm,
                           ale.ratio.xgb,ale.ratio.lm)
    ggsave(plot = plot.ale,filename = 'products/plot2_ale.png',width = 13.16, height = 10.90, units='cm')
   
    ale.ratio.xgb <- ingredients::accumulated_dependency(explainer.train.xgb, 'ratio')
    ale.ratio.lm <- ingredients::accumulated_dependency(explainer.train.lm, 'ratio')
    ggplot_ale(ale.ratio.xgb,ale.ratio.lm)
    
    # building global explanations from local explanations
    # build CP profile
    set.seed(1805)
    pdp.clay.xgb <- DALEX::model_profile(explainer.train.xgb,type='partial', variables = "clay")
    pdp.clay.lm <- DALEX::model_profile(explainer.train.lm,type='partial', variables = "clay")
    
  # make predictions for soil pH in an an averaged situation ----
    
    # make prediction data.frame for an average soil from the original input data, but with different pH range
    # you can also prepare a fake one with a given soil properties needed for the model
    dt.pred <- d1[,lapply(.SD,mean)][,ph := NULL]
    
    # adapt the pH to illustrate the impact of pH (and all other properties remain equal)
    dt.pred <- cbind(dt.pred,ph = seq(3.6,8,0.2))
    
    # convert pH to z-score
    dt.pred[,ph := (ph - d1.mean[,get('ph')]) / d1.sd[,get('ph')]]
    
    # set land use as example
    dt.pred[,ct1_P:= 1]
    dt.pred[,ct1_PU := 0]
    dt.pred[,ct1_U := 0]
    
    # predict lnNUE, and retransform back to original scale
    dt.pred[,lnNUEpred := predict(m1,newdata = dt.pred)]
    dt.pred[,lnNUEpred := lnNUEpred * d1.sd[,get('ln_nue')] + d1.mean[,get('ln_nue')]]
    dt.pred[,NUE_lm := exp(lnNUEpred) - 10]
    
    # predict lnNUE with XGBoost
    dt.pred[,lnNUExgb := predict(model,newdata = dt.pred)]
    dt.pred[,lnNUExgb := lnNUExgb * d1.sd[,get('ln_nue')] + d1.mean[,get('ln_nue')]]
    dt.pred[,NUE_xgboost := exp(lnNUExgb) - 10]
    
    # convert pH back to original scale
    dt.pred[,ph := ph * d1.sd[,get('ph')] + d1.mean[,get('ph')]]
    
    # melt the data.table before plotting
    dt.pred <- melt(dt.pred[,.(ph,NUE_lm,NUE_xgboost)],
                    id.vars = 'ph', 
                    variable.name = 'model',
                    value.name = 'nue')
    
    # plot impact of pH on the averaged soil
    p1 <- ggplot(dt.pred, aes(x = ph, y = nue,col = model,fill=model)) + 
          geom_point(size=5) + 
          geom_smooth(alpha=0.3) +
          theme_bw() + ggtitle('(a) impact of soil pH on NUE') + 
          ylim(0,75) + xlim(3,9) + xlab('soil pH') +
          ylab('NUE (%)') + theme(legend.position = c(0.2,0.8),
                                  legend.background = element_rect(fill='white')) + 
          scale_color_manual(name = 'model', values = c('gray85','springgreen4'))
        
    # save
    ggsave(plot = p1,filename = 'products/pred_ph.png')
    
  # make predictions for total N input in an an averaged situation ----
    
    # make prediction data.frame for an average soil from the original input data, but with different pH range
    # you can also prepare a fake one with a given soil properties needed for the model
    dt.pred <- d1[,lapply(.SD,mean)][,tni := NULL]
    
    # adapt the pH to illustrate the impact of pH (and all other properties remain equal)
    dt.pred <- cbind(dt.pred,tni = seq(0,400,25))
    
    # convert pH to z-score
    dt.pred[,tni := (tni - d1.mean[,get('tni')]) / d1.sd[,get('tni')]]
    
    # set land use as example
    dt.pred[,ct1_P:= 1]
    dt.pred[,ct1_PU := 0]
    dt.pred[,ct1_U := 0]
    
    # predict lnNUE, and retransform back to original scale
    dt.pred[,lnNUEpred := predict(m1,newdata = dt.pred)]
    dt.pred[,lnNUEpred := lnNUEpred * d1.sd[,get('ln_nue')] + d1.mean[,get('ln_nue')]]
    dt.pred[,NUE_lm := exp(lnNUEpred) - 10]
    
    # predict lnNUE with XGBoost
    dt.pred[,lnNUExgb := predict(model,newdata = dt.pred)]
    dt.pred[,lnNUExgb := lnNUExgb * d1.sd[,get('ln_nue')] + d1.mean[,get('ln_nue')]]
    dt.pred[,NUE_xgboost := exp(lnNUExgb) - 10]
    
    # convert pH back to original scale
    dt.pred[,tni := tni * d1.sd[,get('tni')] + d1.mean[,get('tni')]]
    
    # melt the data.table before plotting
    dt.pred <- melt(dt.pred[,.(tni,NUE_lm,NUE_xgboost)],
                    id.vars = 'tni', 
                    variable.name = 'model',
                    value.name = 'nue')
    
    # plot impact of pH on the averaged soil
    p2 <- ggplot(dt.pred, aes(x = tni, y = nue,col = model,fill=model)) + 
          geom_point(size=5) + 
          geom_smooth(alpha=0.3) +
          theme_bw() + ggtitle('(b) impact of total N input on NUE') + 
          ylim(0,75) + xlim(0,400) + xlab('Total N input (kg / ha') +
          ylab('NUE (%)') + theme(legend.position = c(0.2,0.8),
                                  legend.background = element_rect(fill='white')) + 
          scale_color_manual(name = 'model', values = c('gray85','springgreen4'))
    
    # save
    ggsave(plot = p2,filename = 'products/pred_nin.png')

    # make predictions for total N input in an an averaged situation ----
    
    # make prediction data.frame for an average soil from the original input data, but with different pH range
    # you can also prepare a fake one with a given soil properties needed for the model
    dt.pred <- d1[,lapply(.SD,mean)][,tpi := NULL]
    
    # adapt the pH to illustrate the impact of pH (and all other properties remain equal)
    dt.pred <- cbind(dt.pred,tpi = seq(0,300,25))
    
    # convert pH to z-score
    dt.pred[,tpi := (tpi - d1.mean[,get('tpi')]) / d1.sd[,get('tpi')]]
    
    # set land use as example
    dt.pred[,ct1_P:= 1]
    dt.pred[,ct1_PU := 0]
    dt.pred[,ct1_U := 0]
    
    # predict lnNUE, and retransform back to original scale
    dt.pred[,lnNUEpred := predict(m1,newdata = dt.pred)]
    dt.pred[,lnNUEpred := lnNUEpred * d1.sd[,get('ln_nue')] + d1.mean[,get('ln_nue')]]
    dt.pred[,NUE_lm := exp(lnNUEpred) - 10]
    
    # predict lnNUE with XGBoost
    dt.pred[,lnNUExgb := predict(model,newdata = dt.pred)]
    dt.pred[,lnNUExgb := lnNUExgb * d1.sd[,get('ln_nue')] + d1.mean[,get('ln_nue')]]
    dt.pred[,NUE_xgboost := exp(lnNUExgb) - 10]
    
    # convert pH back to original scale
    dt.pred[,tpi := tpi * d1.sd[,get('tpi')] + d1.mean[,get('tpi')]]
    
    # melt the data.table before plotting
    dt.pred <- melt(dt.pred[,.(tpi,NUE_lm,NUE_xgboost)],
                    id.vars = 'tpi', 
                    variable.name = 'model',
                    value.name = 'nue')
    
    # plot impact of pH on the averaged soil
    p3 <- ggplot(dt.pred, aes(x = tpi, y = nue,col = model,fill=model)) + 
      geom_point(size=5) + 
      geom_smooth(alpha=0.3) +
      theme_bw() + ggtitle('(c) impact of total P input on NUE') + 
      ylim(0,75) + xlim(0,300) + xlab('Total P input (kg / ha') +
      ylab('NUE (%)') + theme(legend.position = c(0.2,0.8),
                              legend.background = element_rect(fill='white')) + 
      scale_color_manual(name = 'model', values = c('gray85','springgreen4'))
    
    # save
    ggsave(plot = p3,filename = 'products/pred_pin.png')
    
  # make predictions for available P in an an averaged situation ----
    
    # make prediction data.frame for an average soil from the original input data, but with different pH range
    # you can also prepare a fake one with a given soil properties needed for the model
    dt.pred <- d1[,lapply(.SD,mean)][,ln_ap := NULL]
    
    # adapt the pH to illustrate the impact of pH (and all other properties remain equal)
    dt.pred <- cbind(dt.pred,ln_ap = log(seq(0.5,200,15)))
    
    # convert pH to z-score
    dt.pred[,ln_ap := (ln_ap - d1.mean[,get('ln_ap')]) / d1.sd[,get('ln_ap')]]
    
    # set land use as example
    dt.pred[,ct1_P:= 1]
    dt.pred[,ct1_PU := 0]
    dt.pred[,ct1_U := 0]
    
    # predict lnNUE, and retransform back to original scale
    dt.pred[,lnNUEpred := predict(m1,newdata = dt.pred)]
    dt.pred[,lnNUEpred := lnNUEpred * d1.sd[,get('ln_nue')] + d1.mean[,get('ln_nue')]]
    dt.pred[,NUE_lm := exp(lnNUEpred) - 10]
    
    # predict lnNUE with XGBoost
    dt.pred[,lnNUExgb := predict(model,newdata = dt.pred)]
    dt.pred[,lnNUExgb := lnNUExgb * d1.sd[,get('ln_nue')] + d1.mean[,get('ln_nue')]]
    dt.pred[,NUE_xgboost := exp(lnNUExgb) - 10]
    
    # convert pH back to original scale
    dt.pred[,ln_ap := ln_ap * d1.sd[,get('ln_ap')] + d1.mean[,get('ln_ap')]]
    dt.pred[,ap := exp(ln_ap)]
    
    # melt the data.table before plotting
    dt.pred <- melt(dt.pred[,.(ap,NUE_lm,NUE_xgboost)],
                    id.vars = 'ap', 
                    variable.name = 'model',
                    value.name = 'nue')
    
    # plot impact of pH on the averaged soil
    p4 <- ggplot(dt.pred, aes(x = ap, y = nue,col = model,fill=model)) + 
          geom_point(size=5) + 
          geom_smooth(alpha=0.3) +
          theme_bw() + ggtitle('(d) impact of soil available P on NUE') + 
          ylim(0,75) + xlim(0,200) + xlab('Soil available P (mg/kg') +
          ylab('NUE (%)') + theme(legend.position = c(0.2,0.8),
                                  legend.background = element_rect(fill='white')) + 
          scale_color_manual(name = 'model', values = c('gray85','springgreen4'))
        
    # save
    ggsave(plot = p4,filename = 'products/pred_ap.png')
    
  # make predictions for SOC in an an averaged situation ----
    
    # make prediction data.frame for an average soil from the original input data, but with different pH range
    # you can also prepare a fake one with a given soil properties needed for the model
    dt.pred <- d1[,lapply(.SD,mean)][,ln_soc := NULL]
    
    # adapt the pH to illustrate the impact of pH (and all other properties remain equal)
    dt.pred <- cbind(dt.pred,ln_soc = log(seq(0.5,5,0.5)))
    
    # convert pH to z-score
    dt.pred[,ln_soc := (ln_soc - d1.mean[,get('ln_soc')]) / d1.sd[,get('ln_soc')]]
    
    # set land use as example
    dt.pred[,ct1_P:= 1]
    dt.pred[,ct1_PU := 0]
    dt.pred[,ct1_U := 0]
    
    # predict lnNUE, and retransform back to original scale
    dt.pred[,lnNUEpred := predict(m1,newdata = dt.pred)]
    dt.pred[,lnNUEpred := lnNUEpred * d1.sd[,get('ln_nue')] + d1.mean[,get('ln_nue')]]
    dt.pred[,NUE_lm := exp(lnNUEpred) - 10]
    
    # predict lnNUE with XGBoost
    dt.pred[,lnNUExgb := predict(model,newdata = dt.pred)]
    dt.pred[,lnNUExgb := lnNUExgb * d1.sd[,get('ln_nue')] + d1.mean[,get('ln_nue')]]
    dt.pred[,NUE_xgboost := exp(lnNUExgb) - 10]
    
    # convert pH back to original scale
    dt.pred[,ln_soc := ln_soc * d1.sd[,get('ln_soc')] + d1.mean[,get('ln_soc')]]
    dt.pred[,soc := exp(ln_soc)]
    
    # melt the data.table before plotting
    dt.pred <- melt(dt.pred[,.(soc,NUE_lm,NUE_xgboost)],
                    id.vars = 'soc', 
                    variable.name = 'model',
                    value.name = 'nue')
    
    # plot impact of pH on the averaged soil
    p1 <- ggplot(dt.pred, aes(x = soc, y = nue,col = model,fill=model)) + 
          geom_point(size=5) + 
          geom_smooth(alpha=0.3) +
          theme_bw() + ggtitle('(a) impact of soil organic C on NUE') + 
          ylim(0,75) + xlim(0,5) + xlab('Soil organic C (mg/kg') +
          ylab('NUE (%)') + theme(legend.position = c(0.2,0.8),
                                  legend.background = element_rect(fill='white')) + 
          scale_color_manual(name = 'model', values = c('gray85','springgreen4'))
        
    # save
    ggsave(plot = p1,filename = 'products/pred_soc.png')
    
  # make prediction data.frame for ratio range
    
    # you can also prepare a fake one with a given soil properties needed for the model
    dt.pred <- d1[,lapply(.SD,mean)][,ratio := NULL]
    
    # adapt the pH to illustrate the impact of ratio (and all other properties remain equal)
    dt.pred <- cbind(dt.pred,ratio = seq(0,80,5))
    
    # convert pH to z-score
    dt.pred[,ratio := (ratio - d1.mean[,get('ratio')]) / d1.sd[,get('ratio')]]
    
    # set land use as example
    dt.pred[,ct1_P:= 1]
    dt.pred[,ct1_PU := 0]
    dt.pred[,ct1_U := 0]
    
    # predict lnNUE, and retransform back to original scale
    dt.pred[,lnNUEpred := predict(m1,newdata = dt.pred)]
    dt.pred[,lnNUEpred := lnNUEpred * d1.sd[,get('ln_nue')] + d1.mean[,get('ln_nue')]]
    dt.pred[,NUE_lm := exp(lnNUEpred) - 10]
    
    # predict lnNUE with XGBoost
    dt.pred[,lnNUExgb := predict(model,newdata = dt.pred)]
    dt.pred[,lnNUExgb := lnNUExgb * d1.sd[,get('ln_nue')] + d1.mean[,get('ln_nue')]]
    dt.pred[,NUE_xgboost := exp(lnNUExgb) - 10]
    
    # convert pH back to original scale
    dt.pred[,ratio := ratio * d1.sd[,get('ratio')] + d1.mean[,get('ratio')]]
    
    # melt the data.table before plotting
    dt.pred <- melt(dt.pred[,.(ratio,NUE_lm,NUE_xgboost)],
                    id.vars = 'ratio', 
                    variable.name = 'model',
                    value.name = 'nue')
    
    # plot impact of pH on the averaged soil
    p5 <- ggplot(dt.pred, aes(x = ratio, y = nue,col = model,fill=model)) + 
      geom_point(size=5) + 
      geom_smooth(alpha=0.3) +
      theme_bw() + ggtitle('(e) impact of organic N input on NUE') + 
      ylim(0,75) + xlim(1,80) + xlab('ratio organic N input (-)') +
      ylab('NUE (%)') + theme(legend.position = c(0.2,0.8),
                              legend.background = element_rect(fill='white')) + 
      scale_color_manual(name = 'model', values = c('gray85','springgreen4'))
    
    # save
    ggsave(plot = p1,filename = 'products/pred_ratio.png')
    
    # require patchwork
    require(patchwork)
    theme(legend.position = 'none') 
    
    p1a <- p1 + ggtitle('(a) impact of soil pH')
    p2a <- p2 + ggtitle('(b) impact of N input')
    p3a <- p3 + ggtitle('(c) impact of P input')
    p4a <- p4 + ggtitle('(d) impact of available P')
    p5a <- p5 + ggtitle('(e) impact of organic N input')
    
    p6 <- (p1a | p2a) / (p3a | p4a | p5a) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
        
    ggsave(plot = p6,filename = 'products/pred_all.png', width = 25, height = 15, units = 'cm')
    
# find the highest NUE that can be achieved for the experimental sites
    
    # you can also prepare a fake one with a given soil properties needed for the model
    dt.pred <- d1[,lapply(.SD,mean)][,c('ph','tci','ln_ap','ratio','tpi','tki','tni','ln_an') := NULL]
    
    dt.opt <- data.table(ph = runif(10000,-2,2))
    dt.opt[, ratio := runif(.N,-3,3)]
    dt.opt[,ln_ap := rnorm(.N,mean = 0, sd = 2)]
    dt.opt[,ln_an := ln_ap * (1 + 0.5 * rnorm(.N,mean = 0 , sd = 1))]
    dt.opt[,tci := rnorm(.N, mean = 0, sd = 3)]
    dt.opt[,tpi := tci * (1 + 0.5 * rnorm(.N,mean = 0 , sd = 1))]
    dt.opt[,tni := tci * (1 + 0.5 * rnorm(.N,mean = 0 , sd = 1))]
    dt.opt[,tki := tci * (1 + 0.5 * rnorm(.N,mean = 0 , sd = 1))]
    dt.pred <- cbind(dt.pred,dt.opt)
    
    # set land use as example
    dt.pred[,ct1_P:= 0]
    dt.pred[,ct1_PU := 0]
    dt.pred[,ct1_U := 1]
    
    dt.pred[,ysn := -1]
    
    # predict lnNUE, and retransform back to original scale
    dt.pred[,lnNUEpred := predict(m1,newdata = dt.pred)]
    dt.pred[,lnNUEpred := lnNUEpred * d1.sd[,get('ln_nue')] + d1.mean[,get('ln_nue')]]
    dt.pred[,NUE_lm := exp(lnNUEpred) - 10]
    
    # predict lnNUE with XGBoost
    dt.pred[,lnNUExgb := predict(model,newdata = dt.pred)]
    dt.pred[,lnNUExgb := lnNUExgb * d1.sd[,get('ln_nue')] + d1.mean[,get('ln_nue')]]
    dt.pred[,NUE_xgboost := exp(lnNUExgb) - 10]
    
    # add test
    dt.pred[, nclass := fifelse(NUE_lm > quantile(NUE_lm,0.90),1,0)]
    
    dt.eval <- dt.pred[,lapply(.SD,mean),by=nclass]
    dt.eval[,ph := ph * d1.sd[,get('ph')] + d1.mean[,get('ph')]]
    dt.eval[,ratio := ratio * d1.sd[,get('ratio')] + d1.mean[,get('ratio')]]
    dt.eval[,tni := tni * d1.sd[,get('tni')] + d1.mean[,get('tni')]]
    dt.eval[,tpi := tpi * d1.sd[,get('tpi')] + d1.mean[,get('tpi')]]
    dt.eval[,ln_ap := exp((ln_ap * d1.mean[,get('ln_ap')]) + d1.sd[,get('ln_ap')])]
    dt.eval[,ln_an := exp((ln_an * d1.mean[,get('ln_an')]) + d1.sd[,get('ln_an')])]
    
    
    hist(dt.pred$NUE_lm,xlim=c(0,100),n=150)
    quantile(dt.pred$NUE_lm,0.95)
    hist(dt.pred$NUE_xgboost,n=150)
    quantile(dt.pred$NUE_xgboost,0.975)
    
    dt.pred[,groep := fifelse(NUE_xgboost>quantile(NUE_xgboost,0.95) | 
                              NUE_lm > quantile(NUE_lm,0.95),'top','rest')]
    
    dt.pred[,lapply(.SD,mean), by = groep]
    
    a = sample(1:nrow(dt.pred),1000)
    plot(dt.pred$NUE_lm[a]~dt.pred$ratio[a])
    
    # melt the data.table before plotting
    dt.pred <- melt(dt.pred[,.(ratio,NUE_lm,NUE_xgboost)],
                    id.vars = 'ratio', 
                    variable.name = 'model',
                    value.name = 'nue')
    
    # plot impact of pH on the averaged soil
    p1 <- ggplot(dt.pred, aes(x = ratio, y = nue,col = model,fill=model)) + 
      geom_point(size=5) + 
      geom_smooth(alpha=0.3) +
      theme_bw() + ggtitle('(a) impact of organic N input on NUE') + 
      ylim(0,75) + xlim(1,80) + xlab('ratio organic N input (-)') +
      ylab('NUE (%)') + theme(legend.position = c(0.2,0.8),
                              legend.background = element_rect(fill='white')) + 
      scale_color_manual(name = 'model', values = c('gray85','springgreen4'))
    
    # save
    ggsave(plot = p1,filename = 'products/pred_ratio.png')
    
    