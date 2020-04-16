

SIER_model_simulate <- function(State_nam, 
                               starting_num_cases, 
                               Pred_time,
                               opt_recovery,
                               min_recovery_rate, 
                               max_recovery_rate,
                               min_transmission_rate, 
                               max_transmission_rate,
                               incubation_period,
                               min_incubation_rate, 
                               max_incubation_rate
                               ){
  
  #replacing _ with space for state name
  state_name_cache = State_nam
  State_nam = gsub("_", " ", State_nam)
  print(State_nam)
  options(warn=-1)
  suppressMessages(library(dplyr))
  library(dplyr)
  library(deSolve) 
  library(ggplot2)
  
  ##setwd('C:/aditya/Covid19/covid-19-india-data-master/complete.csv')
  
  df1 = read.csv('./complete.csv')
  df1_for_Sum  <- df1 %>% dplyr::select(-c("Name.of.State...UT", "Latitude", "Longitude"))
  df1_for_Sum <- df1_for_Sum %>% 
    group_by(Date) %>%
    summarise_all(sum)
  
  df1_for_Sum$Name.of.State...UT <- "India"  
  df1$Latitude <- NULL
  df1$Longitude <- NULL
  colnames(df1_for_Sum)
  colnames(df1)
  
  df1 <- rbind(df1_for_Sum, df1 )
  # all_contries_Conf  = read.csv('C:/aditya/Covid19/COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv')
  # all_contries_Conf<- all_contries_Conf[all_contries_Conf$Country.Region =="India",]
  # all_contries_Rec = read.csv('C:/aditya/Covid19/COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv')
  # all_contries_Death = read.csv('C:/aditya/Covid19/COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv')
  
  
  df1$Name.of.State...UT <-  gsub("Union Territory of Jammu and Kashmir", "Jammu and Kashmir", df1$Name.of.State...UT)
  df1$Name.of.State...UT <-  gsub("Odisha", "Orissa", df1$Name.of.State...UT)
  df1$Name.of.State...UT <-  gsub("Union Territory of Chandigarh", "Chandigarh", df1$Name.of.State...UT)
  df1$Name.of.State...UT <-  gsub("Union Territory of Ladakh", "Ladakh", df1$Name.of.State...UT)
  
  population_data = read.csv('./Population_data.csv')
  population_data$Population <- as.numeric(gsub("\\,", "", population_data$Population))
  
  #sum(population_data$Population, na.rm =NA)
  #setdiff(unique(df1$Name.of.State...UT), population_data$State)
  #scenario_list <- c(5,10,30,50,75,100)
  #avg_nat_list <- NULL
  
  ##for(i in scenario_list){
  temp_Rx <- NULL
  i = starting_num_cases
  #int_gamma = 0.28
  #int_beta = 0.27
  # pdf(
  # paste("C:/aditya/Covid19/covid-19-india-data-master/all_state_projections_cumulative_",i,
  #         ".pdf"), width = 8 , height = 5)
  # pdf(
  #   paste("C:/aditya/Covid19/covid-19-india-data-master/exp_results/R_B1_G0001_30",i,
  #         ".pdf"), width = 8 , height = 5)
  #par(mfrow =c(1,2))
  Pred_time = Pred_time
  ###for(State_nam in unique(df1$Name.of.State...UT)){
  df <-  df1 %>% filter(Name.of.State...UT == State_nam)
  
  df$Date <- as.Date(df$Date, "%Y-%m-%d")
  
  df <-  df %>% filter(Total.Confirmed.cases >= i )
  
  if(nrow(df) >= 3){
    #df = df1_for_Sum
    df$Date <- as.Date(df$Date, "%Y-%m-%d")
    
    ##df$Total_ConfirmedCases <- df$ Total.Confirmed.cases..Indian.National. + df$Total.Confirmed.cases...Foreign.National..
    
    
    
    Infected =  df$Total.Confirmed.cases - (df$Cured.Discharged.Migrated+df$Death)
    Recovered = df$Cured.Discharged.Migrated 
    # Infected = log(Infected,base = 10) #### Natural Log
    # Infected = log(Infected,base = 10) #### Natural Log 
    # Infected = Infected[c(1:16)]
    ##Infected <- c(diff(df$Total.Confirmed.cases), NA)
    
    N = population_data$Population[population_data$State == State_nam]  #### Population of India 
    
   
    #int_gamma <- df$Cured.Discharged.Migrated[nrow(df)]/df$Total.Confirmed.cases[nrow(df)]
    #int_beta <- Infected[nrow(df)]/df$Total.Confirmed.cases[nrow(df)]
    
    # int_beta1 <- int_beta - 0.9*(int_beta) 
    # int_beta2 <- int_beta + 0.9*(int_beta) 
    # 
    # int_gamma1 <- int_gamma - 0.9*(int_gamma) 
    # int_gamma2 <- int_gamma + 0.9*(int_gamma) 
    
    int_beta1 <- min_transmission_rate
    int_beta2 <- max_transmission_rate
    int_gamma1 <- min_transmission_rate
    int_gamma2 <- max_transmission_rate
    
    int_delta1 <- min_incubation_rate
    int_delta2 <- max_incubation_rate
  
    
    Day=1:(length(Infected))
    
    library(deSolve) # using the "ode" function
    # Old<- par(mfrow=c(1,2))
    # plot(Day, Infected, type ="b")
    # plot(Day, Infected, log = "y")
    # abline(lm(log10(Infected) ~ Day))
    # title("Confirmed Cases 2019-nCoV India", outer = TRUE, line = -2)
    #N = 1339200000
    
    seir_model = function (current_timepoint, state_values, parameters)
    {
      # create state variables (local variables)
      S = state_values [1]        # susceptibles
      E = state_values [2]        # exposed
      I = state_values [3]        # infectious
      R = state_values [4]        # recovered
      with (
        as.list (parameters),     # variable names within parameters can be used
        {
          # compute derivatives
          dS = (-beta * S * I)/N
          dE = (beta * S * I)/N - (delta * E)
          dI = (delta * E) - (gamma * I)
          dR = (gamma * I)
          # combine results
          results = c (dS, dE, dI, dR)
          list (results)
        }
      )
    }
    
    Day <- 1:length(Infected)
    init <- c(S = N-Infected[1]-incubation_period-Recovered[1],E = incubation_period,I = Infected[1],R = Recovered[1])
    
    Err1 <- function(parameters) {
      #print("Hello")
      names(parameters) <- c("beta", "gamma","delta")
      #print(init)
      #out <- ode(y = init, times = Day, func = sis_model, parms = parameters)
      #print(out)
      out <- ode(init, Day, seir_model, parameters)
      #print(out)
      fit <- out[,3]
      #print(parameters["gamma"])
      #sum((Infected - fit)^2+(Removed - out[,4])^2)
      x <- sum((Infected-fit)^2)
      return(x)
    }
    
    Err2 <- function(parameters) {
      #print("Hello")
      names(parameters) <- c("beta", "gamma","delta")
      #print(init)
      #out <- ode(y = init, times = Day, func = sis_model, parms = parameters)
      #print(out)
      out <- ode(init, Day, seir_model, parameters)
      #print(out)
      fit <- out[,3]
      rec <- out[,4]
      #print(parameters["gamma"])
      #sum((Infected - fit)^2+(Removed - out[,4])^2)
      x <- sum((Infected-fit)^2)
      y <- sum((Recovered-rec)^2)
      z <- alpha*x + (1- alpha)*y 
      return(z)
    }
    
    if(opt_recovery == T){
      Opt <- optim(c(0.5,0.5,0.5), Err2, method = "L-BFGS-B",
                   lower = c(int_beta1,int_gamma1, int_delta1),
                   upper = c(int_beta2,int_gamma2, int_delta2))
    }else{
      Opt <- optim(c(0.5,0.5,0.5), Err1, method = "L-BFGS-B",
                   lower = c(int_beta1,int_gamma1, int_delta1),
                   upper = c(int_beta2,int_gamma2, int_delta2))
      
    }
    Opt_par <- setNames(Opt$par, c("beta","gamma","delta"))
    Opt_par
    
    
    t <- 1:(nrow(df) + Pred_time)# time in days
    model1 <- ode(y = init, times = t, func = seir_model, parms = Opt_par)
    summary(model1)
    fit <- data.frame(ode(y = init, times = t, func = seir_model, parms = Opt_par))
    
    col <- 1:2 # colour
    fit$Date <- seq(df$Date[1], df$Date[1] + (nrow(df) + (Pred_time-1)), 1)
    
    df$Total.Confirmed.cases_non_cum  <- c(diff(df$Total.Confirmed.cases) , NA)
    df$Total.Confirmed.cases_non_cum[which(df$Total.Confirmed.cases_non_cum < 0)] = 0
    df$Cured.Discharged.Migrated_non_cum  <- c(diff(df$Cured.Discharged.Migrated) , NA)
    df$Cured.Discharged.Migrated[which(df$Cured.Discharged.Migrated < 0)] = 0
    
    fit$Actual_Infected <- c(df$Total.Confirmed.cases, rep(NA, (nrow(fit) - nrow(df))))
    fit$Actual_Recoverd <- c(df$Cured.Discharged.Migrated, rep(NA, (nrow(fit) - nrow(df))))
    
    fit$Actual_Infected_Non_cum <- c(df$Total.Confirmed.cases_non_cum, rep(NA, (nrow(fit) - nrow(df))))
    fit$Actual_Recoverd_Non_cum <- c(df$Cured.Discharged.Migrated_non_cum, rep(NA, (nrow(fit) - nrow(df))))
    #matplot(fit$time, fit[ , 3:4], type = "l", 
    #        xlab = "Day", ylab = "Number of subjects", 
    #        lwd = 2, lty = 1, col = col)
    
    #plot(fit[ , 3:4] ~ fit$Date, type = "l", 
    #        xlab = "Day", ylab = "Number of subjects", 
    #        lwd = 2, lty = 1, col = col)
    
    ########################### ggplots #######################
    colnames(fit)[4] <- "Predicted_recovered" 
    colnames(fit)[3] <- "Predicted_Infected" 
    fit$Predicted_recovered_non_cum  <- c(diff(fit$Predicted_recovered) , NA)
    fit$Predicted_recovered_non_cum[which(fit$Predicted_recovered_non_cum < 0)] = 0
    fit$Predicted_Infected_non_cum  <- c(diff(fit$Predicted_Infected) , NA)
    fit$Predicted_Infected_non_cum[which(fit$Predicted_Infected_non_cum < 0)] = 0
    
    
    
    fit$time <- NULL
    
    fit_sel_Cumulative <- fit %>% select(Date,
                                      Predicted_Infected,
                                      Predicted_recovered,
                                      Actual_Infected,
                                      Actual_Infected)
    
    fit_sel_non_cum <- fit %>% select(Date,
                                      Predicted_Infected_non_cum,
                                      Predicted_recovered_non_cum,
                                      Actual_Infected_Non_cum,
                                      Actual_Recoverd_Non_cum)
    dir.create(file.path(paste0(state_name_cache,"_Data"), 'cumulative'), recursive = TRUE)
    dir.create(file.path(paste0(state_name_cache,"_Data"), 'non_cumulative'))
    write.csv(fit_sel_Cumulative, 
               paste0(file.path(paste0(state_name_cache,"_Data"), 'cumulative', "SEIR.csv")),row.names = F )
    
    
    write.csv(fit_sel_non_cum, 
              paste0(file.path(paste0(state_name_cache,"_Data"), 'non_cumulative', "SEIR.csv")),row.names = F )
    fit_sel_non_cum  <- fit %>% select(Date,Predicted_Infected_non_cum,
                                      Actual_Infected_Non_cum)
    
    colnames(fit_sel_non_cum) <- gsub("_Non_cum", "", colnames(fit_sel_non_cum), ignore.case = T)
    fit_melt <- reshape2::melt(fit_sel_non_cum, id = c("Date"))
    
    R0 <- setNames(Opt_par["beta"] / Opt_par["gamma"], "R0")
    
    p = ggplot2::ggplot(data = fit_melt , aes(x = Date, y = value,
                                              color = variable,group = variable)) + geom_line()
    p <- p + theme(axis.text.x=element_text(angle = 45, size = 14, hjust=1),
                   axis.title =element_text(size = 14, face = "bold"))
    p <- p + labs(title = paste0(State_nam, " Starting From ",df$Date[1],", New Cases, R0= ", round(R0,2)),
                  caption = "Data Source: https://www.mohfw.gov.in/,
                  IIITD professors Tavpritesh Sethi, Ponnurangam Kumaraguru & Sriram K. along with their teams
                  Aditya Nagori, Raghav Awasthi, Chandan Gupta") +
      theme(legend.position="top", text = element_text(size = 12))
    # print(p + geom_point())
    
    temp_Rx <- rbind(temp_Rx, c(i,State_nam, R0, as.character(df$Date[1]), Opt_par["beta"] , Opt_par["gamma"]))
  }else{
    temp_Rx <- rbind(temp_Rx, c(i, State_nam, "Not enough data", NA, NA, NA))
  }
  colnames(temp_Rx) <- c("starting_cases_no","State_name", "R0", Opt_par['beta'],Opt_par['gamma'], 
                        "India_start_date")
  my_list <- list(fit_sel_Cumulative,fit_sel_non_cum, temp_Rx)
  # return(my_list)
}


#take in arguments and run the function

args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
} 
# print(args[1])
# print(args[2])
# print(args[3])
# print(args[4])
# print(args[5])
SIER_model_simulate(args[1],as.numeric(args[2]),as.numeric(args[3]),args[4],args[5],args[6],args[7],args[8],as.numeric(args[9]),args[10],args[11])
# SIER_model_simulate <- function(State_nam, 
#                                starting_num_cases, 
#                                Pred_time,
#                                opt_recovery,
#                                min_recovery_rate, 
#                                max_recovery_rate,
#                                min_transmission_rate, 
#                                max_transmission_rate,
#                                incubation_period,
#                                min_incubation_rate, 
#                                max_incubation_rate
#                                )





