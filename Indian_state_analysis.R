
ts_ana_covid19 <- function(datapath_temp,datapath_cases,
                           datapath_output, lead_Time, State_nam){

##setwd("C:/aditya/Covid19/covid-19-india-data-master/")

#datapath_temp <- "C:/aditya/Covid19/TEMPERATURE_DATA_COUNTRY/INDIA/"
#datapath_cases <- "C:/aditya/Covid19/covid-19-india-data-master/"
#datapath_output <- "C:/aditya/Covid19/covid-19-india-data-master/For_App_ts_temp_hum_models/"


#temp_delhi <- read.csv(paste0(datapath_temp, "Delhi/delhi.csv"))
temp_kerala <- read.csv(datapath_temp)
#temp_maha <- read.csv(paste0(datapath_temp, "Maharashtra/MAHARASHTRA.csv"))


indian_cases <- read.csv(paste0(datapath_cases,"India_state_wise.csv"))

State_nam = State_nam
##State_nam= "Kerala"
indian_cases_kerala <- indian_cases[indian_cases$Name.of.State...UT == State_nam, ]
# indian_cases_Maharastra <- indian_cases[indian_cases$Name.of.State...UT == "Maharashtra", ]
# indian_cases_Delhi <- indian_cases[indian_cases$Name.of.State...UT == "Delhi", ]

indian_cases_kerala$Total.Confirmed.cases <- indian_cases_kerala$Total.Confirmed.cases..Indian.National. + 
  indian_cases_kerala$Total.Confirmed.cases...Foreign.National.. 

indian_cases_kerala$Total.Confirmed.cases_diff <- c(diff(indian_cases_kerala$Total.Confirmed.cases) , NA)
indian_cases_kerala$Total.Confirmed.cases_diff[indian_cases_kerala$Total.Confirmed.cases_diff < 0] = 0


library(lubridate)

kerala_ind <-intersect(which(month(indian_cases_kerala$Date[1]) == temp_kerala$MO), 
                       which(day(indian_cases_kerala$Date[1]) == temp_kerala$DY))
indian_cases_kerala_temp <- cbind(indian_cases_kerala[-nrow(indian_cases_kerala),],  temp_kerala[kerala_ind:nrow(temp_kerala), ])
indian_cases_kerala_temp <- indian_cases_kerala_temp[-nrow(indian_cases_kerala_temp),]
indian_cases_kerala_temp$Avg_temp <- ((indian_cases_kerala_temp$T2M_MAX + indian_cases_kerala_temp$T2M_MIN) / 2)


#############################################

plot.new()

png(paste0(datapath_output, "Kerala_temp_cases_ts.png"), width = 500, height = 250)
par(mar = c(10,1,1,1))
plot( indian_cases_kerala_temp$Total.Confirmed.cases_diff ~ indian_cases_kerala_temp$Date,
      type = "l", ylim = c(0,ymax),ylab = "Value" ,xlab = ".",las = 2 )
lines(indian_cases_kerala_temp$Avg_temp[-1], col = "2")
legend("topleft", legend = c("New Cases", "Avg Temperatue"), 
       col = c("black", "red"),lty=1:2, cex=0.8)
dev.off()


png(paste0(datapath_output, State_nam, " CCF_Results.png"), width = 1000, height = 400)
par(mar =c(6,2,8,2))
par(mfrow = c(1,2))
ccf(indian_cases_kerala_temp$Total.Confirmed.cases_diff,indian_cases_kerala_temp$Avg_temp , 
    main = paste0( State_nam," "  , "Average temperature Vs New confirmed cases"))
ccf(indian_cases_kerala_temp$Total.Confirmed.cases_diff,indian_cases_kerala_temp$QV2M , 
    main = paste0( State_nam," "  , "Humidity Vs New confirmed cases"))

dev.off()

lead_Time = lead_Time
indian_cases_kerala_temp$Total_new_cases_lead2 <- shift(indian_cases_kerala_temp$Total.Confirmed.cases_diff, lead_Time)

my_formula <- reformulate(termlabels = c("Total.Confirmed.cases_diff",
                                         "Avg_temp", "RH2M"),
                          response = "Total_new_cases_lead2")
print(my_formula)

# Build the model
model <- gam(my_formula, data = indian_cases_kerala_temp, family=poisson)
state_model_summ = summary(model) 
sink(paste0(datapath_output, "state_model_summ.txt"))
print(state_model_summ)
sink()

}
#c_val <- state_model_summ$p.coeff

##ggplot(data = state_model_summ, aes(x = rownames(state_model_summ), ))
##barplot(state_model_summ$p.coeff[-1], names = rownames(state_model_summ$p.coeff))

# indian_cases_Delhi$Total.Confirmed.cases <- indian_cases_Delhi$Total.Confirmed.cases..Indian.National. + 
#   indian_cases_Delhi$Total.Confirmed.cases...Foreign.National.. 
# indian_cases_kerala$New.Total.Confirmed.cases  <- c(diff(indian_cases_kerala$Total.Confirmed.cases), NA)
# 
# indian_cases_Maharastra$Total.Confirmed.cases <- indian_cases_Maharastra$Total.Confirmed.cases..Indian.National. + 
#   indian_cases_Maharastra$Total.Confirmed.cases...Foreign.National.. 
# indian_cases_kerala$New.Total.Confirmed.cases  <- c(diff(indian_cases_kerala$Total.Confirmed.cases), NA)
# 


# delhi$Total.Confirmed.cases_diff[indian_cases_Delhi$Total.Confirmed.cases_diff < 0] = 0
# 
# indian_cases_Maharastra$Total.Confirmed.cases_diff <- c(diff(indian_cases_Maharastra$Total.Confirmed.cases) , NA)
# indian_cases_Maharastra$Total.Confirmed.cases_diff[indian_cases_Maharastra$Total.Confirmed.cases_diff < 0] = 0


##plot(indian_cases_kerala$Date, indian_cases_kerala$Total.Confirmed.cases_diff)

#################  #####################

# library(lubridate)
# 
# kerala_ind <-intersect(which(month(indian_cases_kerala$Date[1]) == temp_kerala$MO), 
#                        which(day(indian_cases_kerala$Date[1]) == temp_kerala$DY))
# indian_cases_kerala_temp <- cbind(indian_cases_kerala[-nrow(indian_cases_kerala),],  temp_kerala[kerala_ind:nrow(temp_kerala), ])
# indian_cases_kerala_temp <- indian_cases_kerala_temp[-nrow(indian_cases_kerala_temp),]
# indian_cases_kerala_temp$Avg_temp <- ((indian_cases_kerala_temp$T2M_MAX + indian_cases_kerala_temp$T2M_MIN) / 2)


# delhi_ind <- intersect(which(month(indian_cases_Delhi$Date[1]) == temp_delhi$MO), 
#           which(day(indian_cases_Delhi$Date[1]) == temp_delhi$DY))
# indian_cases_Delhi_temp <- cbind(indian_cases_Delhi[-nrow(indian_cases_Delhi),],  temp_delhi[delhi_ind :nrow(temp_delhi), ])
# indian_cases_Delhi_temp <- indian_cases_Delhi_temp[-nrow(indian_cases_Delhi_temp),]
# indian_cases_Delhi_temp$Avg_temp <- ((indian_cases_Delhi_temp$T2M_MAX + indian_cases_Delhi_temp$T2M_MIN) / 2)


# 
# maha_ind <-intersect(which(month(indian_cases_Maharastra$Date[1]) == temp_kerala$MO), 
#                        which(day(indian_cases_Maharastra$Date[1]) == temp_kerala$DY))
# indian_cases_Maharastra_temp <- cbind(indian_cases_Maharastra[1:(nrow(indian_cases_Maharastra)-2),],  temp_maha[50:(nrow(temp_maha)-1), ])
# indian_cases_Maharastra_temp$Avg_temp <- ((indian_cases_Maharastra_temp$T2M_MAX + indian_cases_Maharastra_temp$T2M_MIN) / 2)
# indian_cases_Maharastra_temp <- indian_cases_Maharastra_temp[-nrow(indian_cases_Maharastra_temp), ] 


################################### KERALA ###############
#plot(indian_cases_kerala$Date, indian_cases_kerala$Total.Confirmed.cases, las = 2,
#ylab = "Total.Confirmed.cases", type = "l", main = "Kerala")
#ymax = max(indian_cases_kerala_temp$Total.Confirmed.cases_diff, 70)



# ################################ Delhi ##############
# # plot(indian_cases_Delhi$Date, indian_cases_Delhi$Total.Confirmed.cases, las = 2,
# #      ylab = "Total.Confirmed.cases", type = "l", main = "Delhi")
# indian_cases_Delhi$Total.Confirmed.cases_diff <- c(diff(indian_cases_Delhi$Total.Confirmed.cases) , NA)
# 
# png("Delhi_temp_cases_ts.png", width = 500, height = 250)
# plot( indian_cases_Delhi_temp$Total.Confirmed.cases_diff ~ indian_cases_Delhi_temp$Date
#         , type = "l", ylim = c(0,ymax),
#       ylab = "Value")
# lines(indian_cases_Delhi_temp$Avg_temp[-1], col = "2")
# legend("topleft", legend = c("New Cases", "Avg Temperatue(`C)"), 
#        col = c("black", "red"),lty=1:2, cex=0.8)
# dev.off()
# 
# indian_cases_Delhi_temp$Avg_temp <- ((indian_cases_Delhi_temp$T2M_MAX + indian_cases_Delhi_temp$T2M_MIN) / 2)
# 
# png("Delhi_CCF_Results.png", width = 1000, height = 400)
# par(mar =c(6,2,8,2))
# par(mfrow = c(1,2))
# ccf(indian_cases_Delhi_temp$Total.Confirmed.cases_diff,indian_cases_Delhi_temp$Avg_temp , 
#     main = "Delhi, Average temperature Vs New confirmed cases")
# ccf(indian_cases_Delhi_temp$Total.Confirmed.cases_diff,indian_cases_Delhi_temp$QV2M , 
#     main = "Delhi, Humidity Vs New confirmed cases")
# dev.off()
# ############################ Delhi Model#############
# lead_Time = 2
# indian_cases_Delhi_temp$Total_new_cases_lead2 <- shift(indian_cases_Delhi_temp$Total.Confirmed.cases_diff, lead_Time)
# 
# # library(mgcv)
# # 
# # my_formula <- reformulate_sp(termlabels = Total.Confirmed.cases_diff, 
# # sp1 = "Avg_temp",
# # sp2 = "RH2M",
# # response = "Total_new_cases_lead2")
# 
# my_formula <- reformulate(termlabels = c("Total.Confirmed.cases_diff", "Avg_temp", "RH2M"),
#                           response = "Total_new_cases_lead2")
# print(my_formula)
# 
# # Build the model
# model <- gam(my_formula, data = indian_cases_Delhi_temp, family=poisson)
# Delhi_model_summ  = summary(model) 
# print(Delhi_model_summ)
# 
# ############## Maharastra ####################################
# #@plot(indian_cases_Maharastra$Date, indian_cases_Maharastra$Total.Confirmed.cases,las = 2,
#     # ylab = "Total.Confirmed.cases", main = "Maharastra")
# #plot(indian_cases_Maharastra$Date, indian_cases_Maharastra$Total.Confirmed.cases_diff,las = 2,
#      #ylab = "New.Confirmed.cases", main = "Maharastra")
# 
# png("Maharastra_temp_cases_ts.png", width = 500, height = 400)
# par(mar = c(10,2,3,4))
# plot( indian_cases_Maharastra_temp$Total.Confirmed.cases_diff ~  indian_cases_Maharastra_temp$Date
#       , type = "l", ylim = c(0,ymax),
#       ylab = "Value",   xlab = "." ,las = 2)
# lines(indian_cases_Maharastra_temp$Avg_temp[-1], col = "2")
# ###lines(1000*(indian_cases_Maharastra_temp$QV2M),col = "3" )
# legend("topleft", legend = c("New Cases", "Avg Temperatue(`C)"), 
#        col = c("black", "red"),lty=1:2, cex=0.8)
# 
# dev.off()
# 
# png("Maha_CCF_Results.png", width = 1000, height = 400)
# par(mar =c(6,2,8,2))
# par(mfrow = c(1,2))
# 
# ccf(indian_cases_Maharastra_temp$Avg_temp , 
#     indian_cases_Maharastra_temp$Total.Confirmed.cases_diff,
#     main = "Maha, Average temperature Vs New confirmed cases")
# ccf(indian_cases_Maharastra_temp$RH2M ,
#     indian_cases_Maharastra_temp$Total.Confirmed.cases_diff,
#     main = "Maha, Humidity Vs New confirmed cases")
# dev.off()
# 
# 
# ############################ model maha ############
# lead_Time = 2
# indian_cases_Maharastra_temp$Total_new_cases_lead2 <- shift(indian_cases_Maharastra_temp$Total.Confirmed.cases_diff, lead_Time)
# 
# # library(mgcv)
# # 
# # my_formula <- reformulate_sp(termlabels = Total.Confirmed.cases_diff, 
# # sp1 = "Avg_temp",
# # sp2 = "RH2M",
# # response = "Total_new_cases_lead2")
# 
# my_formula <- reformulate(termlabels = c("Total.Confirmed.cases_diff", "Avg_temp", "RH2M"),
#                           response = "Total_new_cases_lead2")
# print(my_formula)
# 
# # Build the model
# model <- gam(my_formula, data = indian_cases_Maharastra_temp, family=poisson)
# maha_model_summ  = summary(model) 
# print(maha_model_summ)
# #######################################################################
# 
# ################Table of model coefficients()
# 
# model_coeffient_table <- rbind(maha= maha_model_summ$p.coeff, delhi= Delhi_model_summ$p.coeff, 
#       kerala = kerala_model_summ$p.coeff)
# 
# model_pval_table <- rbind(maha= maha_model_summ$p.pv, delhi= Delhi_model_summ$p.pv, 
#                                kerala = kerala_model_summ$p.pv)
# library(reshape2)
# library(ggplot2)
# c_melt <- melt(model_coeffient_table[,-1], id = rownames(model_coeffient_table))
# colnames(c_melt)[3] <- "Coeff"
# p_melt <- melt(model_pval_table[,-1], id = rownames(model_pval_table))
# 
# c_melt$pval <- p_melt$value 
# 
# write.csv(c_melt, paste0(datapath_output, "model_coeff_pvalue.csv"), row.names = F)

##}


