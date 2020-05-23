import os
PATH = "/mnt/c/Users/amogh/Documents/r_scripts"
args = ["Uttar_Pradesh","100","10","0.001","0.002","5","10"]
out = " "
for a in args:
    out = out + a + " "
# print(out)
# print("Rscript SIR_projections_simulate.R " + out)
os.system("Rscript SIR_projections_simulate.R " + out)
# SIR_model_simulate <- function(State_nam, 
#                                starting_num_cases, ###range 1-100
#                                Pred_time, ### range 1-100
#                                tranmission_rate, ## range 0 - 1
#                                recovery_rate ## range 0 - 1
#                                ){