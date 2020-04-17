import os
PATH = "/mnt/c/Users/amogh/Documents/r_scripts"
args = ["Delhi","101","10","3","15","5"]
out = " "
for a in args:
    out = out + a + " "
# print(out)
os.system("Rscript SEIR_model_simulate.R " + out)