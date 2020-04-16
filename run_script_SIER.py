import os
PATH = "/mnt/c/Users/amogh/Documents/r_scripts"
args = ["Delhi","100","10","1","0.002","0.001","0.002"]
out = " "
for a in args:
    out = out + a + " "
# print(out)
os.system("Rscript SEIR_model_simulate.R " + out)