import os
PATH = "/mnt/c/Users/amogh/Documents/r_scripts"
args = ["Delhi","100","10","T","0.001","0.002","0.001","0.002"]
out = " "
for a in args:
    out = out + a + " "
# print(out)
os.system("Rscript SIR_projections_simulate.R " + out)