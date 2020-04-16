import os
PATH = "/mnt/c/Users/amogh/Documents/r_scripts"
args = ["Delhi","100","10","0.002","0.002"]
out = " "
for a in args:
    out = out + a + " "
# print(out)
print("Rscript SIS_projections_simulate.R " + out)
os.system("Rscript SIS_projections_simulate.R " + out)