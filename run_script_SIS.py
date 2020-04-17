import os
PATH = "/mnt/c/Users/amogh/Documents/r_scripts"
args = ["Delhi","101","10","3","15"]
out = " "
for a in args:
    out = out + a + " "
# print(out)
print("Rscript SIS_projections_simulate.R " + out)
os.system("Rscript SIS_projections_simulate.R " + out)