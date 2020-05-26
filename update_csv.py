#!/usr/bin/env python
# coding: utf-8
import json
import pandas as pd
import urllib.request

with urllib.request.urlopen("https://api.rootnet.in/covid19-in/stats/history") as url:
    data = json.loads(url.read().decode())

print("Saving last updated on", data['lastRefreshed'])

column_names = ['Date','Name of State / UT','Total Confirmed cases (Indian National)','Total Confirmed cases ( Foreign National )','Cured/Discharged/Migrated','Death','Total Confirmed cases']

df = pd.DataFrame(columns = column_names)

dflist = list()
for day in data['data']:
    for regional in day['regional']:
        dflist.append([day['day'],regional['loc'],regional['confirmedCasesIndian'],regional['confirmedCasesForeign'],regional['discharged'],regional['deaths'],regional['totalConfirmed']])

df = pd.DataFrame(dflist, columns = column_names)

# df[df.Date=='2020-05-20']

# state = pd.read_csv('states.csv',names=['Name of State/UT','Latitude','Longitude'])

dfs = pd.merge(df, state, on='Name of State / UT',how='left')



dfs.to_csv('complete.csv',index=None)


print("Done")




