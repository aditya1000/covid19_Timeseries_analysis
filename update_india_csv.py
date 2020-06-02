#!/usr/bin/env python
# coding: utf-8
import json
import pandas as pd
import urllib.request

with urllib.request.urlopen("https://api.rootnet.in/covid19-in/stats/history") as url:
    data = json.loads(url.read().decode())

print("Saving last updated on", data['lastRefreshed'])

column_names = ['Date','Total Confirmed cases (Indian National)','Total Confirmed cases ( Foreign National )','Cured/Discharged/Migrated','Death','Total Confirmed cases']

df = pd.DataFrame(columns = column_names)

dflist = list()
for day in data['data']:
    for summary in [day['summary']]:
        dflist.append([day['day'],summary['confirmedCasesIndian'],summary['confirmedCasesForeign'],summary['discharged'],summary['deaths'],summary['total']])

df = pd.DataFrame(dflist, columns = column_names)

df['Date'] = pd.to_datetime(df['Date'])

df['Date'] = df['Date'].dt.strftime('%Y-%m-%d')

# df[df.Date=='2020-05-20']

# state = pd.read_csv('states.csv',names=['Name of State/UT','Latitude','Longitude'])

# dfs = pd.merge(df, state, on='Name of State / UT',how='left')


df.to_csv('india_ts.csv',index=None)


print("Done")
