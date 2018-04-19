import importlib
import numpy as np
import pandas as pd
from datetime import datetime
import json
from mongoengine import *


### Cleaning for SurveyMonkey
connect('surveys',host='localhost',port=27017)
empFile = pd.read_csv('~/Desktop/Data/EmpInfo.csv',low_memory=False)

class employee(Document):
    employeeId = StringField()
    employeeId = StringField()
    costCenter = StringField()
    directIndirect = StringField()
    employeeType = StringField()
    employeeStatus = StringField()
    gender = StringField()
    legacyCompany = StringField()
    managementLevel = StringField()
    birthdate = StringField()
    termDate = StringField()
    region = StringField()
    termType = StringField()
    country = StringField()

def main():
    df = empFile
    df = df.replace(np.nan,'',regex=True)
    i=0
    while i < len(df):
        emp = employee(
            employeeId = df['EmployeeId'][i],
            costCenter = df['CostCenter'][i],
            directIndirect = df['DirectIndirect'][i],
            employeeType = df['EmployeeType'][i],
            employeeStatus = df['EmployeeStatus'][i],
            gender = df['Gender'][i],
            legacyCompany = df['LegacyCompany'][i],
            managementLevel = df['ManagementLevel'][i],
            birthdate = df['BirthDate'][i],
            termDate = df['TermDate'][i],
            region = df['Region'][i],
            termType = df['TermType'][i],
            country = df['Country'][i]
            )
        emp.save()
        print('Document Saved')
        i=i+1


main()
