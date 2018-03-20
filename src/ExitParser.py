import importlib
import src
import numpy as np
import pandas as pd
from datetime import datetime
import json
from mongoengine import *
import nltk as nltk
import string
from nltk.corpus import stopwords
from nltk import word_tokenize

connect('surveys', host='localhost', port=27017)
employeeFile=pd.read_csv('~/Desktop/Data/EmpInfo.csv',low_memory=False)
surveyFile = pd.read_csv('/Users/jb1000249384/Desktop/Data/Western Digital Exit Survey - Employees.csv')

class exitSurvey(Document):
    employeeId = StringField()
    costCenter = StringField()
    legacyCompany = StringField()
    directLaborTag = StringField()
    employeeType = StringField()
    employeeStatus = StringField()
    Gender = StringField()
    managementLevel = StringField()
    birthDate = StringField()
    hireDate = StringField()
    termDate = StringField()
    surveyDate = StringField()
    primaryForJoin = StringField()
    secondaryForJoin = StringField()
    tertiaryForJoin = StringField()
    primaryForLeaving = StringField()
    secondaryForLeaving = StringField()
    tertiaryForLeaving = StringField()
    incidentForLeaving = StringField()
    incidentForLeavingComment = StringField()
    likelyToRecommend = StringField()
    compareWD = StringField()
    improvingWDComment = StringField()
    canWeContact = StringField()
    newContactEmail = StringField()
    toks_incidentForLeaving = ListField()
    toks_improvingWD = ListField()

def fileMerger(employeeFile=employeeFile,surveyFile=surveyFile):
    df1 = employeeFile
    df2 = surveyFile
    df2=df2.merge(df1,left_on='EmployeeId',right_on='EmployeeId',how='left')
    df2 = df2.replace(np.nan,'',regex=True)
    return(df2)

def main():
    df = fileMerger()
    #df = df.head(1)
    stop = set(stopwords.words('english'))
    stop.update(string.punctuation)
    stop.update(['na','NA','n/a','n a','nil'])
    i = 0
    while i < len(df):
        toks1=[j for j in word_tokenize(df['incidentForLeaving-Comment'][i].lower()) if j not in stop]
        toks2=([j for j in word_tokenize(df['improvingWD'][i].lower()) if j not in stop])


        emp = exitSurvey(employeeId = df['EmployeeId'][i],
                       costCenter = df['CostCenter'][i],
                       legacyCompany=df['LegacyCompany'][i],
                       directLaborTag = df['DirectIndirect'][i],
                       employeeType = df['EmployeeType'][i],
                       employeeStatus = df['EmployeeStatus'][i],
                       Gender = df['Gender'][i],
                       managementLevel = df['ManagementLevel'][i],
                       birthDate = df['BirthDate'][i],
                       hireDate = df['HireDate'][i],
                       termDate = df['TermDate'][i],
                       surveyDate = df['surveyDate'][i],
                       primaryForJoin = df['primaryForJoin'][i],
                       secondaryForJoin = df['secondaryForJoin'][i],
                       tertiaryForJoin = df['tertiaryForJoin'][i],
                       primaryForLeaving = df['primaryForLeaving'][i],
                       secondaryForLeaving = df['secondaryForLeaving'][i],
                       tertiaryForLeaving = df['tertiaryForLeaving'][i],
                       incidentForLeaving = df['incidentForLeaving'][i],
                       incidentForLeavingComment = df['incidentForLeaving-Comment'][i],
                       likelyToRecommend = df['likelyToRecommend'][i],
                       compareWD = df['compareWD'][i],
                       improvingWDComment = df['improvingWD'][i],
                       canWeContact = df['contact'][i],
                       newContactEmail = df['newContactEmail'][i],
                       toks_incidentForLeaving = toks1,
                       toks_improvingWD = toks2)
        emp.save()
        i = i + 1


main()
