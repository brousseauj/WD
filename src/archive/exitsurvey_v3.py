import pandas as pd
from mongoengine import *
import nltk as nltk
import string
from datetime import datetime
import numpy as np
from nltk.corpus import stopwords
from nltk import word_tokenize
from nltk.stem import WordNetLemmatizer
from nltk.sentiment.vader import SentimentIntensityAnalyzer




connect('surveys', host='localhost', port=27017)
employeeFile=pd.read_csv('~/Desktop/Data/EmpInfo.csv',low_memory=False)
surveyFile = pd.read_csv('/Users/jb1000249384/Desktop/Data/Western Digital Exit Survey - Employees.csv',)



class exit_survey(Document):
    comment = StringField()
    tokens = ListField()
    sentiment = FloatField()
    surveyDate = DateTimeField()
    primaryForJoin = StringField()
    secondaryForJoin = StringField()
    tertiaryForJoin = StringField()
    primaryForLeaving = StringField()
    secondaryForLeaving = StringField()
    tertiaryForLeaving = StringField()
    incidentForLeaving = StringField()
    likelyToRecommend = StringField()
    compareWD = StringField()
    employeeId = StringField()
    employeeStatus = StringField()
    gender = StringField()
    jobFamilyGroup = StringField()
    jobLevel = StringField()
    legacyCompany = StringField()
    region = StringField()
    workCountry = StringField()
    dateOfBirth = DateTimeField()
    hireDate = DateTimeField()
    termDate = DateTimeField()
    comments = ListField()
    answers = ListField()
    employee = ListField()
    uploadDate = DateTimeField()

def fileMerger(employeeFile=employeeFile,surveyFile=surveyFile):
    df1 = employeeFile
    df2 = surveyFile


    df2=df2.merge(df1,left_on='EmployeeId',right_on='Employee Id',how='left')
    df2 = df2.replace(np.nan,'',regex=True)

    df2['Job Level']=df2['Job Level'].astype(str)
    df2['Date Of Birth']=pd.to_datetime(df2['Date Of Birth'])
    df2['Seniority Date'] = pd.to_datetime(df2['Seniority Date'])
    df2['surveyDate']=pd.to_datetime(df2['surveyDate'])
    return(df2)

def main():
    df = fileMerger()
    lem = WordNetLemmatizer()
    sid = SentimentIntensityAnalyzer()
    stop = set(stopwords.words('english'))
    stop.update(string.punctuation)
    stop.update(['na','NA','n/a','n a','nil'])
    i=0
    while i < len(df):
        toks1=[j for j in word_tokenize(df['incidentForLeaving-Comment'][i].lower()) if j not in stop]
        toks2=([j for j in word_tokenize(df['improvingWD'][i].lower()) if j not in stop])
        toks = toks1 + toks2
        toks = [lem.lemmatize(x) for x in toks]
        toks = list(set(toks))
        text = df['improvingWD'][i]+df['incidentForLeaving-Comment'][i]
        surveyDate = datetime.date(df['surveyDate'][i])
        hireDate = datetime.date(df['Seniority Date'][i])
        if df['Employee Status'][i] == 'Active':
            df['Termination Date'][i]='04/01/2262'
        df['Termination Date'][i]=pd.to_datetime(df['Termination Date'][i])

        termDate = datetime.date(df['Termination Date'][i])
        birthDate = datetime.date(df['Date Of Birth'][i])

        sentiment = sid.polarity_scores(text)['compound']
        entry = exit_survey(uploadDate = datetime.now(),
                        employeeId = df['Employee Id'][i],
                        employeeStatus = df['Employee Status'][i],
                        gender = df['Gender'][i],
                        jobFamilyGroup = df['Job Family Group'][i],
                        jobLevel = df["Job Level"][i],
                        legacyCompany = df['Legacy Company'][i],
                        region = df['Region'][i],
                        workCountry = df['Work Country'][i],
                        dateOfBirth = birthDate,
                        hireDate = hireDate,
                        termDate = termDate,
                        surveyDate = surveyDate,
                        primaryForJoin = df['primaryForJoin'][i],
                        secondaryForJoin = df['secondaryForJoin'][i],
                        tertiaryForJoin = df['tertiaryForJoin'][i],
                        primaryForLeaving = df['primaryForLeaving'][i],
                        secondaryForLeaving = df['secondaryForLeaving'][i],
                        tertiaryForLeaving = df['tertiaryForLeaving'][i],
                        incidentForLeaving = df['incidentForLeaving'][i],
                        likelyToRecommend = df['likelyToRecommend'][i],
                        compareWD = df['compareWD'][i],
                        comment = text,tokens = toks,sentiment = sentiment)
        entry.save()
        print(i)
        i = 1 + i


if __name__=="__main__":
    main()
