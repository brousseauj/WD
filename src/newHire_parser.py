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
surveyFile = pd.read_csv('/Users/jb1000249384/Desktop/Data/New Hire Survey 30 Days_clean.csv')



class Text(EmbeddedDocument):
    comment = StringField()
    tokens = ListField()
    sentiment = DictField()

class Ratings(EmbeddedDocument):
    surveyDate = DateTimeField()
    accuratePosition = StringField()
    communication = StringField()
    smoothTransition = StringField()
    NewHireAffective = StringField()
    receivedTraining = StringField()
    contactHRCentral = StringField()
    contactIT = StringField()
    agreementVision = StringField()
    agreementNewHireBenefits = StringField()
    agreementNewHirePerformance = StringField()
    agreementIT= StringField()
    agreementFacilities= StringField()
    agreementProcessGuidance= StringField()
    agreementCoworkerIntroduction= StringField()
    agreementTransportation= StringField()
    defineExpectations= StringField()
    clearLinkObjectives= StringField()
    improvePerformance= StringField()
    discussingConcerns= StringField()
    systemsAndProcesses= StringField()
    jobPerformanceTime= StringField()
    discretionary= StringField()
    values= StringField()
    belonging= StringField()
    statisfiedDecision= StringField()
    goodPlaceToWork= StringField()
    future35years= StringField()

class Employee(EmbeddedDocument):
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

class newHire30Survey(Document):
    comments = ListField(EmbeddedDocumentField(Text))
    answers = ListField(EmbeddedDocumentField(Ratings))
    employee = ListField(EmbeddedDocumentField(Employee))
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
        toks1=[j for j in word_tokenize(df['workedWellForNewHires'][i].lower()) if j not in stop]
        toks2=([j for j in word_tokenize(df['improvedForNewHiresComment'][i].lower()) if j not in stop])
        toks = toks1 + toks2
        toks = [lem.lemmatize(x) for x in toks]
        toks = list(set(toks))
        text = df['improvedForNewHiresComment'][i]+" "+df['workedWellForNewHires'][i]
        surveyDate = datetime.date(df['surveyDate'][i])
        hireDate = datetime.date(df['Seniority Date'][i])
        if (df['Employee Status'][i] == 'Active' or df['Employee Status'][i]=='Leave of Absence'):
            df['Termination Date'][i]='04/01/2262'
        df['Termination Date'][i]=pd.to_datetime(df['Termination Date'][i])

        termDate = datetime.date(df['Termination Date'][i])
        birthDate = datetime.date(df['Date Of Birth'][i])

        sentiment = sid.polarity_scores(text)
        type(sentiment)
        entry = newHire30Survey(uploadDate = datetime.now())
        employee = Employee(employeeId = df['Employee Id'][i],
                        employeeStatus = df['Employee Status'][i],
                        gender = df['Gender'][i],
                        jobFamilyGroup = df['Job Family Group'][i],
                        jobLevel = df["Job Level"][i],
                        legacyCompany = df['Legacy Company'][i],
                        region = df['Region'][i],
                        workCountry = df['Work Country'][i],
                        dateOfBirth = birthDate,
                        hireDate = hireDate,
                        termDate = termDate)
        answers = Ratings(surveyDate = surveyDate,
                        accuratePosition = df['accuratePosition'][i],
                        communication = df['communication'][i],
                        smoothTransition = df['smoothTransition'][i],
                        NewHireAffective = df['NewHireAffective'][i],
                        receivedTraining=df["I received the appropriate amount of training upon joining Western Digital."][i],
                        contactHRCentral = df['contactHRCentral'][i],
                        contactIT = df['contactIT'][i],
                        agreementVision = df['agreementVision'][i],
                        agreementNewHireBenefits = df['agreementNewHireBenefits'][i],
                        agreementNewHirePerformance = df['agreementNewHirePerformance'][i],
                        agreementIT = df['agreementIT'][i],
                        agreementFacilities = df['agreementFacilities'][i],
                        agreementProcessGuidance = df['agreementProcessGuidance'][i],
                        agreementCoworkerIntroduction = df['agreementCoworkerIntroduction'][i],
                        agreementTransportation = df['agreementTransportation'][i],
                        defineExpectations = df['defineExpectations'][i],
                        clearLinkObjectives = df['clearLinkObjectives'][i],
                        improvePerformance = df['improvePerformance'][i],
                        discussingConcerns = df['discussingConcerns'][i],
                        systemsAndProcesses = df['systemsAndProcesses'][i],
                        jobPerformanceTime = df['jobPerformanceTime'][i],
                        discretionary = df['discretionary'][i],
                        values = df['values'][i],
                        belonging = df['belonging'][i],
                        statisfiedDecision = df['statisfiedDecision'][i],
                        goodPlaceToWork=df['goodPlaceToWork'][i],
                        future35years= df['future3-5years'][i])
        comments = Text(comment = text,tokens = toks,sentiment = sentiment)
        entry.answers.append(answers)
        entry.employee.append(employee)
        entry.comments.append(comments)
        entry.save()

        print(i)
        i = 1 + i


if __name__=="__main__":
    main()
