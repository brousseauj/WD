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
surveyFile = pd.read_csv('~/Desktop/Data/ONA/ONA-Cleaned_GM.csv')

class Text(EmbeddedDocument):
    comment = StringField()
    tokens = ListField()
    sentiment = DictField()



class Employee(EmbeddedDocument):
    Email = StringField()
    JobProfileName = StringField()
    EmployeeId = StringField()
    JobFamily = StringField()
    JobFamilyGroup = StringField()
    JobLevel = IntField()
    LegacyCompany = StringField()
    ManagementLevel = StringField()
    SVP=StringField()
    Manager=StringField()
    PrimaryWorkCountry = StringField()
    Region = StringField()
    Track = StringField()
class ONA_GM(Document):
    comments = ListField(EmbeddedDocumentField(Text))
    employee = ListField(EmbeddedDocumentField(Employee))
    uploadDate = DateTimeField()



def fileMerger(surveyFile=surveyFile):
    df2 = surveyFile
    df2 = df2.replace(np.nan,'',regex=True)
    df2['Job Level']=df2['Job Level'].astype(str)
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

        toks1=[j for j in word_tokenize(df['Q7'][i].lower()) if j not in stop]
        toks2=([j for j in word_tokenize(df['Q8'][i].lower()) if j not in stop])
        toks = toks1 + toks2
        toks = [lem.lemmatize(x) for x in toks]
        toks = list(set(toks))
        text = df['Q7'][i]+" "+df['Q8'][i]

        sentiment = sid.polarity_scores(text)
        type(sentiment)

        ignoreList = ['na ','no ','NA ','No ','no comment ','nil ','No comment ',
              'n/a ','?#Name ','none ','None ',' none ','nothing ',
              'Nothing ','Nothing. ',' ']
        if text not in ignoreList:

            entry = ONA_GM(uploadDate = datetime.now())
            employee = Employee(JobProfileName = df['Job Profile Name'][i],
                            JobFamily = df['Job Family'][i],
                            JobFamilyGroup = df['Job Family Group'][i],
                            JobLevel = df['Job Level'][i],
                            LegacyCompany = df['Legacy Company'][i],
                            ManagementLevel = df['Management Level'][i],
                            SVP=df['SVP'][i],
                            Manager=df['Manager'][i],
                            PrimaryWorkCountry = df['Primary Work Country'][i],
                            Region = df['Region'][i],
                            Track = df['Track'][i])

            comments = Text(comment = text,tokens = toks, sentiment = sentiment)
            entry.employee.append(employee)
            entry.comments.append(comments)
            entry.save()

        print(i)
        i = 1 + i


if __name__=="__main__":
    main()
