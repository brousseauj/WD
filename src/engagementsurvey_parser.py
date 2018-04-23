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
surveyFile = pd.read_csv('~/Desktop/Data/wdps7a_flat_file_clean.csv')

class Text(EmbeddedDocument):
    comment = StringField()
    tokens = ListField()
    sentiment = DictField()

class Ratings(EmbeddedDocument):
    clearLink = IntField()
    managerGivesFeedback = IntField()
    comfortableDiscussingConcernsWithManager = IntField()
    involvedInDecisions = IntField()
    effortToGetOpinions = IntField()
    recommendWD = IntField()
    feelingPersonalAccomplishment = IntField()
    encouragedToReinvent = IntField()
    wdIsMakingChangesToCompete = IntField()
    peopleTakeResponsibilityForActions = IntField()
    affectiveCooperationBetweenDepartments = IntField()
    peopleCooperateToGetThingsDone = IntField()
    accomplishmentsAreRecognized = IntField()
    systemsToDoJob = IntField()
    processesToDoJob = IntField()
    teamHasResourcesToDoJob = IntField()
    givenOpportunityToImproveSkills = IntField()
    jobMakesUseOfSkills = IntField()
    careerAdvancement = IntField()
    advancementGivenFairly = IntField()
    WDCommittedToExceedingCustomerExpectations = IntField()
    seeAFutureAtWD = IntField()
    clearStrategyOfCompany = IntField()
    clearPerformanceGoals = IntField()
    meetingWithManagerToDiscussGoals = IntField()
    meaningfulTalkAboutPerformance = IntField()
    WDSupportsCommunities = IntField()
    volunteerReinforcesValues = IntField()
    volunteerInLastYear = IntField()



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

class engagementSurvey(Document):
    comments = ListField(EmbeddedDocumentField(Text))
    answers = ListField(EmbeddedDocumentField(Ratings))
    employee = ListField(EmbeddedDocumentField(Employee))
    uploadDate = DateTimeField()
    surveyDate = DateTimeField()



def fileMerger(employeeFile=employeeFile,surveyFile=surveyFile):
    df1 = employeeFile
    df2 = surveyFile

    df2=df2.merge(df1,left_on='employee id',right_on='Employee Id',how='left')
    df2 = df2.replace(np.nan,'',regex=True)

    df2['Job Level']=df2['Job Level'].astype(str)
    df2['Date Of Birth']=pd.to_datetime(df2['Date Of Birth'])
    df2['Seniority Date'] = pd.to_datetime(df2['Seniority Date'])
    df2['Date Survey was Completed']=pd.to_datetime(df2['Date Survey was Completed'])
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

        toks1=[j for j in word_tokenize(df['Currently what positively impacts your level of commitment?'][i].lower()) if j not in stop]
        toks2=([j for j in word_tokenize(df['Currently what negatively impacts your level of commitment?'][i].lower()) if j not in stop])
        toks = toks1 + toks2
        toks = [lem.lemmatize(x) for x in toks]
        toks = list(set(toks))
        text = df['Currently what negatively impacts your level of commitment?'][i]+" "+df['Currently what positively impacts your level of commitment?'][i]
        surveyDate = datetime.date(df['Date Survey was Completed'][i])
        hireDate = datetime.date(df['Seniority Date'][i])
        if (df['Employee Status'][i] == 'Active' or df['Employee Status'][i]=='Leave of Absence'):
            df['Termination Date'][i]='04/01/2262'
        df['Termination Date'][i]=pd.to_datetime(df['Termination Date'][i])
        df['Termination Date'][i]
        termDate = datetime.date(df['Termination Date'][i])
        birthDate = datetime.date(df['Date Of Birth'][i])

        sentiment = sid.polarity_scores(text)
        type(sentiment)

        ignoreList = ['na ','no ','NA ','No ','no comment ','nil ','No comment ',
              'n/a ','?#Name ','none ','None ',' none ','nothing ',
              'Nothing ','Nothing. ',' ']
        if text not in ignoreList:

            entry = engagementSurvey(uploadDate = datetime.now(),surveyDate = surveyDate)
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
            answers = Ratings(
                clearLink = df["1. I can see a clear link between my work and Western Digital's objectives."][i],
                managerGivesFeedback = df["2. My manager gives me feedback that helps me improve my performance."][i],
                comfortableDiscussingConcernsWithManager = df["3. I am comfortable discussing any concerns with my manager."][i],
                involvedInDecisions = df["4. I am appropriately involved in decisions that affect my work."][i],
                effortToGetOpinions = df["5. Sufficient effort is made to get the opinions and thinking of people who work here."][i],
                recommendWD = df["6. I would recommend Western Digital as a good place to work."][i],
                feelingPersonalAccomplishment = df["7. My work gives me a feeling of personal accomplishment."][i],
                encouragedToReinvent = df["8. I feel encouraged to come up with new and better ways of doing things."][i],
                wdIsMakingChangesToCompete = df["9. Western Digital is making the changes necessary to compete effectively."][i],
                peopleTakeResponsibilityForActions = df["10. People at Western Digital take responsibility for their actions."][i],
                affectiveCooperationBetweenDepartments = df["11. There is effective cooperation among departments."][i],
                peopleCooperateToGetThingsDone = df["12. The people I work with cooperate to get the job done."][i],
                accomplishmentsAreRecognized = df["13. When I do an excellent job, my accomplishments are recognized."][i],
                systemsToDoJob = df["14. I have the systems I need to do my job effectively."][i],
                processesToDoJob = df["15. I have the processes I need to do my job effectively."][i],
                teamHasResourcesToDoJob = df["16. My immediate work team has the resources (personnel, finances, etc.) necessary to do quality work."][i],
                givenOpportunityToImproveSkills = df["17. I am given a real opportunity to improve my skills at Western Digital."][i],
                jobMakesUseOfSkills = df["18. My job makes good use of my skills and abilities."][i],
                careerAdvancement = df["19. I have opportunities for career advancement at Western Digital."][i],
                advancementGivenFairly = df["20. Advancement opportunities are awarded fairly and objectively."][i],
                WDCommittedToExceedingCustomerExpectations = df["21. Western Digital is committed to exceeding our customers' expectations."][i],
                seeAFutureAtWD = df["22. I can see I have a future with Western Digital over the next 3-5 years."][i],
                clearStrategyOfCompany = df["23. I have a clear understanding of our strategy and why the legacy companies (WD, HGST, SD) have been combined."][i],
                clearPerformanceGoals = df["24. I have clear performance goals."][i],
                meetingWithManagerToDiscussGoals = df["25. Within the last quarter I have had a meeting with my manager to discuss my progress towards achieving my goals."][i],
                meaningfulTalkAboutPerformance = df["26. The conversation I had with my manager related to my performance was meaningful."][i],
                WDSupportsCommunities = df["27. I believe Western Digital is committed to supporting the communities where our employees live and work."][i],
                volunteerReinforcesValues = df["28. I believe the company's volunteer program reinforces the company's values."][i],
                volunteerInLastYear = df["In the last 12 months, have you participated in a company-sponsored volunteer event?"][i])
            comments = Text(comment = text,tokens = toks, sentiment = sentiment)
            entry.answers.append(answers)
            entry.employee.append(employee)
            entry.comments.append(comments)
            entry.save()

        print(i)
        i = 1 + i


if __name__=="__main__":
    main()
