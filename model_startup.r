resultdf = getresultdf()
fixtdf = getfixtdf()

dum = matchupspreadexdata(resultdf, fixtdf)
resultdf = dum$resultdf
fixtdf = dum$fixtdf

resultdf = bolsterresultdf(resultdf)

fixtdf = getfixturegoal(resultdf, fixtdf)

gbgdf=getgbgdf(resultdf)
summarydf=getsummary(gbgdf)

gbgdf=processdeserved(gbgdf)

summarydf=processdeserved(summarydf)

playerdf = getffplayerpricedf()
