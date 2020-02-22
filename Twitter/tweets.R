library("twitteR")
library("ROAuth")


# Download "cacert.pem" file
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

api_key = '5eAxM60oCK4oME2pc086p3R3A'
api_secret = '24t4J77mp0YQ7AiXroqjnOh78jDiUuosKMwhlrJbxQqsZ0pdkD'

access_tocken = '893461067166171137-jV8a8R2lrs6Kp9TsB2lasdBQMWPlPHN'
access_tocken_secret = 'SKLEk9GDqnYnoNZVwFtAwvmHr10fwyvkDb5Ht5Jx2U0oW'

setup_twitter_oauth(api_key, api_secret, access_tocken,  access_tocken_secret)


T_trump = searchTwitter('from:realDonaldTrump', n=20)

T.df <- twListToDF(T_trump)




