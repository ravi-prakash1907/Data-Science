library("twitteR")
library("ROAuth")


# Download "cacert.pem" file
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

api_key = 'your_api_key'
api_secret = 'your_api_secret'

access_tocken = 'your_access_tocken'
access_tocken_secret = 'your_access_tocken_secret'

setup_twitter_oauth(api_key, api_secret, access_tocken,  access_tocken_secret)


T_trump = searchTwitter('from:realDonaldTrump', n=20)

T.df <- twListToDF(T_trump)




