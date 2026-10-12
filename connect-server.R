# # Connect to Shiny server
# rsconnect::connectCloudUser()

rsconnect::deployApp(appDir='tasks')

# rsconnect::deployApp(appDir='wordcloud/words')
rsconnect::deployApp(appDir='wordcloud/wordcloud')

# Deploy with different account
rsconnect::forgetDeployment("wordcloud/words")

rsconnect::deployApp(
  appDir = "wordcloud/words",
  account = "ybkamaleri"
)

