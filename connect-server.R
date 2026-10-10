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

## QRcode
# install.packages("qrcode", repos = "https://thierryo.r-universe.dev")

library(qrcode)

words <- "http://ybkamaleri.shinyapps.io/words"
qr_code(words) |>
  generate_svg(
    "qrcode/qrcode-words.svg",
    background = "transparent",
    show = FALSE
  )

tasks <- "https://folkehelsestats-seminar.share.connect.posit.cloud/"
qr_code(tasks) |>
  generate_svg(
    "qrcode/qrcode-tasks.svg",
    background = "transparent",
    show = FALSE
  )
