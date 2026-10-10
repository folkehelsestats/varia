# Split Shiny word-cloud apps

This folder contains two independent Shiny apps:

- `word_input_app/app.R`: accepts one word at a time and appends it to Google Sheets.
- `wordcloud_display_app/app.R`: displays the word cloud and has Pause/Resume controls for polling.

## Setup

1. Install R and the required packages:

   ```r
   install.packages(c(
     "shiny", "shinyjs", "shinyWidgets",
     "highcharter", "googlesheets4"
   ))
   ```

2. Put the service-account JSON file in each app folder, named
   `service-task-survey.json`, **or** set the environment variable
   `GOOGLE_SERVICE_ACCOUNT_JSON` to the credentials file path before starting
   each app.

3. Ensure the Google service account has access to the spreadsheet with ID
   configured in both `app.R` files. Both apps must use the same spreadsheet
   ID and worksheet name (`words`).

4. Start each app separately, in separate R sessions/terminals:

   ```r
   shiny::runApp("word_input_app")
   shiny::runApp("wordcloud_display_app")
   ```

   You can also set the working directory to either app folder and run
   `shiny::runApp()`.

## Important behavior

- Pause stops the display app from reading new data; it does not stop people
  from submitting words. The chart remains at the last loaded state.
- Resume refreshes the data immediately, then continues polling every 3 seconds.
- The input app creates the `words` worksheet if it is missing. The display
  app expects that worksheet to exist, so start the input app once first if
  you are setting this up on a new spreadsheet.
- For deployment, keep the service-account JSON outside public source control
  and configure its path as an environment variable.
- `Reset responses` was omitted from the public-facing display app to avoid
  accidental deletion. If you need a protected moderator reset, add a separate
  admin-only control with authentication.
