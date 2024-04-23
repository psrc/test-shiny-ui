# test version ----

rsconnect::deployApp(
  appName = 'test-household-travel-survey-trends',
  account = 'psrcwa'
)

# official version ----

rsconnect::deployApp(
  appName = 'household-travel-survey-trends',
  account = 'psrcwa'
)
