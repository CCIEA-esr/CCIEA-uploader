```{r setup}
#| echo: false
#| message: false
#| eval: true
library("googledrive")
library("tidyverse")

google_client <- gargle::gargle_oauth_client_from_json("/home/ldewitt/projects/IEA/docs/uploader/client_secret.json")


drive_auth_configure(client = google_client)
drive_find("CCIEA",n_max=5)

folderId='1feWjetaHALKEBC-5tH263ePtgyrAIv5a'
PI='Schroeder'
pifolder = drive_find(q="'1feWjetaHALKEBC-5tH263ePtgyrAIv5a' in parents and mimeType = 'application/vnd.google-apps.folder' and name = 'Schroeder'")
uploadfolder = drive_find(q="'1Cm11QGmKZESZVCgYE0M62W5IENcvKFPc' in parents and mimeType = 'application/vnd.google-apps.folder' and name = '2024-2025'")

q="'1C0DoDNfHIWeLrp5F3GKRNwHOR3r5FxqM' in parents and fileExtension = 'csv' and mimeType != 'application/vnd.google-apps.folder' and trashed=false"

files = drive_find(q="'1C0DoDNfHIWeLrp5F3GKRNwHOR3r5FxqM' in parents and fileExtension = 'csv' and mimeType != 'application/vnd.google-apps.folder' and trashed=false")
print(files)