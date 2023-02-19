# required due to https://cloud.google.com/pubsub/docs/push#setting_up_for_push_authentication
resource "google_project_iam_member" "pub_sub_account_sa_token_creator" {
  project = google_project.deadpendency_project.project_id
  role    = "roles/iam.serviceAccountTokenCreator"
  member  = "serviceAccount:service-${google_project.deadpendency_project.number}@gcp-sa-pubsub.iam.gserviceaccount.com"
}
