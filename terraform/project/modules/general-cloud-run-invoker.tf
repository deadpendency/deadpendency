resource "google_service_account" "general_cloud_run_invoker_account" {
  account_id   = "general-cr-invoker-account"
  display_name = "Message Replayer Invoker Cloud Run Account"
  project      = google_project.deadpendency_project.project_id
}

resource "google_project_iam_member" "general_cloud_run_invoker_account_invoker" {
  project = google_project.deadpendency_project.project_id
  role    = "roles/run.invoker"
  member  = "serviceAccount:${google_service_account.general_cloud_run_invoker_account.email}"
}

resource "google_service_account_key" "general_cloud_run_invoker_account_key" {
  service_account_id = google_service_account.general_cloud_run_invoker_account.name
  public_key_type    = "TYPE_X509_PEM_FILE"
}

resource "google_secret_manager_secret" "general_cloud_run_invoker_key_secret" {
  project   = google_project.deadpendency_project.project_id
  secret_id = "general-cloud-run-invoker-key-secret"

  replication {
    automatic = true
  }
}

resource "google_secret_manager_secret_version" "general_cloud_run_invoker_key_secret_version" {
  secret = google_secret_manager_secret.general_cloud_run_invoker_key_secret.id

  secret_data = base64decode(google_service_account_key.general_cloud_run_invoker_account_key.private_key)
}
