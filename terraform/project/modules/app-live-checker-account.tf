resource "google_service_account" "app_live_checker_account" {
  account_id   = "deadpendency-app-live-checker"
  display_name = "Deadpendency App Live Checker"
  project      = google_project.deadpendency_project.project_id
}

resource "google_project_iam_member" "app_live_checker_account_run_viewer" {
  project = google_project.deadpendency_project.project_id
  role    = "roles/run.viewer"
  member  = "serviceAccount:${google_service_account.app_live_checker_account.email}"
}

resource "google_service_account_key" "app_live_checker_account_key" {
  service_account_id = google_service_account.app_live_checker_account.name
  public_key_type    = "TYPE_X509_PEM_FILE"
}

resource "google_secret_manager_secret" "app_live_checker_key_secret" {
  project   = google_project.deadpendency_project.project_id
  secret_id = "app-live-checker-key-secret"

  replication {
    automatic = true
  }
}

resource "google_secret_manager_secret_version" "app_live_checker_key_secret_version" {
  secret = google_secret_manager_secret.app_live_checker_key_secret.id

  secret_data = base64decode(google_service_account_key.app_live_checker_account_key.private_key)
}
