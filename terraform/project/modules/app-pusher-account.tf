resource "google_service_account" "app_pusher_account" {
  account_id   = "deadpendency-app-pusher"
  display_name = "Deadpendency Application Docker Pusher"
  project      = google_project.deadpendency_project.project_id
}

resource "google_project_iam_member" "app_pusher_account_storage_admin" {
  project = google_project.deadpendency_project.project_id
  role    = "roles/storage.admin"
  member  = "serviceAccount:${google_service_account.app_pusher_account.email}"
}

resource "google_service_account_key" "app_pusher_account_key" {
  service_account_id = google_service_account.app_pusher_account.name
  public_key_type    = "TYPE_X509_PEM_FILE"
}

resource "google_secret_manager_secret" "app_pusher_key_secret" {
  project   = google_project.deadpendency_project.project_id
  secret_id = "app-pusher-key-secret"

  replication {
    automatic = true
  }
}

resource "google_secret_manager_secret_version" "app_pusher_key_secret_version" {
  secret = google_secret_manager_secret.app_pusher_key_secret.id

  secret_data = base64decode(google_service_account_key.app_pusher_account_key.private_key)
}
