data "google_service_account" "tf_da_admin_account" {
  account_id = "terraform-deadpendency-admin"
  project    = "dgtw-admin-2"
}

resource "google_service_account_key" "tf_da_admin_account_account_key" {
  service_account_id = data.google_service_account.tf_da_admin_account.name
  public_key_type    = "TYPE_X509_PEM_FILE"
}

resource "google_secret_manager_secret" "tf_da_admin_account_key_secret" {
  project   = google_project.deadpendency_project.project_id
  secret_id = "tf-da-admin-key-secret"

  replication {
    automatic = true
  }
}

resource "google_secret_manager_secret_version" "tf_da_admin_account_key_secret_version" {
  secret = google_secret_manager_secret.tf_da_admin_account_key_secret.id

  secret_data = base64decode(google_service_account_key.tf_da_admin_account_account_key.private_key)
}
