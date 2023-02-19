resource "google_service_account" "app_deploy_account" {
  account_id   = "deadpendency-app-deployer"
  display_name = "Deadpendency Application Deployer"
  project      = google_project.deadpendency_project.project_id
}

# Needs to be manually added as a domain manager in https://www.google.com/webmasters/verification/details?hl=en&authuser=1&domain=deadpendency-service.com

resource "google_project_iam_member" "app_deploy_account_run_admin" {
  project = google_project.deadpendency_project.project_id
  role    = "roles/run.admin"
  member  = "serviceAccount:${google_service_account.app_deploy_account.email}"
}

resource "google_project_iam_member" "app_deploy_account_service_account_user" {
  project = google_project.deadpendency_project.project_id
  role    = "roles/iam.serviceAccountUser"
  member  = "serviceAccount:${google_service_account.app_deploy_account.email}"
}

resource "google_project_iam_member" "app_deploy_account_tf_pubsub_admin" {
  project = google_project.deadpendency_project.project_id
  role    = "roles/pubsub.admin"
  member  = "serviceAccount:${google_service_account.app_deploy_account.email}"
}

resource "google_project_iam_member" "app_deploy_account_tf_service_account_iam_admin" {
  project = google_project.deadpendency_project.project_id
  role    = "roles/iam.serviceAccountAdmin"
  member  = "serviceAccount:${google_service_account.app_deploy_account.email}"
}

resource "google_project_iam_member" "app_deploy_account_tf_iam_admin" {
  project = google_project.deadpendency_project.project_id
  role    = "roles/iam.securityAdmin"
  member  = "serviceAccount:${google_service_account.app_deploy_account.email}"
}

resource "google_project_iam_member" "app_deploy_account_tf_compute_network_admin" {
  project = google_project.deadpendency_project.project_id
  role    = "roles/compute.networkAdmin"
  member  = "serviceAccount:${google_service_account.app_deploy_account.email}"
}

resource "google_project_iam_member" "app_deploy_account_tf_redis_admin" {
  project = google_project.deadpendency_project.project_id
  role    = "roles/redis.admin"
  member  = "serviceAccount:${google_service_account.app_deploy_account.email}"
}

resource "google_project_iam_member" "app_deploy_account_tf_vpcaccess_admin" {
  project = google_project.deadpendency_project.project_id
  role    = "roles/vpcaccess.admin"
  member  = "serviceAccount:${google_service_account.app_deploy_account.email}"
}

resource "google_project_iam_member" "app_deploy_account_tf_monitoring_admin" {
  project = google_project.deadpendency_project.project_id
  role    = "roles/monitoring.admin"
  member  = "serviceAccount:${google_service_account.app_deploy_account.email}"
}

resource "google_storage_bucket_iam_member" "app_deploy_account_tf_state_user" {
  bucket = google_storage_bucket.tf_dp_store.name
  role   = "roles/storage.objectAdmin"
  member = "serviceAccount:${google_service_account.app_deploy_account.email}"
}

resource "google_project_iam_member" "app_deploy_account_tf_secret_admin" {
  project = google_project.deadpendency_project.project_id
  role    = "roles/secretmanager.admin"
  member  = "serviceAccount:${google_service_account.app_deploy_account.email}"
}

resource "google_project_iam_member" "app_deploy_account_tf_cloudscheduler_admin" {
  project = google_project.deadpendency_project.project_id
  role    = "roles/cloudscheduler.admin"
  member  = "serviceAccount:${google_service_account.app_deploy_account.email}"
}

resource "google_service_account_key" "app_deploy_account_key" {
  service_account_id = google_service_account.app_deploy_account.name
  public_key_type    = "TYPE_X509_PEM_FILE"
}

resource "google_secret_manager_secret" "app_deploy_key_secret" {
  project   = google_project.deadpendency_project.project_id
  secret_id = "app-deploy-key-secret"

  replication {
    automatic = true
  }
}

resource "google_secret_manager_secret_version" "app_deploy_key_secret_version" {
  secret = google_secret_manager_secret.app_deploy_key_secret.id

  secret_data = base64decode(google_service_account_key.app_deploy_account_key.private_key)
}
