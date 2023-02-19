resource "google_service_account" "dependency_determiner_cloud_run_account" {
  account_id   = "${var.app_env}-${var.app_live}-dd-cloud-run-account"
  display_name = "Dependency Determiner Cloud Run Account ${var.app_env}"
  project      = data.google_project.deadpendency_action_project.project_id
}

resource "google_pubsub_topic_iam_member" "dependency_determiner_cloud_run_account_dependencies_determined_queue_pubsub_publisher" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = google_pubsub_topic.deadpendency_action_pubsub_dependencies_determined.name
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:${google_service_account.dependency_determiner_cloud_run_account.email}"
}

resource "google_pubsub_topic_iam_member" "dependency_determiner_cloud_run_account_processing_failure_queue_pubsub_publisher" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.processing_failure_queue_topic_id
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:${google_service_account.dependency_determiner_cloud_run_account.email}"
}

resource "google_pubsub_topic_iam_member" "dependency_determiner_cloud_run_account_dlq_queue_pubsub_publisher" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.run_prepared_queue_topic_dlq_id
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:${google_service_account.dependency_determiner_cloud_run_account.email}"
}

resource "google_project_iam_member" "dependency_determiner_cloud_run_account_logging_writer" {
  project = data.google_project.deadpendency_action_project.project_id
  role    = "roles/logging.logWriter"
  member  = "serviceAccount:${google_service_account.dependency_determiner_cloud_run_account.email}"
}

resource "google_project_iam_member" "dependency_determiner_run_account_trace_writer" {
  project = data.google_project.deadpendency_action_project.project_id
  role    = "roles/cloudtrace.agent"
  member  = "serviceAccount:${google_service_account.dependency_determiner_cloud_run_account.email}"
}

resource "google_secret_manager_secret_iam_member" "dependency_determiner_cloud_run_account_github_app_private_key_secret_reader" {
  project = data.google_project.deadpendency_action_project.project_id
  secret_id = "${var.app_env}-github-app-private-key-secret"
  role = "roles/secretmanager.secretAccessor"
  member = "serviceAccount:${google_service_account.dependency_determiner_cloud_run_account.email}"
}
