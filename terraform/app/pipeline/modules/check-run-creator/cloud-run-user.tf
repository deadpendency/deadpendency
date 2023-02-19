resource "google_service_account" "check_run_creator_cloud_run_account" {
  account_id   = "${var.app_env}-${var.app_live}-crc-run-account"
  display_name = "Check Run Creator Cloud Run Account ${var.app_env}"
  project      = data.google_project.deadpendency_action_project.project_id
}

resource "google_pubsub_topic_iam_member" "check_run_creator_cloud_run_account_check_run_created_queue_pubsub_publisher" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = google_pubsub_topic.deadpendency_action_pubsub_check_run_created.name
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:${google_service_account.check_run_creator_cloud_run_account.email}"
}

resource "google_pubsub_topic_iam_member" "check_run_creator_cloud_run_account_processing_failure_queue_pubsub_publisher" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.processing_failure_queue_topic_id
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:${google_service_account.check_run_creator_cloud_run_account.email}"
}

resource "google_project_iam_member" "check_run_creator_cloud_run_account_logging_writer" {
  project = data.google_project.deadpendency_action_project.project_id
  role    = "roles/logging.logWriter"
  member  = "serviceAccount:${google_service_account.check_run_creator_cloud_run_account.email}"
}

resource "google_project_iam_member" "check_run_creator_run_account_trace_writer" {
  project = data.google_project.deadpendency_action_project.project_id
  role    = "roles/cloudtrace.agent"
  member  = "serviceAccount:${google_service_account.check_run_creator_cloud_run_account.email}"
}

resource "google_secret_manager_secret_iam_member" "check_run_creator_cloud_run_account_github_app_private_key_secret_reader" {
  project = data.google_project.deadpendency_action_project.project_id
  secret_id = "${var.app_env}-github-app-private-key-secret"
  role = "roles/secretmanager.secretAccessor"
  member = "serviceAccount:${google_service_account.check_run_creator_cloud_run_account.email}"
}
