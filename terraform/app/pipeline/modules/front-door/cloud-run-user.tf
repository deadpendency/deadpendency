resource "google_service_account" "front_door_cloud_run_account" {
  account_id   = "${var.app_env}-${var.app_live}-fd-cloud-run-account"
  display_name = "Front Door Cloud Run Account ${var.app_env}"
  project      = data.google_project.deadpendency_action_project.project_id
}

resource "google_pubsub_topic_iam_member" "front_door_cloud_run_account_init_queue_pubsub_publisher" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = google_pubsub_topic.deadpendency_action_pubsub_init_run.name
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:${google_service_account.front_door_cloud_run_account.email}"
}

resource "google_project_iam_member" "front_door_cloud_run_account_logging_writer" {
  project = data.google_project.deadpendency_action_project.project_id
  role    = "roles/logging.logWriter"
  member  = "serviceAccount:${google_service_account.front_door_cloud_run_account.email}"
}

resource "google_project_iam_member" "front_door_cloud_run_account_trace_writer" {
  project = data.google_project.deadpendency_action_project.project_id
  role    = "roles/cloudtrace.agent"
  member  = "serviceAccount:${google_service_account.front_door_cloud_run_account.email}"
}

resource "google_project_iam_member" "front_door_cloud_run_account_metric_writer" {
  project = data.google_project.deadpendency_action_project.project_id
  role    = "roles/monitoring.metricWriter"
  member  = "serviceAccount:${google_service_account.front_door_cloud_run_account.email}"
}

resource "google_secret_manager_secret_iam_member" "front_door_cloud_run_account_github_webhook_secret_reader" {
  project = data.google_project.deadpendency_action_project.project_id
  secret_id = "${var.app_env}-github-webhook-key-secret"
  role = "roles/secretmanager.secretAccessor"
  member = "serviceAccount:${google_service_account.front_door_cloud_run_account.email}"
}
