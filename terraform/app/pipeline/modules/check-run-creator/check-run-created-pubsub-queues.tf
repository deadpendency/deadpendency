resource "google_pubsub_topic" "deadpendency_action_pubsub_check_run_created" {
  project = data.google_project.deadpendency_action_project.project_id
  name    = "${var.app_env}-${var.app_live}-deadpendency-action-check-run-created"
}

resource "google_pubsub_topic" "deadpendency_action_pubsub_check_run_created_dlq" {
  project = data.google_project.deadpendency_action_project.project_id
  name    = "${var.app_env}-${var.app_live}-deadpendency-action-check-run-created-dlq"
}

resource "google_pubsub_topic_iam_member" "service_account_check_run_created_dlq_queue_pubsub_publisher" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = google_pubsub_topic.deadpendency_action_pubsub_check_run_created_dlq.id
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:service-${data.google_project.deadpendency_action_project.number}@gcp-sa-pubsub.iam.gserviceaccount.com"
}
