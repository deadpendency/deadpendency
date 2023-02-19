resource "google_pubsub_topic" "deadpendency_action_pubsub_dependencies_determined" {
  project = data.google_project.deadpendency_action_project.project_id
  name    = "${var.app_env}-${var.app_live}-deadpendency-action-dependencies-determined"
}

resource "google_pubsub_topic" "deadpendency_action_pubsub_dependencies_determined_dlq" {
  project = data.google_project.deadpendency_action_project.project_id
  name    = "${var.app_env}-${var.app_live}-deadpendency-action-dependencies-determined-dlq"
}

resource "google_pubsub_topic_iam_member" "service_account_dependencies_determined_dlq_queue_pubsub_publisher" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = google_pubsub_topic.deadpendency_action_pubsub_dependencies_determined_dlq.id
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:service-${data.google_project.deadpendency_action_project.number}@gcp-sa-pubsub.iam.gserviceaccount.com"
}
