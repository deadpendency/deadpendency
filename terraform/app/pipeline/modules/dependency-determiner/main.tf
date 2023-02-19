variable "app_version" {
  type = string
}

variable "app_version_sha" {
  type = string
}

variable "app_env" {
  type = string
}

variable "app_live" {
  type = string
}

variable "app_id" {
  type = string
}

variable "run_prepared_queue_topic_id" {
  type = string
}

variable "run_prepared_queue_topic_dlq_id" {
  type = string
}

variable "processing_failure_queue_topic_id" {
  type = string
}

data "google_project" "deadpendency_action_project" {
  project_id = "dgtw-deadpendency-action-2"
}

output "dependencies_determined_queue_topic_id" {
  value = google_pubsub_topic.deadpendency_action_pubsub_dependencies_determined.id
}

output "dependencies_determined_queue_topic_dlq_id" {
  value = google_pubsub_topic.deadpendency_action_pubsub_dependencies_determined_dlq.id
}

variable "minor_notification_channel" {
  type = string
}
