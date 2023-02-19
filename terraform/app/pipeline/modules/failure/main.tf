variable "app_version" {
  type = string
}

variable "app_env" {
  type = string
}

variable "app_live" {
  type = string
}

data "google_project" "deadpendency_action_project" {
  project_id = "dgtw-deadpendency-action-2"
}

output "processing_failure_queue_topic_id" {
  value = google_pubsub_topic.deadpendency_action_pubsub_processing_failure.id
}

output "processing_failure_queue_topic_dlq_id" {
  value = google_pubsub_topic.deadpendency_action_pubsub_processing_failure_dlq.id
}
