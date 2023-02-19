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

variable "check_run_created_queue_topic_id" {
  type = string
}

variable "check_run_created_queue_topic_dlq_id" {
  type = string
}

variable "processing_failure_queue_topic_id" {
  type = string
}

variable "minor_notification_channel" {
  type = string
}

data "google_project" "deadpendency_action_project" {
  project_id = "dgtw-deadpendency-action-2"
}

data "google_redis_instance" "deadpendency_action_cache2" {
  name = "deadpendency-cache2"
  project = data.google_project.deadpendency_action_project.project_id
}

output "run_prepared_queue_topic_id" {
  value = google_pubsub_topic.deadpendency_action_pubsub_run_prepared.id
}

output "run_prepared_queue_topic_dlq_id" {
  value = google_pubsub_topic.deadpendency_action_pubsub_run_prepared_dlq.id
}
