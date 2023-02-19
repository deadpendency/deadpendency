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

variable "processing_failure_queue_topic_id" {
  type = string
}

variable "check_run_created_queue_topic_dlq_id" {
  type = string
}

variable "run_prepared_queue_topic_dlq_id" {
  type = string
}

variable "dependencies_determined_queue_topic_dlq_id" {
  type = string
}

data "google_project" "deadpendency_action_project" {
  project_id = "dgtw-deadpendency-action-2"
}
