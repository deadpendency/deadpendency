# This houses the script runner subscriptions. There needs to be only one subscription for a + b for each script runner.
# This allows for either a or b to process dlq items for a and b, clearing them both.
# Previoulsy there was a-sub-for-a etc. but b would not use / clean a-sub-for-a, only b-sub-for-a

variable "app_env" {
  type = string
}

variable "app_live" {
  type = string
}

variable "minor_notification_channel" {
  type = string
}

locals {
  init_queue_topic_dlq_id_a = "${var.app_env}-a-deadpendency-action-init-run-dlq"
  init_queue_topic_dlq_id_b = "${var.app_env}-b-deadpendency-action-init-run-dlq"
  init_queue_topic_dlq_sub_id_a = "${var.app_env}-init-queue-dlq-script-runner-subscription-a"
  init_queue_topic_dlq_sub_id_b = "${var.app_env}-init-queue-dlq-script-runner-subscription-b"

  check_run_created_queue_topic_dlq_id_a = "${var.app_env}-a-deadpendency-action-check-run-created-dlq"
  check_run_created_queue_topic_dlq_id_b = "${var.app_env}-b-deadpendency-action-check-run-created-dlq"
  check_run_created_queue_topic_dlq_sub_id_a = "${var.app_env}-check-run-created-queue-dlq-script-runner-subscription-a"
  check_run_created_queue_topic_dlq_sub_id_b = "${var.app_env}-check-run-created-queue-dlq-script-runner-subscription-b"

  run_prepared_queue_topic_dlq_id_a = "${var.app_env}-a-deadpendency-action-run-prepared-dlq"
  run_prepared_queue_topic_dlq_id_b = "${var.app_env}-b-deadpendency-action-run-prepared-dlq"
  run_prepared_queue_topic_dlq_sub_id_a = "${var.app_env}-run-prepared-queue-dlq-script-runner-subscription-a"
  run_prepared_queue_topic_dlq_sub_id_b = "${var.app_env}-run-prepared-queue-dlq-script-runner-subscription-b"

  dependencies_determined_queue_topic_dlq_id_a = "${var.app_env}-a-deadpendency-action-dependencies-determined-dlq"
  dependencies_determined_queue_topic_dlq_id_b = "${var.app_env}-b-deadpendency-action-dependencies-determined-dlq"
  dependencies_determined_queue_topic_dlq_sub_id_a = "${var.app_env}-dependencies-determined-queue-dlq-script-runner-subscription-a"
  dependencies_determined_queue_topic_dlq_sub_id_b = "${var.app_env}-dependencies-determined-queue-dlq-script-runner-subscription-b"

  dependencies_fetched_queue_topic_dlq_id_a = "${var.app_env}-a-deadpendency-action-dependencies-fetched-dlq"
  dependencies_fetched_queue_topic_dlq_id_b = "${var.app_env}-b-deadpendency-action-dependencies-fetched-dlq"
  dependencies_fetched_queue_topic_dlq_sub_id_a = "${var.app_env}-dependencies-fetched-queue-dlq-script-runner-subscription-a"
  dependencies_fetched_queue_topic_dlq_sub_id_b = "${var.app_env}-dependencies-fetched-queue-dlq-script-runner-subscription-b"

  report_generated_queue_topic_dlq_id_a = "${var.app_env}-a-deadpendency-action-report-generated-dlq"
  report_generated_queue_topic_dlq_id_b = "${var.app_env}-b-deadpendency-action-report-generated-dlq"
  report_generated_queue_topic_dlq_id_sub_a = "${var.app_env}-report-generated-queue-dlq-script-runner-subscription-a"
  report_generated_queue_topic_dlq_id_sub_b = "${var.app_env}-report-generated-queue-dlq-script-runner-subscription-b"
}

data "google_project" "deadpendency_action_project" {
  project_id = "dgtw-deadpendency-action-2"
}
