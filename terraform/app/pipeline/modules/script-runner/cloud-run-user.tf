resource "google_service_account" "script_runner_cloud_run_account" {
  account_id   = "${var.app_env}-${var.app_live}-sr-run-account"
  display_name = "Message Replayer Cloud Run Account ${var.app_env}"
  project      = data.google_project.deadpendency_action_project.project_id
}

resource "google_project_iam_member" "script_runner_cloud_run_account_logging_writer" {
  project = data.google_project.deadpendency_action_project.project_id
  role    = "roles/logging.logWriter"
  member  = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
}

resource "google_project_iam_member" "script_runner_run_account_trace_writer" {
  project = data.google_project.deadpendency_action_project.project_id
  role    = "roles/cloudtrace.agent"
  member  = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
}

resource "google_project_iam_member" "script_runner_cloud_run_account_metric_writer" {
  project = data.google_project.deadpendency_action_project.project_id
  role    = "roles/monitoring.metricWriter"
  member  = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
}

resource "google_secret_manager_secret_iam_member" "script_runner_cloud_run_account_github_app_private_key_secret_reader" {
  project   = data.google_project.deadpendency_action_project.project_id
  secret_id = "${var.app_env}-github-app-private-key-secret"
  role      = "roles/secretmanager.secretAccessor"
  member    = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
}

## pubsub access

# init new run

resource "google_pubsub_subscription_iam_member" "init_queue_script_runner_subscription_member_account_a" {
  subscription = local.init_queue_topic_dlq_sub_id_a
  role         = "roles/editor"
  member       = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
  project      = data.google_project.deadpendency_action_project.project_id
}

resource "google_pubsub_subscription_iam_member" "init_queue_script_runner_subscription_member_account_b" {
  subscription = local.init_queue_topic_dlq_sub_id_b
  role         = "roles/editor"
  member       = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
  project      = data.google_project.deadpendency_action_project.project_id
  count        = var.app_env == "prod" ? 1 : 0
}

resource "google_pubsub_topic_iam_member" "script_runner_cloud_run_account_init_queue_pubsub_subscriber_a" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = local.init_queue_topic_dlq_id_a
  role    = "roles/pubsub.subscriber"
  member  = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
}

resource "google_pubsub_topic_iam_member" "script_runner_cloud_run_account_init_queue_pubsub_subscriber_b" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = local.init_queue_topic_dlq_id_b
  role    = "roles/pubsub.subscriber"
  member  = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
  count   = var.app_env == "prod" ? 1 : 0
}

resource "google_pubsub_topic_iam_member" "script_runner_cloud_run_account_init_queue_queue_pubsub_publisher" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.init_queue_topic_id
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
}

# check run created

resource "google_pubsub_subscription_iam_member" "check_run_created_script_runner_subscription_member_account_a" {
  subscription = local.check_run_created_queue_topic_dlq_sub_id_a
  role         = "roles/editor"
  member       = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
  project      = data.google_project.deadpendency_action_project.project_id
}

resource "google_pubsub_subscription_iam_member" "check_run_created_script_runner_subscription_member_account_b" {
  subscription = local.check_run_created_queue_topic_dlq_sub_id_b
  role         = "roles/editor"
  member       = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
  project      = data.google_project.deadpendency_action_project.project_id
  count        = var.app_env == "prod" ? 1 : 0
}

resource "google_pubsub_topic_iam_member" "script_runner_cloud_run_account_check_run_created_pubsub_subscriber_a" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = local.check_run_created_queue_topic_dlq_id_a
  role    = "roles/pubsub.subscriber"
  member  = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
}

resource "google_pubsub_topic_iam_member" "script_runner_cloud_run_account_check_run_created_pubsub_subscriber_b" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = local.check_run_created_queue_topic_dlq_id_b
  role    = "roles/pubsub.subscriber"
  member  = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
  count   = var.app_env == "prod" ? 1 : 0
}

resource "google_pubsub_topic_iam_member" "script_runner_cloud_run_account_check_run_created_queue_pubsub_publisher" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.check_run_created_queue_topic_id
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
}

# run prepared

resource "google_pubsub_subscription_iam_member" "run_prepared_script_runner_subscription_member_account_a" {
  subscription = local.run_prepared_queue_topic_dlq_sub_id_a
  role         = "roles/editor"
  member       = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
  project      = data.google_project.deadpendency_action_project.project_id
}

resource "google_pubsub_subscription_iam_member" "run_prepared_script_runner_subscription_member_account_b" {
  subscription = local.run_prepared_queue_topic_dlq_sub_id_b
  role         = "roles/editor"
  member       = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
  project      = data.google_project.deadpendency_action_project.project_id
  count        = var.app_env == "prod" ? 1 : 0
}

resource "google_pubsub_topic_iam_member" "script_runner_cloud_run_account_run_prepared_pubsub_subscriber_a" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = local.run_prepared_queue_topic_dlq_id_a
  role    = "roles/pubsub.subscriber"
  member  = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
}

resource "google_pubsub_topic_iam_member" "script_runner_cloud_run_account_run_prepared_pubsub_subscriber_b" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = local.run_prepared_queue_topic_dlq_id_b
  role    = "roles/pubsub.subscriber"
  member  = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
  count   = var.app_env == "prod" ? 1 : 0
}

resource "google_pubsub_topic_iam_member" "script_runner_cloud_run_account_run_prepared_queue_pubsub_publisher" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.run_prepared_queue_topic_id
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
}

# dependencies determined

resource "google_pubsub_subscription_iam_member" "dependencies_determined_script_runner_subscription_member_account_a" {
  subscription = local.dependencies_determined_queue_topic_dlq_sub_id_a
  role         = "roles/editor"
  member       = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
  project      = data.google_project.deadpendency_action_project.project_id
}

resource "google_pubsub_subscription_iam_member" "dependencies_determined_script_runner_subscription_member_account_b" {
  subscription = local.dependencies_determined_queue_topic_dlq_sub_id_b
  role         = "roles/editor"
  member       = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
  project      = data.google_project.deadpendency_action_project.project_id
  count        = var.app_env == "prod" ? 1 : 0
}

resource "google_pubsub_topic_iam_member" "script_runner_cloud_run_account_dependencies_determined_pubsub_subscriber_a" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = local.dependencies_determined_queue_topic_dlq_id_a
  role    = "roles/pubsub.subscriber"
  member  = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
}

resource "google_pubsub_topic_iam_member" "script_runner_cloud_run_account_dependencies_determined_pubsub_subscriber_b" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = local.dependencies_determined_queue_topic_dlq_id_b
  role    = "roles/pubsub.subscriber"
  member  = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
  count   = var.app_env == "prod" ? 1 : 0
}

resource "google_pubsub_topic_iam_member" "script_runner_cloud_run_account_dependencies_determined_queue_pubsub_publisher" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.dependencies_determined_queue_topic_id
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
}

# dependencies fetched

resource "google_pubsub_subscription_iam_member" "dependencies_fetched_script_runner_subscription_member_account_a" {
  subscription = local.dependencies_fetched_queue_topic_dlq_sub_id_a
  role         = "roles/editor"
  member       = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
  project      = data.google_project.deadpendency_action_project.project_id
}

resource "google_pubsub_subscription_iam_member" "dependencies_fetched_script_runner_subscription_member_account_b" {
  subscription = local.dependencies_fetched_queue_topic_dlq_sub_id_b
  role         = "roles/editor"
  member       = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
  project      = data.google_project.deadpendency_action_project.project_id
  count        = var.app_env == "prod" ? 1 : 0
}

resource "google_pubsub_topic_iam_member" "script_runner_cloud_run_account_dependencies_fetched_pubsub_subscriber_a" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = local.dependencies_fetched_queue_topic_dlq_id_a
  role    = "roles/pubsub.subscriber"
  member  = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
}

resource "google_pubsub_topic_iam_member" "script_runner_cloud_run_account_dependencies_fetched_pubsub_subscriber_b" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = local.dependencies_fetched_queue_topic_dlq_id_b
  role    = "roles/pubsub.subscriber"
  member  = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
  count   = var.app_env == "prod" ? 1 : 0
}

resource "google_pubsub_topic_iam_member" "script_runner_cloud_run_account_dependencies_fetched_queue_pubsub_publisher" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.dependencies_fetched_queue_topic_id
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
}

# report generated

resource "google_pubsub_subscription_iam_member" "report_generated_script_runner_subscription_member_account_a" {
  subscription = local.report_generated_queue_topic_dlq_id_sub_a
  role         = "roles/editor"
  member       = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
  project      = data.google_project.deadpendency_action_project.project_id
}

resource "google_pubsub_subscription_iam_member" "report_generated_script_runner_subscription_member_account_b" {
  subscription = local.report_generated_queue_topic_dlq_id_sub_b
  role         = "roles/editor"
  member       = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
  project      = data.google_project.deadpendency_action_project.project_id
  count        = var.app_env == "prod" ? 1 : 0
}

resource "google_pubsub_topic_iam_member" "script_runner_cloud_run_account_report_generated_pubsub_subscriber_a" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = local.report_generated_queue_topic_dlq_id_a
  role    = "roles/pubsub.subscriber"
  member  = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
}

resource "google_pubsub_topic_iam_member" "script_runner_cloud_run_account_report_generated_pubsub_subscriber_b" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = local.report_generated_queue_topic_dlq_id_b
  role    = "roles/pubsub.subscriber"
  member  = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
  count   = var.app_env == "prod" ? 1 : 0
}

resource "google_pubsub_topic_iam_member" "script_runner_cloud_run_account_report_generated_queue_pubsub_publisher" {
  project = data.google_project.deadpendency_action_project.project_id
  topic   = var.report_generated_queue_topic_id
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:${google_service_account.script_runner_cloud_run_account.email}"
}
