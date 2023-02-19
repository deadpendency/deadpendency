# init new run

resource "google_pubsub_subscription" "init_queue_script_runner_subscription_a" {
  name    = local.init_queue_topic_dlq_sub_id_a
  project = data.google_project.deadpendency_action_project.project_id
  topic   = local.init_queue_topic_dlq_id_a

  ack_deadline_seconds       = 60

  expiration_policy {
    ttl = ""
  }
}

resource "google_pubsub_subscription" "init_queue_script_runner_subscription_b" {
  name    = local.init_queue_topic_dlq_sub_id_b
  project = data.google_project.deadpendency_action_project.project_id
  topic   = local.init_queue_topic_dlq_id_b
  count   = var.app_env == "prod" ? 1 : 0

  ack_deadline_seconds       = 60

  expiration_policy {
    ttl = ""
  }
}

# check run created

resource "google_pubsub_subscription" "check_run_created_queue_script_runner_subscription_a" {
  name    = local.check_run_created_queue_topic_dlq_sub_id_a
  project = data.google_project.deadpendency_action_project.project_id
  topic   = local.check_run_created_queue_topic_dlq_id_a

  ack_deadline_seconds       = 60

  expiration_policy {
    ttl = ""
  }
}

resource "google_pubsub_subscription" "check_run_created_queue_script_runner_subscription_b" {
  name    = local.check_run_created_queue_topic_dlq_sub_id_b
  project = data.google_project.deadpendency_action_project.project_id
  topic   = local.check_run_created_queue_topic_dlq_id_b
  count   = var.app_env == "prod" ? 1 : 0

  ack_deadline_seconds       = 60

  expiration_policy {
    ttl = ""
  }
}

# run prepared

resource "google_pubsub_subscription" "run_prepared_queue_script_runner_subscription_a" {
  name    = local.run_prepared_queue_topic_dlq_sub_id_a
  project = data.google_project.deadpendency_action_project.project_id
  topic   = local.run_prepared_queue_topic_dlq_id_a

  ack_deadline_seconds       = 60

  expiration_policy {
    ttl = ""
  }
}

resource "google_pubsub_subscription" "run_prepared_queue_script_runner_subscription_b" {
  name    = local.run_prepared_queue_topic_dlq_sub_id_b
  project = data.google_project.deadpendency_action_project.project_id
  topic   = local.run_prepared_queue_topic_dlq_id_b
  count   = var.app_env == "prod" ? 1 : 0

  ack_deadline_seconds       = 60

  expiration_policy {
    ttl = ""
  }
}

# dependencies determined

resource "google_pubsub_subscription" "dependencies_determined_queue_script_runner_subscription_a" {
  name    = local.dependencies_determined_queue_topic_dlq_sub_id_a
  project = data.google_project.deadpendency_action_project.project_id
  topic   = local.dependencies_determined_queue_topic_dlq_id_a

  ack_deadline_seconds       = 60

  expiration_policy {
    ttl = ""
  }
}

resource "google_pubsub_subscription" "dependencies_determined_queue_script_runner_subscription_b" {
  name    = local.dependencies_determined_queue_topic_dlq_sub_id_b
  project = data.google_project.deadpendency_action_project.project_id
  topic   = local.dependencies_determined_queue_topic_dlq_id_b
  count   = var.app_env == "prod" ? 1 : 0

  ack_deadline_seconds       = 60

  expiration_policy {
    ttl = ""
  }
}

# dependencies fetched

resource "google_pubsub_subscription" "dependencies_fetched_queue_script_runner_subscription_a" {
  name    = local.dependencies_fetched_queue_topic_dlq_sub_id_a
  project = data.google_project.deadpendency_action_project.project_id
  topic   = local.dependencies_fetched_queue_topic_dlq_id_a

  ack_deadline_seconds       = 60

  expiration_policy {
    ttl = ""
  }
}

resource "google_pubsub_subscription" "dependencies_fetched_queue_script_runner_subscription_b" {
  name    = local.dependencies_fetched_queue_topic_dlq_sub_id_b
  project = data.google_project.deadpendency_action_project.project_id
  topic   = local.dependencies_fetched_queue_topic_dlq_id_b
  count   = var.app_env == "prod" ? 1 : 0

  ack_deadline_seconds       = 60

  expiration_policy {
    ttl = ""
  }
}

# report generated

resource "google_pubsub_subscription" "report_generated_queue_script_runner_subscription_a" {
  name    = local.report_generated_queue_topic_dlq_id_sub_a
  project = data.google_project.deadpendency_action_project.project_id
  topic   = local.report_generated_queue_topic_dlq_id_a

  ack_deadline_seconds       = 60

  expiration_policy {
    ttl = ""
  }
}

resource "google_pubsub_subscription" "report_generated_queue_script_runner_subscription_b" {
  name    = local.report_generated_queue_topic_dlq_id_sub_b
  project = data.google_project.deadpendency_action_project.project_id
  topic   = local.report_generated_queue_topic_dlq_id_b
  count   = var.app_env == "prod" ? 1 : 0

  ack_deadline_seconds       = 60

  expiration_policy {
    ttl = ""
  }
}
