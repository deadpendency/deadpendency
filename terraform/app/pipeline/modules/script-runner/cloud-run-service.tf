resource "google_cloud_run_service" "deadpendency_action_script_runner" {
  project  = data.google_project.deadpendency_action_project.project_id
  name     = "${var.app_env}-${var.app_live}-deadpendency-action-script-runner"
  location = "us-central1"

  template {
    spec {
      containers {
        image = "gcr.io/dgtw-deadpendency-action-2/deadpendency-action-script-runner:${var.app_version_sha}"
        env {
          name  = "INIT_QUEUE"
          value = var.init_queue_topic_id
        }
        env {
          name  = "INIT_QUEUE_DLQ_SUB_ID_A"
          value = local.init_queue_topic_dlq_sub_id_a
        }
        env {
          name  = "INIT_QUEUE_DLQ_SUB_ID_B"
          value = local.init_queue_topic_dlq_sub_id_b
        }
        env {
          name  = "CHECK_RUN_CREATED_QUEUE"
          value = var.check_run_created_queue_topic_id
        }
        env {
          name  = "CHECK_RUN_CREATED_QUEUE_DLQ_SUB_ID_A"
          value = local.check_run_created_queue_topic_dlq_sub_id_a
        }
        env {
          name  = "CHECK_RUN_CREATED_QUEUE_DLQ_SUB_ID_B"
          value = local.check_run_created_queue_topic_dlq_sub_id_b
        }
        env {
          name  = "RUN_PREPARED_QUEUE"
          value = var.run_prepared_queue_topic_id
        }
        env {
          name  = "RUN_PREPARED_QUEUE_DLQ_SUB_ID_A"
          value = local.run_prepared_queue_topic_dlq_sub_id_a
        }
        env {
          name  = "RUN_PREPARED_QUEUE_DLQ_SUB_ID_B"
          value = local.run_prepared_queue_topic_dlq_sub_id_b
        }
        env {
          name  = "DEPENDENCIES_DETERMINED_QUEUE"
          value = var.dependencies_determined_queue_topic_id
        }
        env {
          name  = "DEPENDENCIES_DETERMINED_QUEUE_DLQ_SUB_ID_A"
          value = local.dependencies_determined_queue_topic_dlq_sub_id_a
        }
        env {
          name  = "DEPENDENCIES_DETERMINED_QUEUE_DLQ_SUB_ID_B"
          value = local.dependencies_determined_queue_topic_dlq_sub_id_b
        }
        env {
          name  = "DEPENDENCIES_FETCHED_QUEUE"
          value = var.dependencies_fetched_queue_topic_id
        }
        env {
          name  = "DEPENDENCIES_FETCHED_QUEUE_DLQ_SUB_ID_A"
          value = local.dependencies_fetched_queue_topic_dlq_sub_id_a
        }
        env {
          name  = "DEPENDENCIES_FETCHED_QUEUE_DLQ_SUB_ID_B"
          value = local.dependencies_fetched_queue_topic_dlq_sub_id_b
        }
        env {
          name  = "REPORT_GENERATED_QUEUE"
          value = var.report_generated_queue_topic_id
        }
        env {
          name  = "REPORT_GENERATED_QUEUE_DLQ_SUB_ID_A"
          value = local.report_generated_queue_topic_dlq_id_sub_a
        }
        env {
          name  = "REPORT_GENERATED_QUEUE_DLQ_SUB_ID_B"
          value = local.report_generated_queue_topic_dlq_id_sub_b
        }
        env {
          name  = "GITHUB_PRIVATE_KEY_SECRET_NAME"
          value = "${var.app_env}-github-app-private-key-secret"
        }
        env {
          name  = "APP_ID"
          value = var.app_id
        }
        env {
          name  = "APP_ENV"
          value = var.app_env
        }
        env {
          name  = "APP_VERSION"
          value = var.app_version
        }
        env {
          name  = "APP_LIVE"
          value = var.app_live
        }
      }
      service_account_name = google_service_account.script_runner_cloud_run_account.email

      timeout_seconds = 900
    }

    metadata {
      labels = {
        app_env = var.app_env
      }
    }
  }

  traffic {
    percent         = 100
    latest_revision = true
  }
}
