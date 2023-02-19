resource "google_monitoring_metric_descriptor" "total_installations_metric" {
  project      = data.google_project.deadpendency_action_project.project_id
  description  = "Total installations of the app"
  display_name = "${var.app_env}-total-installations"
  type         = "custom.googleapis.com/deadpendency_action/${var.app_env}_total_installations"
  metric_kind  = "GAUGE"
  value_type   = "INT64"
  labels {
      key = "plan_type"
      value_type = "STRING"
  }
  count = var.app_env == "prod" ? 1 : 0
}

resource "google_monitoring_metric_descriptor" "app_install_metric" {
  project      = data.google_project.deadpendency_action_project.project_id
  description  = "App installations"
  display_name = "${var.app_env}-app-installations"
  type         = "custom.googleapis.com/deadpendency_action/${var.app_env}_app_installations"
  metric_kind  = "GAUGE"
  value_type   = "INT64"
  labels {
      key = "plan_type"
      value_type = "STRING"
  }
  labels {
      key = "login"
      value_type = "STRING"
  }
  count = var.app_env == "prod" ? 1 : 0
}

resource "google_monitoring_metric_descriptor" "app_cancelled_metric" {
  project      = data.google_project.deadpendency_action_project.project_id
  description  = "App cancellations"
  display_name = "${var.app_env}-app-cancellations"
  type         = "custom.googleapis.com/deadpendency_action/${var.app_env}_app_cancellations"
  metric_kind  = "GAUGE"
  value_type   = "INT64"
  labels {
      key = "plan_type"
      value_type = "STRING"
  }
  labels {
      key = "login"
      value_type = "STRING"
  }
  count = var.app_env == "prod" ? 1 : 0
}

resource "google_monitoring_metric_descriptor" "smoke_success_metric" {
  project      = data.google_project.deadpendency_action_project.project_id
  description  = "Smoke test is passing"
  display_name = "${var.app_env}-smoke-success"
  type         = "custom.googleapis.com/deadpendency_action/${var.app_env}_smoke_success"
  metric_kind  = "GAUGE"
  value_type   = "BOOL"
}

# NO SMOKE
# resource "google_monitoring_alert_policy" "smoke_success_metric_alert" {
#   project      = data.google_project.deadpendency_action_project.project_id
#   display_name = "${var.app_env} smoke success"
#   combiner     = "OR"
#   conditions {
#     display_name = "${var.app_env} smoke success getting true"
#     condition_threshold {
#       filter     = "metric.type=\"custom.googleapis.com/deadpendency_action/${var.app_env}_smoke_success\" AND resource.type=\"global\""
#       duration   = "60s"
#       comparison = "COMPARISON_LT"
#       threshold_value = 1
#       aggregations {
#         alignment_period   = "1500s" # 25 mins
#         per_series_aligner = "ALIGN_COUNT_TRUE"
#       }
#     }
#   }

#   conditions {
#     display_name = "${var.app_env} smoke success getting data"
#     condition_absent {
#       filter     = "metric.type=\"custom.googleapis.com/deadpendency_action/${var.app_env}_smoke_success\" AND resource.type=\"global\""
#       duration   = "1500s" # 25 mins
#     }
#   }

#   enabled = var.app_env == "prod" ? true : false

#   notification_channels = [
#     var.critical_notification_channel
#   ]
# }
