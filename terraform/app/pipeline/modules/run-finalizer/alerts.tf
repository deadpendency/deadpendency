resource "google_monitoring_alert_policy" "deadpendency_action_processing_failure_sub_dlq_messages" {
  project      = data.google_project.deadpendency_action_project.project_id
  display_name = "${var.app_env}-${var.app_live} processing failure dlq messages"
  combiner     = "OR"
  conditions {
    display_name = "${var.app_env}-${var.app_live} processing failure dlq got any messages"
    condition_threshold {
      filter     = "metric.type=\"pubsub.googleapis.com/subscription/dead_letter_message_count\" AND resource.type=\"pubsub_subscription\" AND resource.label.subscription_id=\"${google_pubsub_subscription.processing_failure_queue_run_finalizer_subscription.name}\""
      duration   = "60s"
      comparison = "COMPARISON_GT"
      threshold_value = 0
      aggregations {
        alignment_period = "60s"
        per_series_aligner = "ALIGN_COUNT"
      }
    }
  }

  notification_channels = [
    var.minor_notification_channel
  ]
}
