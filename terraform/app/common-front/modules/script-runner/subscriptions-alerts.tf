resource "google_monitoring_alert_policy" "deadpendency_action_init_queue_sub_dlq_messages_a" {
  project      = data.google_project.deadpendency_action_project.project_id
  display_name = "${var.app_env}-a init queue dlq messages"
  combiner     = "OR"
  conditions {
    display_name = "${var.app_env}-a init queue dlq got any messages"
    condition_threshold {
      filter     = "metric.type=\"pubsub.googleapis.com/subscription/num_undelivered_messages\" AND resource.type=\"pubsub_subscription\" AND resource.label.subscription_id=\"${local.init_queue_topic_dlq_sub_id_a}\""
      duration   = "60s"
      comparison = "COMPARISON_GT"
      threshold_value = 0
      aggregations {
        alignment_period = "60s"
      }
    }
  }

  notification_channels = [
    var.minor_notification_channel
  ]
}

resource "google_monitoring_alert_policy" "deadpendency_action_init_queue_sub_dlq_messages_b" {
  project      = data.google_project.deadpendency_action_project.project_id
  display_name = "${var.app_env}-b init queue dlq messages"
  combiner     = "OR"
  conditions {
    display_name = "${var.app_env}-b init queue dlq got any messages"
    condition_threshold {
      filter     = "metric.type=\"pubsub.googleapis.com/subscription/num_undelivered_messages\" AND resource.type=\"pubsub_subscription\" AND resource.label.subscription_id=\"${local.init_queue_topic_dlq_sub_id_b}\""
      duration   = "60s"
      comparison = "COMPARISON_GT"
      threshold_value = 0
      aggregations {
        alignment_period = "60s"
      }
    }
  }

  notification_channels = [
    var.minor_notification_channel
  ]
}

resource "google_monitoring_alert_policy" "deadpendency_action_check_run_created_sub_dlq_messages_a" {
  project      = data.google_project.deadpendency_action_project.project_id
  display_name = "${var.app_env}-a check run created dlq messages"
  combiner     = "OR"
  conditions {
    display_name = "${var.app_env}-a check run created dlq got any messages"
    condition_threshold {
      filter     = "metric.type=\"pubsub.googleapis.com/subscription/num_undelivered_messages\" AND resource.type=\"pubsub_subscription\" AND resource.label.subscription_id=\"${local.check_run_created_queue_topic_dlq_sub_id_a}\""
      duration   = "60s"
      comparison = "COMPARISON_GT"
      threshold_value = 0
      aggregations {
        alignment_period = "60s"
      }
    }
  }

  notification_channels = [
    var.minor_notification_channel
  ]
}

resource "google_monitoring_alert_policy" "deadpendency_action_check_run_created_sub_dlq_messages_b" {
  project      = data.google_project.deadpendency_action_project.project_id
  display_name = "${var.app_env}-b check run created dlq messages"
  combiner     = "OR"
  conditions {
    display_name = "${var.app_env}-b check run created dlq got any messages"
    condition_threshold {
      filter     = "metric.type=\"pubsub.googleapis.com/subscription/num_undelivered_messages\" AND resource.type=\"pubsub_subscription\" AND resource.label.subscription_id=\"${local.check_run_created_queue_topic_dlq_sub_id_b}\""
      duration   = "60s"
      comparison = "COMPARISON_GT"
      threshold_value = 0
      aggregations {
        alignment_period = "60s"
      }
    }
  }

  notification_channels = [
    var.minor_notification_channel
  ]
}

resource "google_monitoring_alert_policy" "deadpendency_action_run_prepared_sub_dlq_messages_a" {
  project      = data.google_project.deadpendency_action_project.project_id
  display_name = "${var.app_env}-a run prepared dlq messages"
  combiner     = "OR"
  conditions {
    display_name = "${var.app_env}-a run prepared dlq got any messages"
    condition_threshold {
      filter     = "metric.type=\"pubsub.googleapis.com/subscription/num_undelivered_messages\" AND resource.type=\"pubsub_subscription\" AND resource.label.subscription_id=\"${local.run_prepared_queue_topic_dlq_sub_id_a}\""
      duration   = "60s"
      comparison = "COMPARISON_GT"
      threshold_value = 0
      aggregations {
        alignment_period = "60s"
      }
    }
  }

  notification_channels = [
    var.minor_notification_channel
  ]
}

resource "google_monitoring_alert_policy" "deadpendency_action_run_prepared_sub_dlq_messages_b" {
  project      = data.google_project.deadpendency_action_project.project_id
  display_name = "${var.app_env}-b run prepared dlq messages"
  combiner     = "OR"
  conditions {
    display_name = "${var.app_env}-b run prepared dlq got any messages"
    condition_threshold {
      filter     = "metric.type=\"pubsub.googleapis.com/subscription/num_undelivered_messages\" AND resource.type=\"pubsub_subscription\" AND resource.label.subscription_id=\"${local.run_prepared_queue_topic_dlq_sub_id_b}\""
      duration   = "60s"
      comparison = "COMPARISON_GT"
      threshold_value = 0
      aggregations {
        alignment_period = "60s"
      }
    }
  }

  notification_channels = [
    var.minor_notification_channel
  ]
}

resource "google_monitoring_alert_policy" "deadpendency_action_dependencies_determined_sub_dlq_messages_a" {
  project      = data.google_project.deadpendency_action_project.project_id
  display_name = "${var.app_env}-a dependencies determined dlq messages"
  combiner     = "OR"
  conditions {
    display_name = "${var.app_env}-a dependencies determined dlq got any messages"
    condition_threshold {
      filter     = "metric.type=\"pubsub.googleapis.com/subscription/num_undelivered_messages\" AND resource.type=\"pubsub_subscription\" AND resource.label.subscription_id=\"${local.dependencies_determined_queue_topic_dlq_sub_id_a}\""
      duration   = "60s"
      comparison = "COMPARISON_GT"
      threshold_value = 0
      aggregations {
        alignment_period = "60s"
      }
    }
  }

  notification_channels = [
    var.minor_notification_channel
  ]
}

resource "google_monitoring_alert_policy" "deadpendency_action_dependencies_determined_sub_dlq_messages_b" {
  project      = data.google_project.deadpendency_action_project.project_id
  display_name = "${var.app_env}-b dependencies determined dlq messages"
  combiner     = "OR"
  conditions {
    display_name = "${var.app_env}-b dependencies determined dlq got any messages"
    condition_threshold {
      filter     = "metric.type=\"pubsub.googleapis.com/subscription/num_undelivered_messages\" AND resource.type=\"pubsub_subscription\" AND resource.label.subscription_id=\"${local.dependencies_determined_queue_topic_dlq_sub_id_b}\""
      duration   = "60s"
      comparison = "COMPARISON_GT"
      threshold_value = 0
      aggregations {
        alignment_period = "60s"
      }
    }
  }

  notification_channels = [
    var.minor_notification_channel
  ]
}

resource "google_monitoring_alert_policy" "deadpendency_action_dependencies_fetched_sub_dlq_messages_a" {
  project      = data.google_project.deadpendency_action_project.project_id
  display_name = "${var.app_env}-a dependencies fetched dlq messages"
  combiner     = "OR"
  conditions {
    display_name = "${var.app_env}-a dependencies fetched dlq got any messages"
    condition_threshold {
      filter     = "metric.type=\"pubsub.googleapis.com/subscription/num_undelivered_messages\" AND resource.type=\"pubsub_subscription\" AND resource.label.subscription_id=\"${local.dependencies_fetched_queue_topic_dlq_sub_id_a}\""
      duration   = "60s"
      comparison = "COMPARISON_GT"
      threshold_value = 0
      aggregations {
        alignment_period = "60s"
      }
    }
  }

  notification_channels = [
    var.minor_notification_channel
  ]
}

resource "google_monitoring_alert_policy" "deadpendency_action_dependencies_fetched_sub_dlq_messages_b" {
  project      = data.google_project.deadpendency_action_project.project_id
  display_name = "${var.app_env}-b dependencies fetched dlq messages"
  combiner     = "OR"
  conditions {
    display_name = "${var.app_env}-b dependencies fetched dlq got any messages"
    condition_threshold {
      filter     = "metric.type=\"pubsub.googleapis.com/subscription/num_undelivered_messages\" AND resource.type=\"pubsub_subscription\" AND resource.label.subscription_id=\"${local.dependencies_fetched_queue_topic_dlq_sub_id_b}\""
      duration   = "60s"
      comparison = "COMPARISON_GT"
      threshold_value = 0
      aggregations {
        alignment_period = "60s"
      }
    }
  }

  notification_channels = [
    var.minor_notification_channel
  ]
}

resource "google_monitoring_alert_policy" "deadpendency_action_report_generated_sub_dlq_messages_a" {
  project      = data.google_project.deadpendency_action_project.project_id
  display_name = "${var.app_env}-a report generated dlq messages"
  combiner     = "OR"
  conditions {
    display_name = "${var.app_env}-a report generated dlq got any messages"
    condition_threshold {
      filter     = "metric.type=\"pubsub.googleapis.com/subscription/num_undelivered_messages\" AND resource.type=\"pubsub_subscription\" AND resource.label.subscription_id=\"${local.report_generated_queue_topic_dlq_id_sub_a}\""
      duration   = "60s"
      comparison = "COMPARISON_GT"
      threshold_value = 0
      aggregations {
        alignment_period = "60s"
      }
    }
  }

  notification_channels = [
    var.minor_notification_channel
  ]
}

resource "google_monitoring_alert_policy" "deadpendency_action_report_generated_sub_dlq_messages_b" {
  project      = data.google_project.deadpendency_action_project.project_id
  display_name = "${var.app_env}-b report generated dlq messages"
  combiner     = "OR"
  conditions {
    display_name = "${var.app_env}-b report generated dlq got any messages"
    condition_threshold {
      filter     = "metric.type=\"pubsub.googleapis.com/subscription/num_undelivered_messages\" AND resource.type=\"pubsub_subscription\" AND resource.label.subscription_id=\"${local.report_generated_queue_topic_dlq_id_sub_b}\""
      duration   = "60s"
      comparison = "COMPARISON_GT"
      threshold_value = 0
      aggregations {
        alignment_period = "60s"
      }
    }
  }

  notification_channels = [
    var.minor_notification_channel
  ]
}
