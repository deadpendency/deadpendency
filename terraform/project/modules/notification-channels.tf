resource "google_monitoring_notification_channel" "critical_da_alert" {
  project      = google_project.deadpendency_project.project_id
  display_name = "Critical DA Alert"
  type         = "pagerduty"

  sensitive_labels {
    // removed as this is sensitive
    service_key = "1234"
  }
}

resource "google_monitoring_notification_channel" "minor_da_alert" {
  project      = google_project.deadpendency_project.project_id
  display_name = "Minor DA Alert"
  type         = "pagerduty"

  sensitive_labels {
    // removed as this is sensitive
    service_key = "1234"
  }
}

resource "google_monitoring_notification_channel" "dev_da_alert" {
  project      = google_project.deadpendency_project.project_id
  display_name = "Dev DA Alert"
  type         = "pagerduty"

  sensitive_labels {
    // removed as this is sensitive
    service_key = "1234"
  }
}
