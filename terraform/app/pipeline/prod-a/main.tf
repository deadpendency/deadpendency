variable "APP_VERSION" {
  type = string
}

variable "APP_VERSION_SHA" {
  type = string
}

terraform {
  backend "gcs" {
    bucket      = "dgtw-products-deadpendency-action-tf-state-central1"
    prefix      = "app-prod-a"
    credentials = "/tmp/credentials.json"
  }

  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "~> 4.43.1"
    }
  }
}

provider "google" {
  region      = "us-central1"
  credentials = "/tmp/credentials.json"
}

data "google_project" "deadpendency_action_project" {
  project_id = "dgtw-deadpendency-action-2"
}

data "google_monitoring_notification_channel" "minor_notification_channel" {
  project      = data.google_project.deadpendency_action_project.project_id
  display_name = "Minor DA Alert"
}

data "google_monitoring_notification_channel" "critical_notification_channel" {
  project      = data.google_project.deadpendency_action_project.project_id
  display_name = "Critical DA Alert"
}

module "modules" {
  source = "../modules"

  app_version     = var.APP_VERSION
  app_version_sha = var.APP_VERSION_SHA
  app_env         = "prod"
  app_live        = "a"
  app_id          = "51614"

  minor_notification_channel    = data.google_monitoring_notification_channel.minor_notification_channel.name
  critical_notification_channel = data.google_monitoring_notification_channel.critical_notification_channel.name
}
