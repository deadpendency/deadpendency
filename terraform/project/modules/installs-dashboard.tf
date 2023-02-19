resource "google_monitoring_dashboard" "installs_dashboard" {
  project        = google_project.deadpendency_project.project_id
  dashboard_json = <<EOF
{
  "category": "CUSTOM",
  "displayName": "Deadpendency Installs",
  "mosaicLayout": {
    "columns": 2,
    "tiles": [
      {
        "height": 1,
        "widget": {
          "title": "App Installations",
          "xyChart": {
            "chartOptions": {
              "mode": "COLOR"
            },
            "dataSets": [
              {
                "minAlignmentPeriod": "1800s",
                "plotType": "STACKED_AREA",
                "targetAxis": "Y1",
                "timeSeriesQuery": {
                  "apiSource": "DEFAULT_CLOUD",
                  "timeSeriesFilter": {
                    "aggregation": {
                      "alignmentPeriod": "1800s",
                      "crossSeriesReducer": "REDUCE_NONE",
                      "perSeriesAligner": "ALIGN_INTERPOLATE"
                    },
                    "filter": "metric.type=\"custom.googleapis.com/deadpendency_action/prod_total_installations\" resource.type=\"global\"",
                    "secondaryAggregation": {
                      "alignmentPeriod": "60s",
                      "crossSeriesReducer": "REDUCE_NONE",
                      "perSeriesAligner": "ALIGN_MEAN"
                    }
                  }
                }
              }
            ],
            "timeshiftDuration": "0s",
            "yAxis": {
              "label": "y1Axis",
              "scale": "LINEAR"
            }
          }
        },
        "width": 1,
        "xPos": 0,
        "yPos": 0
      },
      {
        "height": 1,
        "widget": {
          "timeSeriesTable": {
            "dataSets": [
              {
                "minAlignmentPeriod": "60s",
                "tableDisplayOptions": {
                    "shownColumns": [
                        "login",
                        "plan_type"
                    ]
                },
                "tableTemplate": "$${metric.labels.login}",
                "timeSeriesQuery": {
                  "timeSeriesFilter": {
                    "aggregation": {
                      "alignmentPeriod": "60s",
                      "crossSeriesReducer": "REDUCE_NONE",
                      "perSeriesAligner": "ALIGN_MEAN"
                    },
                    "filter": "metric.type=\"custom.googleapis.com/deadpendency_action/prod_app_installations\" resource.type=\"global\""
                  }
                }
              }
            ]
          },
          "title": "Recent Installations"
        },
        "width": 1,
        "xPos": 1,
        "yPos": 0
      },
      {
        "height": 1,
        "widget": {
          "timeSeriesTable": {
            "dataSets": [
              {
                "minAlignmentPeriod": "60s",
                "tableDisplayOptions": {
                    "shownColumns": [
                        "login",
                        "plan_type"
                    ]
                },
                "timeSeriesQuery": {
                  "timeSeriesFilter": {
                    "aggregation": {
                      "alignmentPeriod": "60s",
                      "crossSeriesReducer": "REDUCE_NONE",
                      "perSeriesAligner": "ALIGN_MEAN"
                    },
                    "filter": "metric.type=\"custom.googleapis.com/deadpendency_action/prod_app_cancellations\" resource.type=\"global\""
                  }
                }
              }
            ]
          },
          "title": "Recent Cancellations"
        },
        "width": 1,
        "xPos": 0,
        "yPos": 1
      }
    ]
  }
}
EOF

}
