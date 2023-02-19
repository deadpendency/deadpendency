resource "google_project_iam_member" "tf_deadpendency_action_admin_editor" {
  project = google_project.deadpendency_project.project_id
  role    = "roles/owner"
  member  = "user:admin@deadgotowork.com"
}
