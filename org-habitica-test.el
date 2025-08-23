(require 'org-habitica)

(org-habitica-feed-pet "Dragon-RoyalPurple" "Honey")
(org-habitica-feed-pet "Cactus-Desert" "Potatoe")
(org-habitica-feed-pet "Fox-Golden" "Honey")
(org-habitica-get-member-profile org-habitica-api-user)
(org-habitica-get-items)
(org-habitica-get-specific-items 'food)
(org-habitica-get-specific-items 'pets)
(org-habitica-get-specific-items 'mounts)
(org-habitica-feed-all-pets)
(org-habitica--pet-type-from-name "LionCub-Shade")
(org-habitica-get-food-for-pet "LionCub-hade")
(org-habitica-get-food-for-pet "LionCub-Shade")
(org-habitica-hatch-all-possible-pets)
(org-habitica-sell-extra-items)

(org-habitica-get-all-tasks-from-habitica)

(setq task-id-done-in-org-not-complete-in-habitica "8f694922-ba27-4f9f-a5a6-934236ddf479")
(setq found-task-done-in-org-not-complete-in-habitica (org-habitica--get-task-from-habitica-by-id-and-format-response task-id-done-in-org-not-complete-in-habitica))

(setq task-id-project-in-org-not-complete-in-habitica "5130246a-050e-4bfe-8633-73b9ebde6498")
(setq found-task-project-in-org-not-complete-in-habitica (org-habitica--get-task-from-habitica-by-id-and-format-response task-id-project-in-org-not-complete-in-habitica))



(defun org-habitica-test-org-habitica--get-changed-data ()
  "Test org-habitica--get-changed-data function."
  (let* ((task-id-done-in-org-not-complete-in-habitica "8f694922-ba27-4f9f-a5a6-934236ddf479")
	 (task-id-project-in-org-not-complete-in-habitica "5130246a-050e-4bfe-8633-73b9ebde6498")
	 (found-task-done-in-org-not-complete-in-habitica (org-habitica--format-response-from-buffer-if-success (org-habitica--get-task-from-habitica task-id-done-in-org-not-complete-in-habitica)))
	 (found-task-project-in-org-not-complete-in-habitica (org-habitica--format-response-from-buffer-if-success (org-habitica--get-task-from-habitica task-id-project-in-org-not-complete-in-habitica))))
    (equal (org-habitica--get-changed-data found-task-done-in-org-not-complete-in-habitica 1.5 "per") (list (cons "priority" 1.5)))))

(org-habitica-test-org-habitica--get-changed-data)
(org-habitica--get-changed-data found-task-done-in-org-not-complete-in-habitica 1.5 "int")
(org-habitica--get-changed-data found-task-done-in-org-not-complete-in-habitica 0.1 "int")
(org-habitica--get-priority-from-task found-task-done-in-org-not-complete-in-habitica)
(org-habitica-delete-all-todos)
