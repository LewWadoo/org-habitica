;;; org-habitica.el --- Habitica integration for org-mode

;; Author: Evgeny Mikhaylov <lewwadoo@gmail.com>
;; Created: [2016-07-15]
;; Version: 20210411
;; Keywords: calendar

;; Copyright (c) Evgeny Mikhaylov
;; This program is free software: you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation, either version 3 of the License, or
;;     (at your option) any later version.

;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.

;;     You should have received a copy of the GNU General Public License
;;     along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Package-Requires: ((json "1.4") (org))

;;; Commentary:

;; This package provides an integration between org-mode and Habitica.  When a state of a TODO task in org-mode is changed, the task gets created, its "completed" state is updated or it gets deleted from Habitica.  The action depends on the final state of a task.

;;; Installation
;; Copy this file (org-habitica.el) to your machine

;; Copy lines between "begin feature 'org-habitica" and "end feature 'org-habitica" to your .emacs file:

					; begin feature 'org-habitica
;; (add-to-list 'load-path "<PATH-TO-YOUR-ORG-HABITICA-FILE>/org-habitica/")

;; (defun org-habitica-unload-feature ()
;;   "Unload org-habitica."
;;   (interactive)
;;   (global-unset-key (kbd "C-c u"))
;;   (global-unset-key (kbd "C-c C-x h"))
;;   (unload-feature 'org-habitica)
;;   (message "feature org-habitica unloaded"))

;; (defun org-habitica-load-feature ()
;;   "Load org-habitica."
;;   (interactive)
;;   (require 'org-habitica)
;;   (global-set-key (kbd "C-c u") 'org-habitica-unload-feature)
;;   (global-set-key (kbd "C-c C-x h") 'org-habitica-sync-task)
;;   (add-hook 'org-after-todo-state-change-hook 'org-habitica-sync-task) ;; org-after-todo-state-change-hook is from org.el
;;   (setq org-habitica-api-user "<PUT-YOUR-ID-HERE>")
;;   (setq org-habitica-api-token "<PUT-YOUR-TOKEN-HERE>")
;;   (org-habitica-check-access))

;; (global-set-key (kbd "C-c h") 'org-habitica-load-feature)

					; end feature 'org-habitica

;; Then get your User ID and API Token which can be found on thepage with API settings when you are logged in to your Habitica account: https://habitica.com/#/options/settings/api and replace:
;; <PUT-YOUR-ID-HERE> with your User ID;
;; <PUT-YOUR-TOKEN-HERE> with your API Token.

;; Uncomment just inserted lines in your .emacs that start with ;;

;; Finally to start using the feature reload your Emacs and type:
;; C-c h
;; To unload the feature type:
;; C-c u

;; Requires
(require 'json)
(require 'org)

;; Possible keys to consider:
;; C-c C-x u ­ add or update a task
;; C-c C-x d ­ delete a task
;; C-c C-x o ­ enable org-habitica
;; C-u C-c C-x o ­ disable org-habitica

;;; Code:

(defvar org-habitica-request-method-get "GET")
(defvar org-habitica-request-method-post "POST")
(defvar org-habitica-request-method-delete "DELETE")
(defvar org-habitica-request-method-update "UPDATE")
(defvar org-habitica-request-method-put "PUT")

(defvar org-habitica--neither-todo-nor-done-keywords '("WAIT" "MAYBE" "HOLD" "CANCELED" "FAILED" "DELEGATED" nil)
  "Org states which delete a task in Habitica when switching to.")

(defvar org-habitica--response nil
  "Contain the data returned by a request.")

(defvar org-habitica--buffer nil
  "Current org buffer.")

(setq org-habitica-habit-buttons (list "up" "down"))

(setq org-habitica-attributes (list "int" "str" "con" "per"))

(setq org-habitica-types (list "habit" "daily" "todo" "reward"))

(setq org-habitica-priorities (list "trivial" "easy" "medium" "hard"))

(setq org-habitica-priority-values (list (cons (car org-habitica-priorities) 0.1) (cons (cadr org-habitica-priorities) 1) (cons (caddr org-habitica-priorities) 1.5) (cons (cadddr org-habitica-priorities) 2)))

(defcustom org-habitica--api-url "https://habitica.com/api/v3"
  "API url.")

(defcustom org-habitica-api-user nil
  "API user id.")

(defcustom org-habitica-api-token nil
  "API token.")

(defcustom org-habitica--id-property-name "HABITICA_ID"
  "Property id name of an org task.")

(defconst org-habitica--score-direction-down "down"
  "Negative score direction of a task.
In Habitica it means that a todo task's state turns from DONE to TODO.")

(defconst org-habitica--score-direction-up "up"
  "Positive score direction of a task.
In Habitica it means that a todo task's state turns from TODO to DONE.")

;;;###autoload
(defun org-habitica-check-access ()
  "Check Habitica's API status."
  (message "Connection to Habitica is %s" (org-habitica--get-from-data 'status (org-habitica--format-response-from-buffer-if-success (org-habitica--send-request org-habitica-request-method-get "/status")))))

(defun org-habitica-get-member-profile ()
  "Get a member profile."
  (org-habitica--send-request org-habitica-request-method-get (concat "/members/" org-habitica-api-user)))

(defun org-habitica--entry-is-neither-todo-nor-done-p (state)
  "Check whether the task was discarded according to its STATE."
  (member state org-habitica--neither-todo-nor-done-keywords))

(defun org-habitica--get-data-from-response (response)
  "Extract DATA from returned request RESPONSE."
  (cdr (assoc 'data response)))

(defun org-habitica--get-from-data (symbol response)
  "Extract SYMBOL from returned request RESPONSE."
  (cdr (assoc symbol (org-habitica--get-data-from-response response))))

(defun org-habitica--get-id-from-org (buffer)
  "Get id property of an org task in BUFFER."
  (save-current-buffer
    (set-buffer buffer)
    (org-entry-get (point) org-habitica--id-property-name)))

(defun org-habitica--set-id (id)
  "Set ID as a property of an org task."
  (save-current-buffer
    (set-buffer org-habitica--buffer)
    (org-set-property org-habitica--id-property-name id)))

(defun org-habitica--was-response-successful-p (response)
  "Return t if the RESPONSE was successful."
  (not (equal
	(assoc-default 'success response)
	json-false)))

(defun org-habitica--get-error-from-response (response)
  "Return error from RESPONSE."
  (unless (org-habitica--was-response-successful-p response)
    (assoc-default 'error response)))

(defun org-habitica--format-status-buffer (status)
  (message "debug! Buffer: %s" (buffer-substring-no-properties (point-min) (point-max)))
  (kill-buffer (current-buffer))
  )

(defun org-habitica--format-response-from-buffer (buffer)
  "Format BUFFER data from json to a list."
  ;; (let ((response-buffer buffer))
  (when (not (bufferp buffer))
    ;; (setq response-buffer (current-buffer)))
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (goto-char url-http-end-of-headers)
    (message "buffer: %s" (buffer-string)) ;; (point) (point-max))
    (encode-coding-region (point) (point-max) 'iso-8859-1)
    (decode-coding-region (point) (point-max) 'utf-8)
    (let
	((json-object-type 'alist)
	  (json-key-type 'symbol)
	  ;; (json-false :json-false)
	  (json-array-type 'list))
      (setq org-habitica--response (json-read))
      ;; (setq text (org-habitica--get-from-data 'text (org-habitica--get-data-from-response org-habitica--response)))
      ;; (message "text: %s" text)
      ;; (message "text iso: %s" (encode-coding-string text 'iso-8859-1))
      ;; (message "text utf: %s" (decode-coding-string text 'utf-8))
      ))
  (kill-buffer buffer)
  org-habitica--response)

(defun org-habitica--create-task-get-id (caption &optional notes priority-value attribute type habit-buttons)
  "Create task by TYPE with CAPTION, NOTES, PRIORITY and STATE in Habitica and return its id."
  (org-habitica--get-from-data 'id (org-habitica--format-response-from-buffer (org-habitica--create-task caption notes priority-value attribute type habit-buttons))))

(defun org-habitica--create-set-id-and-score-task (caption state notes &optional priority-value attribute type habit-buttons)
  "Create task with CAPTION, NOTES and PRIORITY-VALUE, set returned id for org task and score up if the STATE is done."
  (let* ((priority-value (or priority-value (org-habitica--get-priority-value-by-name (car org-habitica-priorities))))
	 (task-id (org-habitica--create-task-get-id caption notes priority-value attribute type habit-buttons)))
    (org-habitica--set-id task-id)
    (if (org-habitica--entry-is-neither-todo-nor-done-p state)
      (org-habitica--delete-task task-id)
	;; (org-habitica--add-tag-by-name-to-task task-id state)
      (org-habitica--score-task-by-state task-id nil state type))))

(defun org-habitica--send-request-with-user-key (method api-url-end &optional data extra-headers)
  "Send request DATA with possibly EXTRA-HEADERS by METHOD to url ending with API-URL-END."
  (let* ((url-request-method method)
         (url (concat org-habitica--api-url api-url-end))
	 (url-request-data
	  (when data
	      (encode-coding-string (json-encode data) 'utf-8)))
         (url-request-extra-headers 
	  (list
	   (cons "Content-Type" "application/json")
	   ;; (cons "Content-Length" (number-to-string (length url-request-data)))
	   (cons "X-Client" (concat org-habitica-api-user "-org-habitica")) 
	   (cons "X-API-User" org-habitica-api-user) 
	   (cons "X-API-Key" org-habitica-api-token)))
	 (url-request-extra-headers
	  (if extra-headers
	      (cons extra-headers url-request-extra-headers)
	    url-request-extra-headers)))
    (url-retrieve-synchronously url)))
    ;; (url-retrieve url 'org-habitica--format-status-buffer))))
    ;; (with-temp-buffer (url-retrieve-synchronously url) (json-read))))

(defun org-habitica--send-request (method api-url-end)
  "Send url request without authentification."
  (let ((url-request-method method)
        (url (concat org-habitica--api-url api-url-end))
        (url-request-extra-headers 
	 (list (cons "Content-Type" "application/json"))))
    (url-retrieve-synchronously url)))

(defun org-habitica-get-all-tasks-from-habitica ()
  "Get all tasks and rewards from Habitica."
  (org-habitica--get-data-from-response (format-response-from-buffer (org-habitica--send-request-with-user-key org-habitica-request-method-get "/tasks/user" nil))))

(defun org-habitica-get-all-of-type-from-habitica (type)
  "Get all of TYPE from Habitica."
  (org-habitica--get-data-from-response (format-response-from-buffer (org-habitica--send-request-with-user-key org-habitica-request-method-get (concat "/tasks/user" "?type=" type) nil))))

(defun org-habitica-create-tasks-from-response (response)
  "Create tasks in Org Mode from RESPONSE."
  )

(defun org-habitica--get-task-from-habitica (id)
  "Get task from Habitica by its ID.
Return a buffer with json response."
  (org-habitica--send-request-with-user-key org-habitica-request-method-get (concat "/tasks/" id) nil))

(defun org-habitica--score-task (id direction)
  "Score a task with ID up or down depending on DIRECTION."
  (org-habitica--format-response-from-buffer (org-habitica--send-request-with-user-key org-habitica-request-method-post (concat "/tasks/" id "/score/" direction) nil (cons "Content-Length" "0"))))

(defun org-habitica--convert-from-org-to-markdown (text)
  "Convert TEXT from org to markdown."
  (let ((org-export-show-temporary-export-buffer nil))
    (with-temp-buffer (insert text)
		      (org-md-export-as-markdown)
		      (with-current-buffer (get-buffer "*Org MD Export*")
			(search-forward "\n\n\n\n" nil t)
			(buffer-substring (point) (- (point-max) 2))))))

(defun org-habitica--create-task (caption &optional notes priority-value attribute type habit-buttons)
  "Create task with CAPTION, NOTES, PRIORITY-VALUE and STATE in Habitica."
  (let* ((api-url-end "/tasks/user")
	(request-data 
	 (list 
	  (cons "type" (or type (nth 2 org-habitica-types)))
	  (cons "text" (org-habitica--convert-from-org-to-markdown caption))
	  (cons "priority" (or priority-value (org-habitica--get-priority-value-by-name (car org-habitica-priorities))))
	  (cons "notes" notes)))
	(attribute (or attribute (cadddr org-habitica-attributes)))
	(request-data
	     (cons (cons "attribute" attribute) request-data))
	(request-data
	 (if (equal type (car org-habitica-types))
	     (cons (cons "up" (not (not (member "up" habit-buttons)))) request-data)
	   request-data))
	(request-data
	 (if (equal type (car org-habitica-types))
	     (cons (cons "down" (not (not (member "down" habit-buttons)))) request-data)
	   request-data)))
    (org-habitica--send-request-with-user-key org-habitica-request-method-post api-url-end request-data)))

(defun org-habitica--delete-task (id)
  "Delete a task by its ID in Habitica."
  (org-habitica--send-request-with-user-key org-habitica-request-method-delete (concat "/tasks/" id) nil (cons "taskId" id)))

(defun org-habitica--get-all-tags-from-habitica ()
  "Get list of all tags from Habitica."
  (org-habitica--get-data-from-response (org-habitica--format-response-from-buffer (org-habitica--send-request-with-user-key org-habitica-request-method-get "/tags" nil))))

(defun org-habitica--get-tag-by-name (name)
  "Get one tag by its name."
  (let ((tags (org-habitica--get-all-tags-from-habitica))
	id)
    (dolist (tag tags id)
      (when (equal (assoc-default 'name tag) name)
	(setq id (assoc-default 'id tag))))))

(defun org-habitica--add-tag-to-task (task-id tag-id)
  "Add a tag to a task in Habitica."
  (let ((api-url-end (concat "/tasks/" task-id "/tags/" tag-id))
	(request-data 
	 (list 
	  (cons "taskId" task-id) 
	  (cons "tagId" tag-id))))
    (org-habitica--send-request-with-user-key org-habitica-request-method-post api-url-end request-data)))

(defun org-habitica--create-tag (name)
  "Create tag by NAME and return tag id."
  (org-habitica--get-from-data 'id (org-habitica--format-response-from-buffer (org-habitica--send-request-with-user-key org-habitica-request-method-post "/tags" (list (cons "name" name))))))

(defun org-habitica--add-tag-by-name-to-task (task-id tag-name)
  "Add a tag by its name to a task."
  (let ((tag-id (org-habitica--get-tag-by-name tag-name)))
    (if tag-id
	(org-habitica--add-tag-to-task task-id tag-id)
      (org-habitica--add-tag-to-task task-id (org-habitica--create-tag tag-name)))))

(defun org-habitica--format-response-from-buffer-if-success (buffer)
  "Return response from BUFFER or nil if success = :json-false."
  (let ((response (org-habitica--format-response-from-buffer buffer)))
    (when (org-habitica--was-response-successful-p response)
      response)))

(defun org-habitica--score-task-by-state (id habitica-completed-state org-state &optional type)
  "Score task by TYPE up or down found by ID syncing HABITICA-COMPLETED-STATE, ORG-STATE."
  (if (org-habitica--entry-is-neither-todo-nor-done-p org-state)
      (org-habitica--delete-task id)
      ;; (org-habitica--add-tag-by-name-to-task id org-state)
    (if (org-entry-is-done-p)
	(when (or (equal habitica-completed-state json-false)
		  (not habitica-completed-state))
	  (org-habitica--score-task id org-habitica--score-direction-up))
      (when (equal habitica-completed-state t)
	(org-habitica--score-task id org-habitica--score-direction-down)))))

(defun org-habitica--get-priority-value-by-name (priority-name)
  "Get priority value by PRIORITY-NAME."
  (cdr (assoc priority-name org-habitica-priority-values)))

(defun org-habitica--update-task (task-id org-state data &optional found-task)
  "Update task with TASK-ID and ORG-STATE by sending DATA."
  (when data
    (let ((habitica-completed-state (org-habitica--get-from-data 'completed found-task)))
      (org-habitica--format-response-from-buffer (org-habitica--send-request-with-user-key org-habitica-request-method-put (concat "/tasks/" task-id) data))
      (org-habitica--score-task-by-state task-id habitica-completed-state org-state))))

(defun org-habitica--get-priority-from-task (task)
  "Get priority from TASK."
  (org-habitica--get-from-data 'priority task))

(defun org-habitica--get-task-from-habitica-by-id-and-format-response (id)
  "Get task from Habitica from ID."
  (org-habitica--format-response-from-buffer-if-success (org-habitica--get-task-from-habitica id)))

(defun org-habitica--get-changed-data (found-task org-priority-value org-attribute)
  "Compare FOUND-TASK with ORG-PRIORITY-VALUE and ORG-ATTRIBUTE and get changed data for the update request."
  (let* ((habitica-priority-value (org-habitica--get-from-data 'priority found-task))
	 (habitica-attribute (org-habitica--get-from-data 'attribute found-task))
	 (priorities-equal-p (equal org-priority-value habitica-priority-value))
	 (attributes-equal-p (equal org-attribute habitica-attribute))
	 (update-data))
    (unless priorities-equal-p
      (setq update-data (list (cons "priority" org-priority-value))))
    (unless attributes-equal-p
      (setq update-data (cons (cons "attribute" org-attribute) update-data)))
  ;; (push (cons "attribute" org-attribute) update-data)))
  update-data))

(defun org-habitica--sync-task-with-id (id org-state &optional caption notes priority-value attribute type habit-buttons)
  "Synchronize a task with ID, CAPTION with Habitica setting its ORG-STATE, NOTES and PRIORITY."
  (let ((found-task (org-habitica--get-task-from-habitica-by-id-and-format-response id)))
    (if found-task
	(org-habitica--update-task id org-state (org-habitica--get-changed-data found-task priority-value attribute) found-task)
      (org-habitica--create-set-id-and-score-task caption org-state notes priority-value attribute type))))

(defun org-habitica--sync-found-recurring-task (habitica-task id org-state caption source org-priority-value org-attribute org-task-type habit-buttons)
  "Synchronize with Habitica a recurring org task under point."
  (let ((habitica-task-type (org-habitica--get-from-data 'type habitica-task))
	(habitica-completed-state (org-habitica--get-from-data 'completed habitica-task))
	(org-habitica-type-todo (car (cddr org-habitica-types)))
	(org-habitica-type-habit (car org-habitica-types)))
    (cond
	  ((equal habitica-task-type org-habitica-type-todo)
	   (when (equal habitica-completed-state json-false)
	     (org-habitica--score-task-by-state id habitica-completed-state org-state))
	   (org-habitica--set-id (org-habitica--create-task-get-id caption source org-priority-value org-attribute org-habitica-type-habit (list (car org-habitica-habit-buttons)))))
	  ((equal habitica-task-type org-habitica-type-habit)
	   (org-habitica--score-task-by-state id habitica-completed-state org-state)))))

(defun org-habitica--sync-recurring-task (id org-state caption source org-priority-value org-attribute org-task-type habit-buttons)
  "Synchronize with Habitica a recurring org task under point."
  (let ((habitica-task (org-habitica--get-task-from-habitica-by-id-and-format-response id))
	(org-habitica-type-habit (car org-habitica-types)))
    (if habitica-task
	(org-habitica--sync-found-recurring-task habitica-task id org-state caption source org-priority-value org-attribute org-task-type habit-buttons)
      (let* ((new-id (org-habitica--create-task-get-id caption source org-priority-value org-attribute org-habitica-type-habit (list (car org-habitica-habit-buttons)))))
	(org-habitica--set-id new-id)
	(org-habitica--score-task-by-state new-id nil org-state)))))

(defun org-habitica-sync-task ()
  "Synchronize an org task under point with Habitica."
  (interactive)
  (save-excursion
    (setq org-habitica--buffer (current-buffer))
    (let* ((caption (nth 4 (org-heading-components)))
	   (org-state (nth 2 (org-heading-components)))
	   (id (org-habitica--get-id-from-org (current-buffer)))
	   (source (buffer-name (current-buffer)))
	   (tags (org-get-tags))
	   (org-task-type (car (seq-intersection org-habitica-types tags)) )
	   (habit-buttons (seq-intersection org-habitica-habit-buttons tags))
	   (org-priority-name (car (seq-intersection org-habitica-priorities tags)))
	   (org-priority-value (cdr (assoc org-priority-name org-habitica-priority-values)))
	   (org-attribute (car (seq-intersection org-habitica-attributes tags))))
      (if (org-get-repeat)
	  (org-habitica--sync-recurring-task id org-state caption source org-priority-value org-attribute org-task-type habit-buttons)
	(if id
	    (org-habitica--sync-task-with-id id org-state caption source org-priority-value org-attribute org-task-type habit-buttons)
	  (if caption
	      (org-habitica--create-set-id-and-score-task caption org-state source org-priority-value org-attribute org-task-type habit-buttons)
	    ;; no caption
	    (message "No caption. No task.")))))))

(defun org-habitica-delete-all-todos ()
  "Delete all todos."
  (let* ((all-tasks (org-habitica-get-all-tasks-from-habitica)))
    (dolist (task all-tasks)
      (sleep-for 3)
      (when (equal (cdr (assoc 'type task)) "todo")
	(org-habitica--delete-task (cdr (assoc 'id task)))))))

(defun org-habitica--get-state ()
  "Get Org task state."
  (nth 2 (org-heading-components)))

(defun org-habitica--task-p ()
  "Check if the item under point is a task."
  (if (org-habitica--get-state)
      t
    nil))

(provide 'org-habitica)


;;; org-habitica.el ends here
