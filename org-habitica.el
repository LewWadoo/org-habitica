;;; org-habitica.el --- Habitica integration for org-mode

;; Author: Evgeny Mikhaylov <lewwadoo@gmail.com>
;; Created: [2016-07-15]
;; Version: 20160807
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

;; This package provides an integration between org-mode and Habitica. When a state of a TODO task in org-mode is changed, the task gets created, its "completed" state is updated or it gets deleted from Habitica. The action depends on the final state of a task.

;;; Installation
;; Copy this file (org-habitica.el) to your machine

;; Copy lines between "begin feature 'org-habitica" and "end feature 'org-habitica" to your .emacs file:

; begin feature 'org-habitica
;; (add-to-list 'load-path "<PATH-TO-YOUR-ORG-HABITICA-FILE>/org-habitica/")

;; (defun org-habitica--unload-feature ()
;;   "Unload org-habitica."
;;   (unload-feature 'org-habitica))

;; (defun org-habitica--load-feature ()
;;   "Load org-habitica."
;;   (require 'org-habitica)
;;   (add-hook 'org-after-todo-state-change-hook 'org-habitica--sync-task))

;; (setq org-habitica-api-user "<PUT-YOUR-ID-HERE>")
;; (setq org-habitica-api-token "<PUT-YOUR-TOKEN-HERE>")

; end feature 'org-habitica

;; Then get your User ID and API Token which can be found on thepage with API settings when you are logged in to your Habitica account: https://habitica.com/#/options/settings/api and replace:
;; <PUT-YOUR-ID-HERE> with your User ID;
;; <PUT-YOUR-TOKEN-HERE> with your API Token.

;; Uncomment just inserted lines in your .emacs that start with ;;

;; Finally to start using the feature reload your Emacs and type:
;; M-: (org-habitica-load-feature)
;; To unload the feature type:
;; M-: (org-habitica-unload-feature)

;;; Code:

;; Requires
(require 'json)
(require 'org)

;; Possible keys to consider:
;; C-c C-x u ­ add or update a task
;; C-c C-x d ­ delete a task
;; C-c C-x o ­ enable org-habitica
;; C-u C-c C-x o ­ disable org-habitica

(defcustom org-habitica--neither-todo-nor-done-keywords '("WAIT" "MAYBE" "HOLD" "CANCELED" "FAILED" "DELEGATED" nil)
  "Org states which delete a task in Habitica when switching to.")

(defvar org-habitica--response nil
  "Contain the data returned by a request.")

(defvar org-habitica--buffer nil
  "Current org buffer.")

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
  (let ((url-request-method "GET")
	(url (concat org-habitica--api-url "/status"))
	(url-request-extra-headers
	 `(
	   ("Content-Type" . "application/json"))))
    (url-retrieve url 'org-habitica--format-response))
  ;; TODO: return an appropriate string of login status
  (if (org-habitica--was-response-successful-p org-habitica--response)
      (message "Connection to Habitica is up")))
    ;; (message "Cannot connect to Habitica")))


(defun org-habitica--entry-is-neither-todo-nor-done-p (state)
  "Check whether the task was discarded according to its STATE."
  (member state org-habitica--neither-todo-nor-done-keywords))

;; (defun get-list-of-org-todo-keywords ()
;;   "Get alist of org-todo-keywords as keys with values."
  
(defun org-habitica--get-from-data (symbol data)
  "Extract SYMBOL from returned request response DATA."
  (cdr (assoc symbol (cdr (assoc 'data data)))))

(defun org-habitica--get-id-from-org ()
  "Get id property of an org task."
  (save-current-buffer
    (set-buffer org-habitica--buffer)
    ;; (setq org-habitica--current-task-id (org-entry-get (point) org-habitica--id-property-name))
    (org-entry-get (point) org-habitica--id-property-name)))

(defun org-habitica--set-id (id)
  "Set ID as a property of org task."
  (save-current-buffer
    (set-buffer org-habitica--buffer)
    ;; (message "property %s is set to %s" org-habitica--id-property-name id)
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

(defun org-habitica--format-response (buffer)
  "Format BUFFER data from json to a list."
  (let ((response-buffer buffer))
    (when (not (bufferp buffer))
      (setq response-buffer (current-buffer)))
    (with-current-buffer response-buffer
      (goto-char url-http-end-of-headers)
      (let ((json-object-type 'alist)
	    (json-key-type 'symbol)
	    (json-array-type 'list))
	(setq org-habitica--response (json-read))))
    (kill-buffer response-buffer)
    org-habitica--response))

(defun org-habitica--create-set-id-and-score-task (caption state source)
  "Create, set returned id for org task and score up if the task is done already."
  ;; (if (org-habitica--entry-is-neither-todo-nor-done-p state)
      ;; (message "The state is %s. So the task should not be created." state)
    (progn
      (org-habitica--format-response (org-habitica--create-task caption state source))
      (org-habitica--set-id (org-habitica--get-from-data 'id org-habitica--response))
      (when (org-entry-is-done-p)
	(let ((id (org-habitica--get-from-data 'id org-habitica--response)))
	  (org-habitica--score-task id org-habitica--score-direction-up)))))

(defun org-habitica-search-habitica-todos-in-org ()
  "Return how many todos are synced in org."
  (setq all-habitica-todos (org-habitica-get-all-todos-from-habitica)))

(defun org-habitica-get-all-todos-from-habitica ()
  "Get all uncompleted todos from Habitica.
Return array of tasks."
  (let (
	(url-request-method "GET")
	(url (concat org-habitica--api-url "/tasks/user"))
	(url-request-extra-headers
	 `(
	   ("type" . "todos")
	   ("Content-Type" . "application/json")
	   ("X-API-User" . ,org-habitica-api-user)
	   ("X-API-Key" . ,org-habitica-api-token))))
    (url-retrieve url 'org-habitica--format-response)))

(defun org-habitica--get-task-from-habitica (id)
  "Get task from Habitica by its ID.
Return a buffer with json response containing task's data."
  (let (
	(url-request-method "GET")
	(url (concat org-habitica--api-url "/tasks/" id))
	(url-request-extra-headers
	 `(
	   ("Content-Type" . "application/json")
	   ("X-API-User" . ,org-habitica-api-user)
	   ("X-API-Key" . ,org-habitica-api-token))))
    (url-retrieve-synchronously url)))

(defun org-habitica--score-task (id direction)
  "Score a task with id ID up or down."
  (let (
	(url-request-method "POST")
	(url (concat org-habitica--api-url "/tasks/" id "/score/" direction))
	(url-request-extra-headers
	 `(
	   ("Content-Type" . "application/json")
	   ("X-API-User" . ,org-habitica-api-user)
	   ("X-API-Key" . ,org-habitica-api-token))))
    (url-retrieve url 'org-habitica--format-response)))

(defun org-habitica--create-task (caption state source)
  "Create task with caption CAPTION in Habitica."
  (let (
	(url-request-method "POST")
	(url (concat org-habitica--api-url "/tasks/user"))
	(url-request-extra-headers
	 `(
	   ("Content-Type" . "application/json")
	   ("X-API-User" . ,org-habitica-api-user)
	   ("X-API-Key" . ,org-habitica-api-token)))
	(url-request-data
	 (json-encode `(
			("type" . "todo")
			("text" . ,caption)
			("notes" . ,source)))))
    (url-retrieve-synchronously url)))

(defun org-habitica--delete-task (id)
  "Delete a task by its ID in Habitica."
  (let (
	(url-request-method "DELETE")
	(url (concat org-habitica--api-url "/tasks/" id))
	(url-request-extra-headers
	 `(
	   ("Content-Type" . "application/json")
	   ("taskId" . ,id)
	   ("X-API-User" . ,org-habitica-api-user)
	   ("X-API-Key" . ,org-habitica-api-token))))
    (url-retrieve url 'org-habitica--format-response)))

(defun org-habitica-login (username password)
  "Login to Habitica."
  (let (
	(url-request-method "POST")
	(url (concat org-habitica--api-url "/user/auth/local/login"))
	(url-request-extra-headers
	 `(
	   ("Content-Type" . "application/json")
	   ("username" . ,username)
	   ("password" . ,password))))
    (url-retrieve url 'org-habitica--format-response)))
 


(defun org-habitica-sync-task ()
  "Synchronize an org task under point with Habitica."
  (interactive)
  (save-excursion
    (setq org-habitica--buffer (current-buffer))
    (let (
	  (caption (nth 4 (org-heading-components)))
	  (state (nth 2 (org-heading-components)))
	  (id (org-habitica--get-id-from-org))
	  (source (buffer-name org-habitica--buffer))
	  (tags (org-get-tags)))
      ;; if a task was found in habitica by its ID
      (if id
	  (progn
	    (setq org-habitica--response (org-habitica--format-response (org-habitica--get-task-from-habitica id)))
	    (if
		(and
		 org-habitica--response
		 (or
		  (equal (org-habitica--get-error-from-response org-habitica--response) "NotFound")
		  (equal (cdr org-habitica--response) '((error http 404)))))
		;; A task was not found
		(progn
		  ;; (message "org-habitica--sync-task: task %s with id %s was not found." caption id)
		      (org-habitica--create-set-id-and-score-task caption state source))
	      ;; A task was found
	      (progn
		;; (message "task %s with id %s was found" caption id)
		(let (
		      (habitica-completed-state (org-habitica--get-from-data 'completed org-habitica--response)))
		  (if (org-habitica--entry-is-neither-todo-nor-done-p state)
		      (org-habitica--delete-task id)
		    (if
			(and
			 (org-entry-is-done-p)
			 (equal habitica-completed-state json-false))
			(org-habitica--score-task id org-habitica--score-direction-up)
		      (if
			  (and
			   (org-entry-is-todo-p)
			   (equal habitica-completed-state t))
			  (org-habitica--score-task id org-habitica--score-direction-down))))))))
	;; id is nil
	(if caption
	    (org-habitica--create-set-id-and-score-task caption state source)
	  ;; no caption
	  (message "No caption. No task."))
	(if (org-habitica--entry-is-neither-todo-nor-done-p state)
	    ;; should not be created
	    (message "The task %s should not be created." caption))))))

(defun org-habitica--get-state ()
  "Get Org task state."
  (nth 2 (org-heading-components)))

(defun org-habitica--task-p ()
  "Check if the item under point is a task."
  (if (org-habitica--get-state)
      t
    nil))

(defun org-habitica--goto-next-item ()
  "Go to first child, sibling or parent's sibling."
  (or (org-goto-first-child)
      (org-goto-sibling) ;; if this fails try (org-forward-heading-same-level)
      (outline-up-heading))) ;; TODO: think about implementation

(defun org-habitica-copy-all-from-org ()
  "Copy tasks from org file to Habitica.
The org tasks take priority."
  (interactive)
  (let ((source (current-buffer)))
    (save-excursion
      ;; If the current item is not a task but a heading or a note
      (goto-char (point-min)))))

(defun org-habitica-copy-task-from-org (state)
  "Copy an org task under point with Habitica.
The org task takes priority."
    (let (
	  (caption (nth 4 (org-heading-components)))
	  (id (org-habitica--get-id-from-org))
	  (source (buffer-name org-habitica--buffer)))
      (if id
	  (progn
	    (setq org-habitica--response (org-habitica--format-response (org-habitica--get-task-from-habitica id)))
	    (if
		(and
		 org-habitica--response
		 (or
		  (equal (org-habitica--get-error-from-response org-habitica--response) "NotFound")
		  (equal (cdr org-habitica--response) '((error http 404)))))
		;; A task was not found
		(progn
		  ;; (message "org-habitica--sync-task: task %s with id %s was not found." caption id)
		  (org-habitica--create-set-id-and-score-task caption state source))
	      ;; A task was found
	      (progn
		;; (message "task %s with id %s was found" caption id)
		(let (
		      (habitica-completed-state (org-habitica--get-from-data 'completed org-habitica--response)))
		  (if (org-habitica--entry-is-neither-todo-nor-done-p state)
		      (org-habitica--delete-task id)
		    (if
			(and
			 (org-entry-is-done-p)
			 (equal habitica-completed-state json-false))
			(org-habitica--score-task id org-habitica--score-direction-up)
		      (if
			  (and
			   (org-entry-is-todo-p)
			   (equal habitica-completed-state t))
			  (org-habitica--score-task id org-habitica--score-direction-down))))))))
	;; id is nil
	(org-habitica--create-set-id-and-score-task caption state source))))


(defun org-habitica--search-agenda (task)
  "Search org-agenda for a TASK."
  )


;; formerly org-tags-view from org-agenda.el
(defun org-habitica--find-tags (match)
  "Get all headlines for all `org-agenda-files' with MATCH."
  ;; (interactive "P")
  ;; (if org-agenda-overriding-arguments
  ;;     (setq todo-only (car org-agenda-overriding-arguments)
  ;; 	    match (nth 1 org-agenda-overriding-arguments)))
  (let* ((org-tags-match-list-sublevels
	  org-tags-match-list-sublevels)
	 (completion-ignore-case t)
	 ;; (org--matcher-tags-todo-only todo-only)
	 rtn rtnall files file pos matcher
	 buffer)
    (when (and (stringp match) (not (string-match "\\S-" match)))
      (setq match nil))
    (catch 'exit
      ;; (if org-agenda-sticky
      ;; 	  (setq org-agenda-buffer-name
      ;; 		(if (stringp match)
      ;; 		    (format "*Org Agenda(%s:%s)*"
      ;; 			    (or org-keys (or (and todo-only "M") "m")) match)
      ;; 		  (format "*Org Agenda(%s)*" (or (and todo-only "M") "m")))))
      ;; Prepare agendas (and `org-tag-alist-for-agenda') before
      ;; expanding tags within `org-make-tags-matcher'
      ;; (org-agenda-prepare (concat "TAGS " match))
      (setq matcher (org-make-tags-matcher match)
	    match (car matcher)
	    matcher (cdr matcher))
      (org-compile-prefix-format 'tags)
      (org-set-sorting-strategy 'tags)
      (setq org-agenda-query-string match)
      (setq org-agenda-redo-command
	    (list 'org-tags-view
		  ;; `(quote ,org--matcher-tags-todo-only)
		  `(if current-prefix-arg nil ,org-agenda-query-string)))
      (setq files (org-agenda-files nil 'ifmode)
	    rtnall nil)
      (while (setq file (pop files))
	(catch 'nextfile
	  (org-check-agenda-file file)
	  (setq buffer (if (file-exists-p file)
			   (org-get-agenda-file-buffer file)
			 (error "No such file %s" file)))
	  (if (not buffer)
	      ;; If file does not exist, error message to agenda
	      (setq rtn (list
			 (format "No such org-file %s" file))
		    rtnall (append rtnall rtn))
	    (with-current-buffer buffer
	      (unless (derived-mode-p 'org-mode)
		(error "Agenda file %s is not in `org-mode'" file))
	      (save-excursion
		(save-restriction
		  (if (eq buffer org-agenda-restrict)
		      (narrow-to-region org-agenda-restrict-begin
					org-agenda-restrict-end)
		    (widen))
		  (setq rtn (org-scan-tags 'agenda
					   matcher
					   ;; org--matcher-tags-todo-only))
					   t))
		  (setq rtnall (append rtnall rtn))))))))
;;       (if org-agenda-overriding-header
;; 	  (insert (org-add-props (copy-sequence org-agenda-overriding-header)
;; 		      nil 'face 'org-agenda-structure) "\n")
;; 	(insert "Headlines with TAGS match: ")
;; 	(add-text-properties (point-min) (1- (point))
;; 			     (list 'face 'org-agenda-structure
;; 				   'short-heading
;; 				   (concat "Match: " match)))
;; 	(setq pos (point))
;; 	(insert match "\n")
;; 	(add-text-properties pos (1- (point)) (list 'face 'org-warning))
;; 	(setq pos (point))
;; 	(unless org-agenda-multi
;; 	  (insert (substitute-command-keys
;; 		   "Press `\\[universal-argument] \\[org-agenda-redo]' \
;; to search again with new search string\n")))
;; 	(add-text-properties pos (1- (point))
;; 			     (list 'face 'org-agenda-structure)))
;;       (org-agenda-mark-header-line (point-min))
      (when rtnall
	;; (org-agenda-finalize-entries rtnall 'tags)))))
	rtnall))))
      ;; (goto-char (point-min))
      ;; (or org-agenda-multi (org-agenda-fit-window-to-buffer))
      ;; (add-text-properties
      ;;  (point-min) (point-max)
      ;;  `(org-agenda-type tags
      ;; 			 org-last-args (,org--matcher-tags-todo-only ,match)
      ;; 			 org-redo-cmd ,org-agenda-redo-command
      ;; 			 org-series-cmd ,org-cmd))
      ;; (org-agenda-finalize)
      ;; (setq buffer-read-only t))))


;;; -> Functions from abrochard-habitica I need in my program

(defun habitica--send-request (endpoint type data)
  "Base function to send request to the Habitica API.
ENDPOINT can be found in the habitica doc.
TYPE is the type of HTTP request (GET, POST, DELETE)
DATA is the form to be sent as x-www-form-urlencoded."
  (let ((url  (concat org-habitica--api-url endpoint))
        (url-request-method        type)
        (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded") ("x-api-user" . ,habitica-uid) ("x-api-key" . ,habitica-token)))
        (url-request-data          data))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (delete-region (point-min) (string-match-p "{" (buffer-string)))
      (assoc-default 'data (json-read-from-string (decode-coding-string
                                                   (buffer-string)
                                                   'utf-8))))))

(defun habitica--get-profile ()
  "Get the user's raw profile data."
  (habitica--send-request "/user" "GET" ""))


(defun habitica--get-tasks ()
  "Gets all the user's tasks."
  (habitica--send-request "/tasks/user" "GET" ""))

(defun habitica--parse-tasks (tasks order)
  "Parse the tasks to 'org-mode' format.
TASKS is the list of tasks from the JSON response
ORDER is the ordered list of ids to print the task in."
  (dolist (id (append order nil))
    (dolist (value (append tasks nil))
      ;; Get a task from loop
      (if (equal (assoc-default 'id value) id)
	  ;; Search for a task ID in org-agenda
	  (let* ((found (org-habitica--find-tags (concat org-habitica--id-property-name "=" "" id "")))
		 notfoundall)
	    (if found
		(habitica--insert-task value)))))))

  ;; 	    (setq foundall (append found))
  ;; 	    (setq notfoundall (append notfoundnall id))))))
  ;; foundall)

;;;###autoload
(defun habitica-tasks ()
  "Main function to summon the habitica buffer."
  ;; (interactive)
  ;; (if (or (not habitica-uid) (not habitica-token))
  ;;     (call-interactively 'habitica-login))
  (switch-to-buffer "*habitica*")
  (delete-region (point-min) (point-max))
  (org-mode)
  ;; (habitica-mode)
  ;; (insert "#+TITLE: Habitica Dashboard\n\n")
  ;; (habitica--get-tags)
  (let ((habitica-data (habitica--get-tasks))
        (habitica-profile (habitica--get-profile)))
    ;; (habitica--parse-profile (assoc-default 'stats habitica-profile) nil)
    (let ((tasksOrder (assoc-default 'tasksOrder habitica-profile)))
      ;; (insert "* Habits :habit:\n")
      ;; (habitica--parse-tasks habitica-data (assoc-default 'habits tasksOrder))
      ;; (insert "* Daily Tasks :daily:\n")
      ;; (habitica--parse-tasks habitica-data (assoc-default 'dailys tasksOrder))
      (insert "* To-Dos :todo:\n")
      (habitica--parse-tasks habitica-data (assoc-default 'todos tasksOrder))
      ;; (insert "* Rewards :rewards:\n")
      ;; (habitica--parse-rewards habitica-data (assoc-default 'rewards tasksOrder))))
  ;; (org-align-all-tags)
  (org-content))))


;; Loop through all the tasks
(defun habitica-sync-todos ()
  "Sync all todos from Habitica to org."
  ;; 10 Get all tasks from Habitica
  (let ((habitica-data (habitica--get-tasks))
	(habitica-profile (habitica--get-profile)))
    (let ((tasksOrder (assoc-default 'tasksOrder habitica-profile)))
      (habitica--parse-todos habitica-data (assoc-default 'todos tasksOrder))
)))


;; If not found

;; Add new task to weekly_inbox.org





(defun habitica--sync-todo (id)
  "Sync a task from Habitica to org by its ID."
  )


(defun habitica--insert-todo (task)
  "Logic to insert TODO or DONE for a task.

TASK is the parsed JSON response."
  ;; (if (equal (format "%s" (assoc-default 'type task)) "habit")
  ;;     (cond ((>= (assoc-default 'value task) habitica-habit-threshold) (insert "** DONE "))
  ;;           ((< (assoc-default 'value task) habitica-habit-threshold) (insert "** TODO ")))
    (cond         ((eq (assoc-default 'completed task) :json-false) (insert "** TODO "))
                  ((eq (assoc-default 'completed task) t) (insert "** DONE "))))


(defun habitica--insert-task (task)
  "Format the task into org mode todo heading.

TASK is the parsed JSON response."
  (habitica--insert-todo task)
  (insert (assoc-default 'text task))
  ;; (if (< 0 (length (assoc-default 'checklist task)))
  ;;     (habitica--insert-checklist task))
  (insert "\n")
  ;; (habitica--insert-deadline task)
  ;; (habitica--insert-tags task)
  (org-set-property "ID" (assoc-default '_id task))
  (org-set-property "value" (format "%s" (assoc-default 'value task)))
  ;; (if habitica-turn-on-highlighting
  ;;     (catch 'aaa
  ;;       (habitica--highlight-task task))
    ;; )
  )


;;; <- Functions from abrochard emacs-habitica I need in my program



(provide 'org-habitica)


;;; org-habitica.el ends here
