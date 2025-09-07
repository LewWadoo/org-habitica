;;; org-habitica.el --- Habitica integration for org-mode

;; Author: Evgeny Mikhaylov <lewwadoo@gmail.com>
;; Created: [2016-07-15]
;; Version: 20250824
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

;; Copy lines between "begin feature 'org-habitica" and "end feature 'org-habitica" to your init file:

					; begin feature 'org-habitica
;; (add-to-list 'load-path "<PATH-TO-YOUR-ORG-HABITICA-FILE>/org-habitica/")

(defun org-habitica-unload-feature ()
  "Unload org-habitica."
  (interactive)
  (global-unset-key (kbd "C-c u"))
  (global-unset-key (kbd "C-c C-x h"))
  (unload-feature 'org-habitica)
  (message "feature org-habitica unloaded"))

(defun org-habitica-load-feature ()
  "Load org-habitica."
  (interactive)
  (require 'org-habitica)
  (global-set-key (kbd "C-c u") 'org-habitica-unload-feature)
  (global-set-key (kbd "C-c C-x h") 'org-habitica-sync-task)
  (add-hook 'org-after-todo-state-change-hook 'org-habitica-sync-task) ;; org-after-todo-state-change-hook is from org.el
  (org-habitica-check-access))

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
(require 'url-util) ;; for `url-hexify-string'

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

(defvar org-habitica-pet-food-map 
  '(
        ("Base"  . "Meat")
        ("White" . "Milk")
        ("Desert" . "Potatoe")
        ("Red" . "Strawberry")
	("Shade" . "Chocolate")
        ("Skeleton" . "Fish")
        ("Zombie" . "RottenMeat")
        ("CottonCandyPink" . "CottonCandyPink")
        ("CottonCandyBlue" . "CottonCandyBlue")
        ("Golden" . "Honey")
	("RoyalPurple" . "Honey")) ;; any food is favourite for RoyalPurple
  "Alist mapping pet-type string -> food.")

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

(defvar org-habitica-sell-keep-defaults
  '((eggs . 0) (hatchingPotions . 0) (food . 0))
  "Alist mapping item type symbol to number to keep per key when selling extras.
Example: '((food . 10) (eggs . 0)) will keep 10 of each food, sell the rest of each food key.")

;;; Member
(defun org-habitica-get-member-profile (member)
  "Get MEMBER profile."
  (org-habitica--get-data-from-response (format-response-from-buffer (org-habitica--send-request-with-user-key org-habitica-request-method-get (concat "/members/" member) nil))))

;;; User
(defun org-habitica-feed-pet (pet food &optional amount)
  "Feed PET with FOOD.  Assumes FOOD is valid (not \"any\").

Optional AMOUNT is the number of food items to feed.  If non-nil, the
request will include the query parameter \"?amount=AMOUNT\".
AMOUNT is coerced to an integer and must be positive.

Examples:
- /user/feed/Armadillo-Shade/Chocolate
- /user/feed/Armadillo-Shade/Chocolate?amount=9"
  (let* ((amt (when amount (max 0 (round (string-to-number (format "%s" amount))))))
         (pet-enc (url-hexify-string (format "%s" pet)))
         (food-enc (url-hexify-string (format "%s" food))))
    (when (and amount (<= amt 0))
      (user-error "AMOUNT must be a positive integer"))
    (let ((path (concat "/user/feed/" pet-enc "/" food-enc
                        (when amount (concat "?amount=" (number-to-string amt))))))
      (org-habitica--get-from-response
       'data
       (org-habitica--format-response-from-buffer
        (org-habitica--send-request-with-user-key
         org-habitica-request-method-post
         path
         nil
         (cons "Content-Length" "0")))))))

(defun org-habitica--sell-item-request (type key &optional amount)
  "Send a sell request for TYPE and KEY.
If AMOUNT is non-nil and >1, include ?amount=AMOUNT."
  (let* ((type-str  (format "%s" type))
         (key-str   (format "%s" key))
         (path-base (concat "/user/sell/" (url-hexify-string type-str) "/" (url-hexify-string key-str)))
         (path      (if (and amount (> amount 0) (> amount 1))
                        (concat path-base "?amount=" (number-to-string amount))
                      path-base)))
    (condition-case err
        (org-habitica--get-from-response
         'data
         (org-habitica--format-response-from-buffer
          (org-habitica--send-request-with-user-key
           org-habitica-request-method-post
           path
           nil
           (cons "Content-Length" "0"))))
      (error
       (message "Error selling %s %s: %s" type-str key-str err)
       nil))))

(defun org-habitica-sell-extra-items (&optional types keep-alist dry-run)
  "Sell extra Habitica items for TYPES according to KEEP-ALIST.
TYPES is a list of symbols (default: (eggs hatchingPotions food)).
KEEP-ALIST is an alist mapping type symbol to keep count per key; defaults to `org-habitica-sell-keep-defaults'.
If DRY-RUN is non-nil, the function only logs planned sales and does not call the API.

Returns a list of successful sale records of the form: (((TYPE . KEY) . AMOUNT) ...)."
  (let* ((types (or types '(eggs hatchingPotions food)))
         (keep-alist (or keep-alist org-habitica-sell-keep-defaults))
         (results '()))
    ;; retrieve snapshot for each type once and mutate the local snapshot as we sell
    (dolist (type types)
      (let* ((items (org-habitica-get-specific-items type)) ; alist (Name . qty)
             (keep-default (or (cdr (assoc type keep-alist)) 0)))
        (dolist (item items)
          (let* ((key (format "%s" (car item)))
                 (qty (cdr item))
                 (keep keep-default)
                 (to-sell (max 0 (- qty keep))))
	    (when (and (> to-sell 0)
		       (not (string= key "Saddle")))
              (if dry-run
                  (progn
                    (message "[dry-run] Would sell %d of %s (%s)" to-sell key type)
                    (push (cons (cons type key) to-sell) results)
                    ;; update local snapshot in dry-run to reflect subsequent steps
		    (setq items (org-habitica--specific-item-decrement items key to-sell)))
                ;; perform actual sell
                (message "Selling %d of %s (%s) ..." to-sell key type)
                ;; call API: when to-sell == 1 call without amount arg, else include amount
                (let ((resp (if (= to-sell 1)
                                (org-habitica--sell-item-request type key)
                              (org-habitica--sell-item-request type key to-sell))))
		  (sleep-for 3)
                  (if resp
                      (progn
                        (message "Sold %d of %s (%s) — server responded" to-sell key type)
                        (push (cons (cons type key) to-sell) results)
                        ;; decrement local snapshot so further items in this run see updated counts
                        (setq items (org-habitica--specific-item-decrement items key to-sell)))
                    (message "Failed to sell %s %s (attempted %d)" type key to-sell)))))))))
    (nreverse results)))

;;;###autoload
(defun org-habitica-check-access ()
  "Check Habitica's API status."
  (message "Connection to Habitica is %s" (org-habitica--get-from-data 'status (org-habitica--format-response-from-buffer-if-success (org-habitica--send-request org-habitica-request-method-get "/status")))))

(defun org-habitica-get-items ()
  "Get user items."
  (org-habitica--get-from-response 'items (org-habitica-get-user-profile)))
  
(defun org-habitica--pet-type-from-name (pet)
  "Return the pet type part from PET (string) after the last dash.
Examples:
- \"LionCub-Shade\"          -> \"Shade\"
- \"BearCub-CottonCandyBlue\" -> \"CottonCandyBlue\"
Return nil if PET is nil or not a string or contains no dash."
  (when (and pet (stringp pet))
    (let ((parts (split-string pet "-")))
      (when (> (length parts) 1)
        (car (last parts))))))

(defun org-habitica-get-specific-items (item-symbol)
  "Get specific type of items."
  (org-habitica--get-from-response item-symbol (org-habitica-get-items)))

(defun org-habitica--specific-item-quantity (alist name)
  "Return quantity of NAME in ALIST, or 0 if not present.
ALIST entries may have keys as symbols or strings; comparison is done
by converting keys to strings via `format'."
  (let ((entry (cl-find name alist
                        :key (lambda (e) (format "%s" (car e)))
                        :test #'string=)))
    (if entry (cdr entry) 0)))

(defun org-habitica--specific-item-decrement (alist name amount)
  "Decrease quantity of NAME in ALIST by AMOUNT and return ALIST.
If NAME is not present, return ALIST unchanged. Ensures count doesn't go below 0.
ALIST entries may have keys as symbols or strings."
  (when (and alist name (> amount 0))
    (let ((entry (cl-find name alist
                          :key (lambda (e) (format "%s" (car e)))
                          :test #'string=)))
      (when entry
        (setcdr entry (max 0 (- (cdr entry) amount))))))
  alist)

(defun org-habitica--specific-item-entry (alist name)
  "Return the cons cell in ALIST whose car printed equals NAME, or nil."
  (cl-find name alist
           :key (lambda (e) (format "%s" (car e)))
           :test #'string=))

(defun org-habitica-hatch-all-possible-pets ()
  "Attempt to hatch pets from all egg × hatching-potion combinations you have.

Uses local snapshots of eggs and hatching potions (from
`org-habitica-get-specific-items') and the list of owned pets to
avoid re-hatching owned combinations. Decrements the local snapshots
only after successful hatches. Returns a list of hatched pet names."
  (let ((eggs    (org-habitica-get-specific-items 'eggs))              ; alist (Name . qty)
        (potions (org-habitica-get-specific-items 'hatchingPotions))   ; alist (Name . qty)
        (pets    (org-habitica-get-specific-items 'pets))              ; alist of owned pets
        (hatched '()))
    (dolist (egg eggs)
      (let ((egg-name (format "%s" (car egg)))
            (egg-qty  (cdr egg)))
        (when (> egg-qty 0)
          (dolist (potion potions)
            (let ((potion-name (format "%s" (car potion)))
                  (potion-qty  (cdr potion)))
              (when (> potion-qty 0)
                (let ((target-pet (format "%s-%s" egg-name potion-name)))
                  (let* ((entry (org-habitica--specific-item-entry pets target-pet))
			 (entry-value (cdr entry)))
                    (cond
                     ;; ((null entry)
                     ;;  (message "Skipping %s: no local pet entry found" target-pet))
                     ((not (or (equal -1 entry-value)
			       (equal nil entry-value)))
                      (message "Skipping %s: not eligible for hatching (value=%s)" target-pet entry-value))
                     (t
                      ;; eligible: attempt hatch
                      (let ((path (concat "/user/hatch/"
                                         (url-hexify-string egg-name) "/"
                                         (url-hexify-string potion-name))))
                      ;; small delay to be kind to API
			(sleep-for 3)
			(org-habitica--get-from-response 'data (org-habitica--format-response-from-buffer (org-habitica--send-request-with-user-key org-habitica-request-method-post path nil (cons "Content-Length" "0"))))
                      ;; decrement local snapshots so subsequent loops use updated counts
                      (setq eggs    (org-habitica--specific-item-decrement eggs egg-name 1))
                      (setq potions (org-habitica--specific-item-decrement potions potion-name 1))
                      ;; add to local pets to avoid re-hatching same combination
		      (push (cons target-pet t) pets)
                      (push target-pet hatched))))))))))))
    ;; return list of hatched pet names (possibly empty)
    (nreverse hatched)))

(defun org-habitica-get-mounts ()
  "Get user mounts."
  (org-habitica--get-from-response 'mounts (org-habitica-get-items)))

(defun org-habitica-get-pets ()
  "Get user pets."
  (org-habitica--get-from-response 'pets (org-habitica-get-items)))

(defun org-habitica-get-food-for-pet (pet)
  "Get appropriate meal for PET.

Extract the pet type (the part after the last dash) and look it up in
`org-habitica-pet-food-map'. If no mapping exists (or PET does not
contain a type), return the string \"any\"."
  (let* ((type (org-habitica--pet-type-from-name pet)))
    (let ((food (when type
                  (alist-get type org-habitica-pet-food-map nil nil #'string=))))
      (or food "any"))))

(require 'cl-lib)

(defun org-habitica--alist-has-key-name-p (alist name)
  "Return non-nil if ALIST contains a key whose printed name equals NAME.
This handles keys that may be symbols or strings by comparing
\"(format \"%s\" (car entry))\" against NAME."
  (and alist name
       (cl-some (lambda (entry)
                  (string= name (format "%s" (car entry))))
                alist)))

(defconst org-habitica--preferred-food-units-per-item 5
  "Units of pet progress gained from one preferred food item.")

(defconst org-habitica--pet-max-units 50
  "Maximum pet units at which the pet becomes a mount and is removed from pets.")

(defun org-habitica-required-food-amount-from-value (pet-value &optional units-per-item max-units)
  "Return number of preferred-food items required to raise PET-VALUE to MAX-UNITS.
PET-VALUE may be a number or a string.  UNITS-PER-ITEM and MAX-UNITS
default to org-habitica--preferred-food-units-per-item' and org-habitica--pet-max-units' respectively.

Returns an integer (>= 0).  In normal operation this will be >= 1,
because pets present in the list should have value < MAX-UNITS."
  (let* ((units (or units-per-item org-habitica--preferred-food-units-per-item))
         (maxu (or max-units org-habitica--pet-max-units))
         (pv (if (numberp pet-value)
                 pet-value
               (string-to-number (format "%s" pet-value))))
         (needed (max 0 (- maxu pv))))
    (if (<= needed 0)
        0
      (ceiling (/ (float needed) units)))))

(defun org-habitica-feed-all-pets ()
  "Feed all pets.

Skip a pet if the same mount is already present in mounts.

For pets that are fed, compute how many preferred food items are
needed using `org-habitica-required-food-amount-from-value' and send
that amount to the API. If we don't have enough favourite food,
feed the maximum available amount. The local food alist is decremented
after each feed so later iterations use up-to-date counts. When the
amount is 1 the API call is made without the amount query parameter."
  (let ((pets (org-habitica-get-specific-items 'pets))
        (mounts (org-habitica-get-specific-items 'mounts))
        (food  (org-habitica-get-specific-items 'food))) ; local snapshot of food counts
    (dolist (pet pets)
      (let* ((pet-name (format "%s" (car pet)))
             (pet-value (cdr pet)))
        (cond
         ((< pet-value 0)
          (message "Skipping %s: pet is not yet hatched" pet-name))
         ((org-habitica--alist-has-key-name-p mounts pet-name)
          (message "Skipping %s: mount already owned" pet-name))
         (t
          (let ((favourite-food (org-habitica-get-food-for-pet pet-name)))
            (if (string= favourite-food "any")
                (message "Skipping %s: favourite food for type %s is not mapped"
                         pet-name (org-habitica--pet-type-from-name pet-name))
              (let* ((needed (org-habitica-required-food-amount-from-value pet-value))
                     (available (org-habitica--specific-item-quantity food favourite-food))
                     (to-feed (cond
                               ((<= available 0) 0)
                               ((<= needed available) needed)
                               (t available)))) ; cap to what we have
                (cond
                 ((= to-feed 0)
                  (message "Skipping %s: no %s available to feed" pet-name favourite-food))
                 ((= to-feed 1)
                  (message "Feeding %s with %s (amount=1) — using single-item API call" pet-name favourite-food)
                  (org-habitica-feed-pet pet-name favourite-food)
                  ;; decrement local food snapshot
                  (setq food (org-habitica--specific-item-decrement food favourite-food 1)))
                 (t
		  (message "Feeding %s with %s (amount=%d) — available=%d, needed=%d"
                           pet-name favourite-food to-feed available needed)
                  (org-habitica-feed-pet pet-name favourite-food to-feed)
                  ;; decrement local food snapshot
                  (setq food (org-habitica--specific-item-decrement food favourite-food to-feed)))))
                (sleep-for 3)))))))))


(defun org-habitica-get-user-profile ()
  "Get user profile."
  (org-habitica-get-member-profile org-habitica-api-user))

(defun org-habitica--entry-is-neither-todo-nor-done-p (state)
  "Check whether the task was discarded according to its STATE."
  (member state org-habitica--neither-todo-nor-done-keywords))

(defun org-habitica--get-from-response (symbol response)
  "Extract SYMBOL from returned request RESPONSE."
  (cdr (assoc symbol response)))

(defun org-habitica--get-data-from-response (response)
  "Extract DATA from returned request RESPONSE."
  (org-habitica--get-from-response 'data response))

(defun org-habitica--get-from-data (symbol response)
  "Extract SYMBOL from returned request RESPONSE."
  (cdr (assoc symbol (org-habitica--get-data-from-response response))))

(defun org-habitica--get-id-from-org ()
  "Get id property of an org task at point."
  (setq point-get (point))
  (org-entry-get point-get org-habitica--id-property-name))

(defun org-habitica--set-id (id)
  "Set ID as a property of an org task."
  (org-entry-put point-get org-habitica--id-property-name id))

(defun org-habitica--was-response-successful-p (response)
  "Return t if the RESPONSE was successful."
  (not (equal
	(assoc-default 'success response)
	json-false)))

(defun org-habitica--get-error-from-response (response)
  "Return error from RESPONSE."
  (unless (org-habitica--was-response-successful-p response)
    (assoc-default 'error response)))

(defun org-habitica--format-response-from-buffer (buffer)
  "Format BUFFER data from json to a list."
  ;; (let ((response-buffer buffer))
  (when (not (bufferp buffer))
    ;; (setq response-buffer (current-buffer)))
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (goto-char url-http-end-of-headers)
    ;; (message "buffer: %s" (buffer-string)) ;; (point) (point-max))
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
      (let ((new-id (org-habitica--create-task-get-id caption source org-priority-value org-attribute org-habitica-type-habit (list (car org-habitica-habit-buttons)))))
	(org-habitica--set-id new-id)
	(org-habitica--score-task-by-state new-id nil org-state)))))

(defun org-habitica-sync-task ()
  "Synchronize an org task under point with Habitica."
  (interactive)
  (save-excursion
    (let* ((caption (nth 4 (org-heading-components)))
	   (org-state (nth 2 (org-heading-components)))
	   (id (org-habitica--get-id-from-org))
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
