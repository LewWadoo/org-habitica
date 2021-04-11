 This package provides an integration between org-mode and Habitica. When a state of a TODO task in org-mode is changed, the task gets created, its "completed" state is updated or it gets deleted from Habitica. The action depends on the final state of a task.

# Installation
 Copy this file (org-habitica.el) to your machine

 Copy lines between "begin feature 'org-habitica" and "end feature 'org-habitica" to your .emacs file:

					; begin feature 'org-habitica
 (add-to-list 'load-path "<PATH-TO-YOUR-ORG-HABITICA-FILE>/org-habitica/")

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
   (add-hook 'org-after-todo-state-change-hook 'org-habitica-sync-task)  org-after-todo-state-change-hook is from org.el
   (setq org-habitica-api-user "<PUT-YOUR-ID-HERE>")
   (setq org-habitica-api-token "<PUT-YOUR-TOKEN-HERE>")
   (org-habitica-check-access))

 (global-set-key (kbd "C-c h") 'org-habitica-load-feature)

					; end feature 'org-habitica

 Then get your User ID and API Token which can be found on thepage with API settings when you are logged in to your Habitica account: https://habitica.com/#/options/settings/api and replace:
 <PUT-YOUR-ID-HERE> with your User ID;
 <PUT-YOUR-TOKEN-HERE> with your API Token.

 Finally to start using the feature reload your Emacs and type:
 C-c h
 To unload the feature type:
 C-c u
