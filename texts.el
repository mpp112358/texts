;;; texts.el --- Extend LaTeX -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Manuel Pérez
;;
;; Author: Manuel Pérez <mperez@fomento.edu>
;; Maintainer: Manuel Pérez <mperez@fomento.edu>
;; Created: January 06, 2026
;; Modified: January 06, 2026
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/mpp112358/texts
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  texts-numitems: Numerate items inside an enumerate environment in LaTeX
;;
;;; Code:

(defun texts-create-state (status depth current-item insertions)
  "Create a state with given STATUS DEPTH and CURRENT-ITEM and INSERTIONS."
  `((status ,status) (depth . ,depth) (current-item ,current-item) (insertions ,insertions)))

(defun texts-get-status (state)
  "Get the status of the given STATE."
  (alist-get 'status state))

(defun texts-get-depth (state)
  "Get the depth of the given STATE."
  (alist-get 'depth state))

(defun texts-get-current-item (state)
  "Get the current item number of the given STATE."
  (alist-get 'current-item state))

(defun texts-get-insertions (state)
  "Get the insertions list from given STATE."
  (alist-get 'insertions state))

(defun texts-fsm (transitions)
  "Return a texts finite state machine for the given TRANSITIONS."
  (lambda (events initial-state)
    (seq-reduce (lambda (state event)
                  (funcall (alist-get event (alist-get (texts-get-status state) transitions)) state))
                events
                initial-state)))

(defun texts-numitems ()
  "Define an alist of states and transitions between them."
  (texts-fsm
   `((null-state .
      ((begin . ,(lambda (state) (texts-create-state 'numerating-state 1 0 (texts-get-insertions state))))
       (end . ,(lambda (state) state))
       (item . ,(lambda (state) state))))
     (numerating-state .
                       ((begin . ,(lambda (state)
                                    (texts-create-state 'nested-state 2 (texts-get-current-item state) (texts-get-insertions state))))
                        (end . ,(lambda (state)
                                  (texts-create-state 'null-state 0 0 (texts-get-insertions state))))
                        (item . ,(lambda (state)
                                   (texts-create-state 'numerating-state 1 (1+ (texts-get-current-item state)) (cons (1+ (texts-get-current-item state)) (texts-get-insertions state)))))))
     (nested-state .
                   ((begin . ,(lambda (state)
                                (numitem-create-state 'nested-state (1+ (texts-get-depth state)) (texts-get-current-item state) (texts-get-insertions state))))
                    (end . ,(lambda (state)
                              (let ((depth (numitem-get-depth state)))
                                (if (eq depth 2)
                                    (texts-create-state 'numerating-state 1 (texts-get-current-item state) (texts-get-insertions state))
                                  (texts-create-state 'nested-state (1- (texts-get-depth state)) (texts-get-current-item state) (texts-get-insertions state))))))
                    (item . ,(lambda (state)
                               (texts-create-state 'nested-state (texts-get-depth state) (texts-get-current-item state) (texts-get-insertions state)))))))))

(defun texts-test-numitems (events)
  "Test texts-numitems using EVENTS sequence of events."
  (interactive "sEvents: ")
  (message "%s" (funcall (texts-numitems) events (texts-create-state 'null-state 0 0 nil))))

(provide 'texts)
;;; texts.el ends here
