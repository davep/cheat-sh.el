;;; cheat-sh.el --- Interact with cheat.sh  -*- lexical-binding: t -*-
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.0
;; Keywords: docs
;; URL: https://github.com/davep/cheat-sh.el
;; Package-Requires: ((emacs "24"))

;; cheat-sh.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2 or (at your option) any later version.
;; For details see the file COPYING.

;;; Commentary:
;; 
;; cheat-sh.el provides a simple Emacs interface for looking things up on
;; cheat.sh.

;;; Code:

(defconst cheat-sh-url "http://cheat.sh/%s?T"
  "URL for cheat.sh.")

(defun cheat-sh-get (thing)
  "Get THING from cheat.sh."
  (with-current-buffer
      ;; Here I "fake" the user agent. I do this because I want plain text
      ;; but cheat.sh uses the User-Agent to decide if it should send it or
      ;; HTML. See https://goo.gl/8gh95X for what I mean. (I'd have thought
      ;; it would make more sense to look for a requested content type, or
      ;; perhaps both?)
      (let ((url-request-extra-headers '(("User-Agent" . "curl (cheat-sh.el)"))))
        (url-retrieve-synchronously (format cheat-sh-url (url-hexify-string thing)) t t))
    (setf (point) (point-min))
    (when (search-forward-regexp "^$" nil t)
      (buffer-substring (point) (point-max)))))

;;;###autoload
(defun cheat-sh (thing)
  "Look up THING on cheat.sh and display the result."
  (interactive "sLookup: ")
  (let ((result (cheat-sh-get thing)))
    (if result
        (with-help-window "*cheat.sh*"
          (princ result))
      (error "Can't find anything for %s on cheat.sh" thing))))

(provide 'cheat-sh)

;;; cheat-sh.el ends here
