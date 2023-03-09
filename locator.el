;; -*- lexical-binding: t -*-
;;
;; locator - an i18n files lookup tool
;; Copyright (C) 2023  Markus Peröbner
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(defcustom locator-key-display-length 30
  "locator-key-display-length specifies the number of characters
  the translate entry keys should occupy on the screen in the
  entry find.")

;;;###autoload
(defun locator-find-entry (locale)
  (interactive "sLocale: ")
  (helm
   :sources
   (helm-build-sync-source "localized strings"
     :candidates
     (lambda ()
       (locator--collect-lang-entries locale))
     :candidate-transformer
     (lambda (candidates)
       (mapcar
        (lambda (candidate)
          (seq-let (lang-file-path entry-key entry-value key-point) candidate
            (list (locator--format-entry-candidate entry-key entry-value) candidate)))
        candidates))
     :action
     '(
       ("Goto" . locator-goto-entry-key)
       )
     )))

(defun locator--format-entry-candidate (entry-key entry-value)
  (let ((short-entry-key (s-pad-left locator-key-display-length " " (locator--shorten-entry-key entry-key locator-key-display-length))))
    (let ((line (concat short-entry-key ":" entry-value)))
      (put-text-property 0 (+ (length short-entry-key) 1) 'face 'shadow line)
      line
      )))

;; (locator--shorten-entry-key "short" 8)
;; (locator--shorten-entry-key "very long key" 8)
(defun locator--shorten-entry-key (entry-key max-len)
  (let ((entry-key-len (length entry-key)))
    (if (> max-len entry-key-len)
        entry-key
      (let ((right-len (/ max-len 2)))
        (concat
         (substring entry-key 0 (- max-len right-len 1))
         "…"
         (substring entry-key (- entry-key-len right-len) entry-key-len))
        ))))

(defun locator--collect-lang-entries (locale)
  (apply
   'append
   (mapcar
    (lambda (lang-file-path)
      (mapcar
       (lambda (entry)
         (append `(,lang-file-path) entry))
       (locator--read-lang-file lang-file-path)))
    (locator--find-lang-files locale))))

(defun locator--find-lang-files (locale)
  (let ((pr (project-current t)))
    (seq-filter
     (lambda (file-path)
       (let ((file-name (file-name-nondirectory file-path)))
         (and
          (string-suffix-p ".json" file-name)
          (or
           (string-prefix-p (concat locale ".") file-name)
           (string-prefix-p (concat locale "-") file-name)))))
     (project-files pr (project-roots pr)))))

;; (message "lang-file %s" (locator--read-lang-file "test/de.json"))
(defun locator--read-lang-file (file-path)
  (with-current-buffer (find-file-noselect file-path)
    (save-excursion
      (goto-char (point-min))
      (let (
            (state 'detect-value)
            (entries '())
            (key-artifacts '())
            current-key-start-point
            current-key-end-point
            current-string-value)
        (while (not (eobp))
          (cond
           ((eq state 'detect-value)
            (cond
             ((eq (char-after) ? ) t)
             ((eq (char-after) ?{)
              (setq state 'in-obj))
             ((eq (char-after) ?\")
              (setq current-string-value "")
              (setq state 'in-string-value))
             ('t (locator--parse-fail state)))
            )
           ((eq state 'in-obj)
            (cond
             ((eq (char-after) ?\n) t)
             ((eq (char-after) ?\t) t)
             ((eq (char-after) ? ) t)
             ((eq (char-after) ?,) t)
             ((eq (char-after) ?\")
              (setq current-key-start-point (+ (point) 1))
              (setq state 'in-key)
              )
             ((eq (char-after) ?})
              (setq key-artifacts (butlast key-artifacts))
              )
             ('t (locator--parse-fail state)))
            )
           ((eq state 'in-key)
            (cond
             ((eq (char-after) ?\")
              (setq current-key-end-point (point))
              (setq state 'before-assignment))
             ('t 't)
             ))
           ((eq state 'before-assignment)
            (cond
             ((eq (char-after) ?:)
              (setq state 'detect-value)
              (setq key-artifacts (append key-artifacts `(,(buffer-substring-no-properties current-key-start-point current-key-end-point)))))
             ('t (locator--parse-fail state))
             ))
           ((eq state 'in-string-value)
            (cond
             ((eq (char-after) ?\\)
              (setq state 'in-string-value-escape)
              )
             ((eq (char-after) ?\")
              (setq entries (append entries `((,(string-join key-artifacts ".") ,current-string-value ,current-key-start-point))))
              (setq state 'in-obj)
              )
             ('t (setq current-string-value (format-message "%s%c" current-string-value (char-after))))
             ))
           ((eq state 'in-string-value-escape)
            (cond
             ((eq (char-after) ?n)
              (setq current-string-value (concat current-string-value "\n")))
             ((eq (char-after) ?t)
              (setq current-string-value (concat current-string-value "\t")))
             ('t (setq current-string-value (format-message "%s%c" current-string-value (char-after))))
             )
            (setq state 'in-string-value)
            )
           ('t
            ;; this happening is always a bug
            (error "locator went into unexpected state %s" (symbol-name state))))
          (forward-char))
        entries))))

(defun locator--parse-fail (state)
  (error "Unexpected character '%c' at %d when parsing %s in %s" (char-after) (point) (symbol-name state) (buffer-file-name)))

(defun locator-goto-entry-key (candidates)
  (seq-let (file-path lang-key lang-value key-point) (car candidates)
    (find-file file-path)
    (goto-char key-point)))

(provide 'locator)
