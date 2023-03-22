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

(require 'helm)
(require 'project)

(defcustom locator-key-display-length 40
  "locator-key-display-length specifies the number of characters
  the translate entry keys should occupy on the screen in the
  entry find."
  :type 'integer
  :group 'locator)

(defcustom locator-key-join-separator "."
  "locator-key-join-separator defines a string used to join the
key artifacts from a lang json file into a composite key."
  :type 'string
  :group 'locator)

;; (string-match locator-lang-file-name-pattern "de.json")
;; (string-match locator-lang-file-name-pattern "de-DE.json")
(defcustom locator-lang-file-name-pattern "^[a-z]\\{2,3\\}\\([-_][A-Z]\\{2,3\\}\\)?\\.json$"
  "locator-lang-file-name-pattern is a regular expression which
must match the nondirectory part of valid lang json files."
  :type 'string
  :group 'locator)

(defcustom locator-key-builder 'locator-join-key
  "locator-key-builder is a function for generating composite
keys from keys in the long json files.

The function is called with the follwing arguments. The first
argument key-artifacts is a list of keys from the json file which
point the current entry.

The second argument lang-file-path is the path of the lang json
file.

The function must return the lang entrie's global key as string."
  :type 'function
  :group 'locator)

(defcustom locator-key-splitter 'locator-split-key
  "locator-key-splitter must implement the reverse operation
provided by locator-key-builder.

The function is called with the following arguments. The first
argument key is the to be splitted key as string.

The second argument lang-file-path is the path of the lang json
file."
  :type 'function
  :group 'locator)

;;;###autoload
(defun locator-find-entry (locale)
  (interactive "sLocale: ")
  (helm
   :prompt
   "Localization: "
   :sources
   (helm-build-sync-source "localized strings"
     :candidates
     (lambda ()
       (seq-filter
        (lambda (entry)
          (alist-get 'value entry))
        (locator--collect-lang-entries locale)))
     :candidate-transformer
     (lambda (candidates)
       (mapcar
        (lambda (candidate)
            (list (locator--format-entry-candidate (alist-get 'key candidate) (alist-get 'value candidate)) candidate))
        candidates))
     :action
     '(
       ("Goto" . locator-goto-entry-key)
       ("Insert key" . locator-insert-key)
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

;; (locator--collect-lang-entries "en")
(defun locator--collect-lang-entries (locale)
  (apply
   'append
   (mapcar
    (lambda (lang-file-path)
      (mapcar
       (lambda (entry)
         (add-to-list 'entry `(file-path . ,lang-file-path)))
       (locator--read-lang-file lang-file-path)))
    (locator--find-lang-files locale))))

;; (message "lang-file %s" (locator--read-lang-file "test/en.json"))
(defun locator--read-lang-file (file-path)
  """locator--read-lang-file visits file-path and parses the lang json within.

The function returns an assoc list of lang entry tuples. Each
tuple contains the following fields.

The key field is the combined key produced by joining the nested
JSON keys.

The value field is the string value of the found key. May be nil
if the key points to an object.

The point field depends on value. If value is nil then point is
the point in the buffer where the opening curly brace of the json
object is. If value is a string then point is the point in the
buffer where the key's string content begins. So it is the
character after the string's opening quotes.
"""
  (with-current-buffer (find-file-noselect file-path)
    (save-excursion
      (goto-char (point-min))
      (let (
            (state 'detect-value)
            (entries '())
            (key-artifacts '())
            current-key-start-point
            current-key-end-point
            current-string-start-point
            current-string-value)
        (while (not (eobp))
          (cond
           ((eq state 'detect-value)
            (cond
             ((eq (char-after) ? ) t)
             ((eq (char-after) ?{)
              (setq entries (append
                             entries
                             `((
                                (type . object)
                                (key . ,(funcall locator-key-builder key-artifacts file-path))
                                (value . ())
                                (value-start-point . ,(point))
                                )))
                    state 'in-obj))
             ((eq (char-after) ?\")
              (setq current-string-value ""
                    current-string-start-point (+ (point) 1)
                    state 'in-string-value))
             ('t (locator--parse-fail state)))
            )
           ((eq state 'in-obj)
            (cond
             ((eq (char-after) ?\n) t)
             ((eq (char-after) ?\t) t)
             ((eq (char-after) ? ) t)
             ((eq (char-after) ?,) t)
             ((eq (char-after) ?\")
              (setq current-key-start-point (+ (point) 1)
                    state 'in-key))
             ((eq (char-after) ?})
              (setq key-artifacts (butlast key-artifacts)))
             ('t (locator--parse-fail state)))
            )
           ((eq state 'in-key)
            (cond
             ((eq (char-after) ?\")
              (setq current-key-end-point (point)
                    state 'before-assignment))
             ('t 't)
             ))
           ((eq state 'before-assignment)
            (cond
             ((eq (char-after) ?:)
              (setq state 'detect-value
                    key-artifacts (append key-artifacts `(,(buffer-substring-no-properties current-key-start-point current-key-end-point)))))
             ('t (locator--parse-fail state))
             ))
           ((eq state 'in-string-value)
            (cond
             ((eq (char-after) ?\\)
              (setq state 'in-string-value-escape))
             ((eq (char-after) ?\")
              (setq entries (append
                             entries
                             `((
                                (type . value)
                                (key . ,(funcall locator-key-builder key-artifacts file-path))
                                (value . ,current-string-value)
                                (key-start-point . ,current-key-start-point)
                                (key-end-point . ,current-key-end-point)
                                (value-start-point . ,current-string-start-point)
                                (value-end-point . ,(point))
                                )))
                    key-artifacts (butlast key-artifacts)
                    state 'in-obj))
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
  (let ((entry (car candidates)))
    (find-file (alist-get 'file-path entry))
    (goto-char (alist-get 'point entry))))

(defun locator-insert-key (candidates)
  (mapc
   (lambda (entry)
     (insert (alist-get 'key entry)))
   candidates))

(defun locator-join-key (key-artifacts lang-file-path)
  (string-join key-artifacts locator-key-join-separator))

(defun locator-split-key (key lang-file-path)
  (split-string key (regexp-quote locator-key-join-separator)))

;;;###autoload
(defun locator-modify-entry (key)
  """locator-modify-entry creates or modifies localizations.
"""
  (interactive "sKey: ")
  (helm
   :prompt
   "Locale dir: "
   :sources
   (helm-build-sync-source "localized-strings"
     :candidates
     (locator--collect-locale-dirs)
     :action
     `(
       ("Complete values" . ,(lambda (locale-dir)
                               (locator-complete-values key locale-dir)))
       ))))

(defun locator--collect-locale-dirs ()
  ;; TODO for now we only support projects with exactly one project root
  (let ((prl (length (locator--single-project-root))))
    (sort
     (mapcar
      (lambda (locale-dir)
        (if (> (length locale-dir) prl)
            (substring locale-dir prl (- (length locale-dir) 1))
          locale-dir
          ))
      (seq-uniq
       (mapcar
        'file-name-directory
        (locator--find-lang-files ()))))
     'string<)))

(defun locator-complete-values (key locale-dir)
  (let ((abs-locale-dir (concat (locator--single-project-root) locale-dir "/")))
    (mapc
     (lambda (lang-path)
       (when (string-prefix-p abs-locale-dir lang-path)
         (let ((value (read-from-minibuffer (concat (string-remove-suffix ".json" (file-name-nondirectory lang-path)) " value: "))))
           (unless (string= value "")
             (locator--set-translation
              lang-path
              key
              value
              )))))
     (locator--find-lang-files))))

;; (locator--set-translation "test/en.json" "a-group.following-key" "a value")
;; (locator--set-translation "test/en.json" "a-group.new-group.with-new-key" "a new value")
(defun locator--set-translation (lang-path key value)
  (let ((entries (locator--read-lang-file lang-path)))
    (with-current-buffer (find-file-noselect lang-path)
      (save-excursion
        (let ((matching-entry (seq-find
                               (lambda (entry)
                                 (string= key (alist-get 'key entry)))
                               entries)))
          (if matching-entry
              (progn
                (goto-char (alist-get 'value-start-point matching-entry))
                (insert-before-markers (locator--escape-string value))
                (delete-char (- (alist-get 'value-end-point matching-entry) (alist-get 'value-start-point matching-entry))))
            (locator--append-translation lang-path entries key value)))))))

(defun locator--append-translation (lang-path entries key value)
  (let ((existing-parent
         (seq-find
          'identity
          (mapcar
           (lambda (parent-key)
             (seq-find
              (lambda (entry)
                (string= (alist-get 'key entry) parent-key))
              entries))
           (locator--key-parents key lang-path))
          )))
    (goto-char (alist-get 'value-start-point existing-parent))
    (forward-char) ;; jump over the {
    (locator--insert-tree (locator--remaining-keys lang-path (alist-get 'key existing-parent) key) value)
    (insert ",")
    (json-pretty-print-buffer)))

;; (locator--key-parents "fake-path.json" "my.long.key")
(defun locator--key-parents (key lang-path)
  """locator--key-parents returns a list with key's parents.

The lists always ends with "".
"""
  (let ((key-parts (funcall locator-key-splitter key lang-path))
        (keys ()))
    (dotimes (i (length key-parts))
      (add-to-list 'keys (funcall locator-key-builder (seq-subseq key-parts 0 i) lang-path)))
    keys))

;; (locator--remaining-keys "ye-fake.json" "ye.parent" "ye.parent.with.key")
;; (locator--remaining-keys "ye-fake.json" "" "other.key")
(defun locator--remaining-keys (lang-path parent key)
  (if (eq (length parent) 0)
      (funcall locator-key-splitter key lang-path)
    (seq-subseq
     (funcall locator-key-splitter key lang-path)
     (length (funcall locator-key-splitter parent lang-path)))))

;; (locator--insert-tree '("a" "b") "val")
(defun locator--insert-tree (remaining-keys value)
  (insert (concat "\"" (car remaining-keys) "\":"))
  (if (eq (length remaining-keys) 1)
      (insert (concat "\"" value "\""))
    (insert "{")
    (locator--insert-tree (cdr remaining-keys) (locator--escape-string value))
    (insert "}")
  ))

;; (locator--escape-string "this \"is\" it")
(defun locator--escape-string (s)
  (seq-reduce
   (lambda (value replacement)
     (seq-let (pattern substitute) replacement
       (replace-regexp-in-string pattern substitute value)))
   '(("[\\]" "\\\\") ("\n" "\\\\n") ("\"" "\\\\\""))
   s))

(defun locator--single-project-root ()
  (let ((prs (project-roots (project-current t))))
    (when (not (= (length prs) 1))
      ;; you are welcome to fix this with a pull request :)
      (error "locator only supports projects with one root directory for now"))
    (expand-file-name (car prs))))

(defun locator--find-lang-files (&optional locale-prefix)
  """locator--find-lang-files returns the path of certain json files which contains translations.

The locale-prefix argument specifies a prefix which must be
present in the found file names. The prefix is ignored if the
argument is nil.
"""
  (let ((pr (project-current t)))
    (seq-filter
     (lambda (file-path)
       (let ((file-name (file-name-nondirectory file-path)))
         (and
          (string-match locator-lang-file-name-pattern file-name)
          (or
           (eq locale-prefix ())
           (string-prefix-p (concat locale-prefix ".") file-name)
           (string-prefix-p (concat locale-prefix "-") file-name)))))
     (project-files pr (project-roots pr)))))

(provide 'locator)
