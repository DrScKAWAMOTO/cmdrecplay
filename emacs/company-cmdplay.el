;;; company-cmdplay.el --- company-mode completion backend for Cmdplay  -*- lexical-binding: t -*-

;; Copyright (C) 2009, 2011, 2013-2019  Free Software Foundation, Inc.

;; Author: Nikolaj Schumacher

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

(require 'company)
(require 'company-template)
(require 'cl-lib)

(defgroup company-cmdplay nil
  "Completion backend for Cmdplay."
  :group 'company
  :group 'cmdplay)

(defcustom company-cmdplay-executable
  (executable-find "cmdplay")
  "Location of cmdplay executable."
  :type 'file)

(defcustom company-cmdplay-begin-after-member-access t
  "When non-nil, automatic completion will start whenever the current
symbol is preceded by \".\", \"->\" or \"::\", ignoring
`company-minimum-prefix-length'.

If `company-begin-commands' is a list, it should include `c-electric-lt-gt'
and `c-electric-colon', for automatic completion right after \">\" and
\":\"."
  :type 'boolean)

(defcustom company-cmdplay-use-compile-flags-txt nil
  "When non-nil, use flags from compile_flags.txt if present.

The lines from that files will be appended to `company-cmdplay-arguments'.

And if such file is found, Cmdplay is called from the directory containing
it.  That allows the flags use relative file names within the project."
  :type 'boolean
  :safe 'booleanp)

(defcustom company-cmdplay-arguments nil
  "Additional arguments to pass to cmdplay when completing.
Prefix files (-include ...) can be selected with `company-cmdplay-set-prefix'
or automatically through a custom `company-cmdplay-prefix-guesser'."
  :type '(repeat (string :tag "Argument")))

(defcustom company-cmdplay-prefix-guesser 'company-cmdplay-guess-prefix
  "A function to determine the prefix file for the current buffer."
  :type '(function :tag "Guesser function" nil))

(defvar company-cmdplay-modes '(c-mode c++-mode objc-mode)
  "Major modes which cmdplay may complete.")

(defcustom company-cmdplay-insert-arguments t
  "When non-nil, insert function arguments as a template after completion."
  :type 'boolean
  :package-version '(company . "0.8.0"))

;; prefix ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-cmdplay--prefix nil)

(defsubst company-cmdplay--guess-pch-file (file)
  (let ((dir (directory-file-name (file-name-directory file))))
    (when (equal (file-name-nondirectory dir) "Classes")
      (setq dir (file-name-directory dir)))
    (car (directory-files dir t "\\([^.]h\\|[^h]\\).pch\\'" t))))

(defsubst company-cmdplay--file-substring (file beg end)
  (with-temp-buffer
    (insert-file-contents-literally file nil beg end)
    (buffer-string)))

(defun company-cmdplay-guess-prefix ()
  "Try to guess the prefix file for the current buffer."
  ;; Prefixes seem to be called .pch.  Pre-compiled headers do, too.
  ;; So we look at the magic number to rule them out.
  (let* ((file (company-cmdplay--guess-pch-file buffer-file-name))
         (magic-number (and file (company-cmdplay--file-substring file 0 4))))
    (unless (member magic-number '("CPCH" "gpch"))
      file)))

(defun company-cmdplay-set-prefix (&optional prefix)
  "Use PREFIX as a prefix (-include ...) file for cmdplay completion."
  (interactive (let ((def (funcall company-cmdplay-prefix-guesser)))
     (unless (stringp def)
       (setq def default-directory))
     (list (read-file-name "Prefix file: "
                           (when def (file-name-directory def))
                           def t (when def (file-name-nondirectory def))))))
  ;; TODO: pre-compile?
  (setq company-cmdplay--prefix (and (stringp prefix)
                                   (file-regular-p prefix)
                                   prefix)))

;; Clean-up on exit.
(add-hook 'kill-emacs-hook 'company-cmdplay-set-prefix)

;; parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Handle Pattern (syntactic hints would be neat).
;; Do we ever see OVERLOAD (or OVERRIDE)?
(defconst company-cmdplay--completion-pattern
  "^COMPLETION: \\_<\\(%s[a-zA-Z0-9_:]*\\)\\(?:\\(?: (InBase)\\)? : \\(.*\\)$\\)?$")

(defconst company-cmdplay--error-buffer-name "*cmdplay-error*")

(defun company-cmdplay--lang-option ()
     (if (eq major-mode 'objc-mode)
         (if (string= "m" (file-name-extension buffer-file-name))
             "objective-c" "objective-c++")
       (substring (symbol-name major-mode) 0 -5)))

(defun company-cmdplay--parse-output (prefix _objc)
  (goto-char (point-min))
  (let ((pattern (format company-cmdplay--completion-pattern
                         (regexp-quote prefix)))
        (case-fold-search nil)
        (results (make-hash-table :test 'equal :size (/ (point-max) 100)))
        lines match)
    (while (re-search-forward pattern nil t)
      (setq match (match-string-no-properties 1))
      (unless (equal match "Pattern")
        (save-match-data
          (when (string-match ":" match)
            (setq match (substring match 0 (match-beginning 0)))))
        (let ((meta (match-string-no-properties 2)))
          ;; Avoiding duplicates:
          ;; https://github.com/company-mode/company-mode/issues/841
          (cond
           ;; Either meta != completion (not a macro)
           ((not (equal match meta))
            (puthash match meta results))
           ;; Or it's the first time we see this completion
           ((eq (gethash match results 'none) 'none)
            (puthash match nil results))))))
    (maphash
     (lambda (match meta)
       (when meta
         (put-text-property 0 1 'meta (company-cmdplay--strip-formatting meta) match))
       (push match lines))
     results)
    lines))

(defun company-cmdplay--meta (candidate)
  (get-text-property 0 'meta candidate))

(defun company-cmdplay--annotation (candidate)
  (let ((ann (company-cmdplay--annotation-1 candidate)))
    (if (not (and ann (string-prefix-p "(*)" ann)))
        ann
      (with-temp-buffer
        (insert ann)
        (search-backward ")")
        (let ((pt (1+ (point))))
          (re-search-forward ".\\_>" nil t)
          (delete-region pt (point)))
        (buffer-string)))))

(defun company-cmdplay--annotation-1 (candidate)
  (let ((meta (company-cmdplay--meta candidate)))
    (cond
     ((null meta) nil)
     ((string-match "[^:]:[^:]" meta)
      (substring meta (1+ (match-beginning 0))))
     ((string-match "(anonymous)" meta) nil)
     ((string-match "\\((.*)[ a-z]*\\'\\)" meta)
      (let ((paren (match-beginning 1)))
        (if (not (eq (aref meta (1- paren)) ?>))
            (match-string 1 meta)
          (with-temp-buffer
            (insert meta)
            (goto-char paren)
            (substring meta (1- (search-backward "<"))))))))))

(defun company-cmdplay--strip-formatting (text)
  (replace-regexp-in-string
   "#]" " "
   (replace-regexp-in-string "[<{[]#\\|#[>}]" "" text t)
   t))

(defun company-cmdplay--handle-error (res args)
  (goto-char (point-min))
  (let* ((buf (get-buffer-create company-cmdplay--error-buffer-name))
         (cmd (concat company-cmdplay-executable " " (mapconcat 'identity args " ")))
         (pattern (format company-cmdplay--completion-pattern ""))
         (message-truncate-lines t)
         (err (if (and (re-search-forward pattern nil t)
                       ;; Something in the Windows build?
                       ;; Looks like Cmdplay doesn't always include the error text
                       ;; before completions (even if exited with error).
                       (> (match-beginning 0) (point-min)))
                  (buffer-substring-no-properties (point-min)
                                                  (1- (match-beginning 0)))
                ;; Warn the user more aggressively if no match was found.
                (message "cmdplay failed with error %d: %s" res cmd)
                (buffer-string))))
    ;;(message "TODO company-cmdplay--handle-error")
    ;;(message "TODO     buf=%s" buf)
    ;;(message "TODO     cmd=%s" cmd)
    ;;(message "TODO     pattern=%s" pattern)
    ;;(message "TODO     err=%s" err)
    (when err (message "%s" (replace-regexp-in-string "[\n\r]+$" "" err)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (current-time-string)
                (format "\ncmdplay failed with error %d:\n" res)
                cmd "\n\n")
        (insert err)
        (setq buffer-read-only t)
        (goto-char (point-min))))))

(defun company-cmdplay--start-process (prefix callback &rest args)
  (let* ((objc (derived-mode-p 'objc-mode))
         (buf (get-buffer-create "*cmdplay-output*"))
         ;; Looks unnecessary in Emacs 25.1 and later.
         ;; (Inconclusive, needs more testing):
         ;; https://github.com/company-mode/company-mode/pull/288#issuecomment-72491808
         (process-adaptive-read-buffering nil)
         (existing-process (get-buffer-process buf)))
    (when existing-process
      (kill-process existing-process))
    (with-current-buffer buf
      (erase-buffer)
      (setq buffer-undo-list t))
    ;;(message "TODO company-cmdplay--start-process")
    ;;(message "TODO     cmd=%s" company-cmdplay-executable)
    ;;(message "TODO     args=%s" args)
    (message "company-cmdplay: %s %s" company-cmdplay-executable
             (mapconcat (lambda (x) (format "%s" x)) args " "))
    (let* ((process-connection-type nil)
           (process (apply #'start-file-process "company-cmdplay" buf
                           company-cmdplay-executable args)))
      (set-process-sentinel
       process
       (lambda (proc status)
         (unless (string-match-p "hangup\\|killed" status)
           (funcall
            callback
            (let ((res (process-exit-status proc)))
              (with-current-buffer buf
                (unless (eq 0 res)
                  (company-cmdplay--handle-error res args))
                ;; Still try to get any useful input.
                (company-cmdplay--parse-output prefix objc)))))))
      (send-region process (point-min) (point-max))
      (send-string process "\n")
      (process-send-eof process))))

(defsubst company-cmdplay--build-location (pos)
  (save-excursion
    (goto-char pos)
    (format "-:%d:%d"
            (line-number-at-pos)
            (1+ (length
                 (encode-coding-region
                  (line-beginning-position)
                  (point)
                  'utf-8
                  t))))))

(defsubst company-cmdplay--build-complete-args (pos)
  (let* ((lang-option (company-cmdplay--lang-option))
         (cmdplay-command (cond ((string= lang-option "c") "clang-company")
                                ((string= lang-option "c++") "clang++-company"))))
    (append '("-f" "-" "--")
            (list cmdplay-command)
            (list "-x" lang-option)
            (list buffer-file-name)
            (company-cmdplay--arguments)
            (when (stringp company-cmdplay--prefix)
              (list "-include" (expand-file-name company-cmdplay--prefix)))
            (list "-Xclang" (format "-code-completion-at=%s"
                                    (company-cmdplay--build-location pos))))))

(defun company-cmdplay--arguments ()
  (let ((fname "compile_flags.txt")
        (args company-cmdplay-arguments)
        current-dir-rel)
    (when company-cmdplay-use-compile-flags-txt
      (let ((dir (locate-dominating-file default-directory fname)))
        (when dir
          (setq current-dir-rel (file-relative-name default-directory dir))
          (setq default-directory dir)
          (with-temp-buffer
            (insert-file-contents fname)
            (setq args
                  (append
                   args
                   (split-string (buffer-substring-no-properties
                                  (point-min) (point-max))
                                 "[\n\r]+"
                                 t
                                 "[ \t]+"))))
          (unless (equal current-dir-rel "./")
            (push (format "-I%s" current-dir-rel) args)))))
    args))

(defun company-cmdplay--candidates (prefix callback)
  (when (null company-cmdplay--prefix)
    (company-cmdplay-set-prefix (or (funcall company-cmdplay-prefix-guesser)
                                  'none)))
  (let ((default-directory default-directory))
    (apply 'company-cmdplay--start-process
           prefix
           callback
           (company-cmdplay--build-complete-args
            (if t
                (point)
              (- (point) (length prefix)))))))

(defun company-cmdplay--prefix ()
  (if company-cmdplay-begin-after-member-access
      (company-grab-symbol-cons "\\.\\|->\\|::" 2)
    (company-grab-symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst company-cmdplay-required-version 1.1)

(defun company-cmdplay (command &optional arg &rest ignored)
  "`company-mode' completion backend for Cmdplay.
Cmdplay is a parser for C and ObjC.

Additional command line arguments can be specified in
`company-cmdplay-arguments'.  Prefix files (-include ...) can be selected
with `company-cmdplay-set-prefix' or automatically through a custom
`company-cmdplay-prefix-guesser'."
  (interactive (list 'interactive))
  ;;(message "TODO company-cmdplay %s %s %s" command arg ignored)
  (cl-case command
    (interactive (company-begin-backend 'company-cmdplay))
    (init (when (memq major-mode company-cmdplay-modes)
            (unless company-cmdplay-executable
              (message "error: Company found no cmdplay executable")
              (error "Company found no cmdplay executable"))))
    (prefix (and (memq major-mode company-cmdplay-modes)
                 buffer-file-name
                 company-cmdplay-executable
                 (not (company-in-string-or-comment))
                 (or (company-cmdplay--prefix) 'stop)))
    (candidates (cons :async
                      (lambda (cb) (company-cmdplay--candidates arg cb))))
    (meta       (company-cmdplay--meta arg))
    (kind (company-cmdplay--kind arg))
    (annotation (company-cmdplay--annotation arg))
    (post-completion (let ((anno (company-cmdplay--annotation arg)))
                       (when (and company-cmdplay-insert-arguments anno)
                         (insert anno)
                         (if (string-match "\\`:[^:]" anno)
                             (company-template-objc-templatify anno)
                           (company-template-c-like-templatify
                            (concat arg anno))))))))

(defun company-cmdplay--kind (arg)
  ;; XXX: Not very precise.
  ;; E.g. it will say that an arg-less ObjC method is a variable (perhaps we
  ;; could look around for brackets, etc, if there any actual users who's
  ;; bothered by it).
  ;; And we can't distinguish between local vars and struct fields.
  ;; Or between keywords and macros.
  (let ((meta (company-cmdplay--meta arg)))
    (cond
     ((null meta) 'keyword)
     ((string-match "(" meta)
      (if (string-match-p (format "\\`%s *\\'" (regexp-quote arg))
                          (substring meta 0 (match-beginning 0)))
          'keyword ; Also macro, actually (no return type).
        'function))
     (t 'variable))))

(provide 'company-cmdplay)
;;; company-cmdplay.el ends here
