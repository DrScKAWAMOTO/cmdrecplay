;;; CMDPLAY.EL - Short description here
;;; Copyright (C) 2014-2016 Dr.Sc.KAWAMOTO,Takuji (Ext)
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to <kawamoto@spike.anavas>) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;

;;; After installation, append the following line in your emacs startup file:
;;;
;;;     (require 'cmdplay)
;;;     (add-hook 'c-mode-common-hook 'cmdplay-clang-set-cflags)
;;;
;;; To associate with the `compile' command, write as follows too.
;;;
;;;     (require 'cmdplay)
;;;     (setq compile-command "LANG=C cmdrec -- make -k ")
;;;     (advice-add 'compile :around #'cmdplay-clang-advice-around-compile)
;;;     (advice-add 'recompile :around #'cmdplay-clang-advice-around-compile)
;;;
;;; First, per your project:
;;;
;;;     cd <your_project_directory_which_contains_your_makefile>
;;;     cmdrec -- make
;;;
;;; If your project is cmake, you should do as follows instead:
;;;
;;;     cd <your_project_directory_which_contains_your_CMakeLists.txt>
;;;     mkdir build
;;;     cd build
;;;     cmdrec -- sh -c "cmake ..; make all"
;;;
;;; The `--` separates the parameters from the build command. The output sqlite3 file
;;; `~/.cmdrec.db` found in your home directory.

;;;
;;; flycheck related
;;;      define flycheck checkers
;;;          c/c++-cmdplay-clang
;;;          c/c++-cmdplay-cppcheck
;;;

(require 'flycheck)

(flycheck-define-checker c/c++-cmdplay-clang
  "A C/C++ syntax checker using Clang and cmdplay.

See URL `http://clang.llvm.org/' and `https://github.com/DrScKAWAMOTO/cmdrecplay'."
  :command ("cmdplay"
            "-f" source-original
            "--"
            (eval (pcase major-mode (`c++-mode "clang++-on-the-fly-check")
                         (`c-mode "clang-on-the-fly-check")))
            "-x" (eval (pcase major-mode (`c++-mode "c++") (`c-mode "c")))
            ;; We must stay in the same directory, to properly resolve #include
            ;; with quotes
            source-inplace)
  :error-patterns
  ((error line-start
          (message "In file included from") " " (file-name) ":" line ":"
          line-end)
   (info line-start (file-name) ":" line ":" column
            ": note: " (message) line-end)
   (warning line-start (file-name) ":" line ":" column
            ": warning: " (message) line-end)
   (error line-start (file-name) ":" line ":" column
          ": " (or "fatal error" "error") ": " (message) line-end))
  :modes (c-mode c++-mode)
  :next-checkers ((warnings-only . c/c++-cmdplay-cppcheck)))

(flycheck-define-checker c/c++-cmdplay-cppcheck
  "A C/C++ checker using cppcheck and cmdplay.

See URL `http://cppcheck.sourceforge.net/' and `https://github.com/DrScKAWAMOTO/cmdrecplay'."
  :command ("cmdplay"
            "-f" source-original
            "--"
            "cppcheck-on-the-fly-check"
            ;; We must stay in the same directory, to properly resolve #include
            ;; with quotes
            source-inplace)
  :error-parser flycheck-parse-cppcheck
  :modes (c-mode c++-mode))

(setq flycheck-checkers (cons 'c/c++-cmdplay-cppcheck flycheck-checkers))
(setq flycheck-checkers (cons 'c/c++-cmdplay-clang flycheck-checkers))
(setq flycheck-disabled-checkers (cons 'c/c++-cppcheck flycheck-disabled-checkers))
(setq flycheck-disabled-checkers (cons 'c/c++-clang flycheck-disabled-checkers))

;;;
;;; auto-complete related
;;;      define function of automatic setting ac-clang-cflags
;;;

(defun cmdplay-clang-set-cflags-buffer (buffer)
  "Set `ac-clang-cflags' of BUFFER to new cflags for ac-clang by executing cmdplay."
  (interactive)
  (save-excursion
    (set-buffer buffer)
    (let* ((arg2 (format "%s" buffer-file-name))
           (arg1 (cond ((eq major-mode 'c-mode) "clang-completion")
                       (t "clang++-completion")))
           (cmd (format "cmdplay -- %s %s" arg1 arg2)))
      (when buffer-file-name
        (message (format "execute `%s'" cmd))
        (setq ac-clang-cflags
              (split-string
               (shell-command-to-string cmd)))
        (ac-clang-update-cmdlineargs)))))

(defun cmdplay-clang-set-cflags-all-buffers ()
  "Set `ac-clang-cflags' of all buffers to new cflags for ac-clang by executing cmdplay."
  (interactive)
  (mapcar 'cmdplay-clang-set-cflags-buffer (buffer-list)))

(defun cmdplay-clang-set-cflags ()
  "Set `ac-clang-cflags' to new cflags for ac-clang by executing cmdplay."
  (interactive)
  (cmdplay-clang-set-cflags-buffer (current-buffer)))

(defun cmdplay-clang-sentinel (process event)
  (when (string= event "finished\n")
    (mapcar 'cmdplay-clang-set-cflags-buffer (buffer-list))))

(defun cmdplay-clang-advice-around-compile (f &rest args)
  "Set `ac-clang-cflags' of all buffers to new cflags for ac-clang by executing cmdplay."
  (let ((ret (apply f args)))
    (message "cmdplay-clang-advice-around-compile called")
    (set-process-sentinel (get-buffer-process ret) 'cmdplay-clang-sentinel)
    ret))

(provide 'cmdplay)
