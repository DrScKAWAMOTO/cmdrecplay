;;; CMDPLAY-CLANG-ASYNC.EL - Short description here
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

;;;
;;; auto-complete-clang-async related
;;;      define function of automatic setting ac-clang-cflags
;;;

(defun cmdplay-clang-set-cflags-by-cmdplay ()
  "Set `ac-clang-cflags' of current buffer to new cflags for ac-clang by executing cmdplay."
  (interactive)
  (let* ((arg2 (format "%s" buffer-file-name))
         (arg1 (cond ((eq major-mode 'c-mode) "clang-completion")
                     (t "clang++-completion")))
         (cmd (format "cmdplay -f %s -- %s %s" arg2 arg1 arg2)))
    (when buffer-file-name
      (message (format "cmdplay-clang-async: `%s'" cmd))
      (setq ac-clang-cflags
            (split-string
             (shell-command-to-string cmd)))
      (ac-clang-update-cmdlineargs))))

(defun cmdplay-clang-set-cflags-buffer (buffer)
  "Set `ac-clang-cflags' of BUFFER to new cflags for ac-clang by executing cmdplay."
  (interactive)
  (save-excursion
    (set-buffer buffer)
    (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
        (cmdplay-clang-set-cflags-by-cmdplay))))


(defun cmdplay-clang-set-cflags-all-buffers ()
  "Set `ac-clang-cflags' of all buffers to new cflags for ac-clang by executing cmdplay."
  (interactive)
  (mapcar 'cmdplay-clang-set-cflags-buffer (buffer-list)))

(defun cmdplay-clang-set-cflags ()
  "Set `ac-clang-cflags' to new cflags for ac-clang by executing cmdplay."
  (interactive)
  (cmdplay-clang-set-cflags-buffer (current-buffer)))

(provide 'cmdplay-clang-async)
