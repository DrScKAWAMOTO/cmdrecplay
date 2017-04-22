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

;;; After installation, append the following lines in your emacs startup file:
;;;
;;;     (setq cmdplay-use-flycheck t)
;;;     (setq cmdplay-use-clang-async t)
;;;     (setq cmdplay-use-ifendif t)
;;;     (require 'cmdplay)
;;;
;;; To associate with the `M-x compile` command, write follows instead.
;;;
;;;     (setq cmdplay-use-flycheck t)
;;;     (setq cmdplay-use-clang-async t)
;;;     (setq cmdplay-use-ifendif t)
;;;     (setq cmdplay-use-compile t)
;;;     (setq cmdplay-compile-command "LANG=C cmdrec -- make -k ")
;;;     (require 'cmdplay)
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
;;;    cmdrec -s ~/cmdskin -- cmake ..
;;;    cmdrec -s ~/cmdskin -- make all
;;;
;;; The `--` separates the options from the build command. You will find the output
;;; sqlite3 file `~/.cmdrec.db` in your home directory.
;;;
;;; You may specify `-s` option, in this case it's directory is used for `cmdskin`
;;; searching PATH. If this directory already exists, `cmdrec` shall not update it's
;;; contents.
;;; So delete the directory when the compiler options were not recorded correctly.
;;;
;;; For more options, you may check by passing `-h` option to `cmdrec` or `cmdplay`
;;; commands.
;;;
;;; From now on, you can edit your project's C/C++ source files by emacs, without
;;; specifying include pathes or macro defines explicitly for flycheck, auto completion,
;;; and ifendif.
;;; If your include pathes or macro defines are changed in your makefiles, then you need
;;; to executing `cmdrec -- make` by `M-x compile` again.
;;;

;;; Setq t if you use flycheck.
(defvar cmdplay-use-flycheck nil)
;;; Setq t if you use auto-complete-clang-async.
(defvar cmdplay-use-clang-async nil)
;;; Setq t if you use ifendif (gray out C/C++ source lines invalidated by `if direc...).
(defvar cmdplay-use-ifendif nil)
;;; Setq t to associate with the `compile' command.
(defvar cmdplay-use-compile nil)
;;; Set default compile-command value.
(defvar cmdplay-compile-command "LANG=C cmdrec -- make -k ")

(when cmdplay-use-flycheck (require 'cmdplay-flycheck))
(when cmdplay-use-clang-async (require 'cmdplay-clang-async))
(when cmdplay-use-ifendif (require 'cmdplay-ifendif))

(when (or cmdplay-use-clang-async cmdplay-use-ifendif)
  (defun cmdplay-c-common-hook ()
    (when cmdplay-use-clang-async (cmdplay-clang-set-cflags))
    (when cmdplay-use-ifendif (cmdplay-ifendif-mode))
  )
  (add-hook 'c-mode-common-hook 'cmdplay-c-common-hook t)  ;; append at the end
  )

(when cmdplay-use-compile
  (setq compile-command cmdplay-compile-command)
  (when (or cmdplay-use-clang-async cmdplay-use-ifendif)
    (defun cmdplay-sentinel (process event)
      (message "cmdplay-sentinel called")
      (when (string= event "finished\n")
        (message "compile finished")
        (when cmdplay-use-clang-async
          (mapcar 'cmdplay-clang-set-cflags-buffer (buffer-list)))
        (when cmdplay-use-ifendif
          (mapcar 'cmdplay-ifendif-gray-out-invalidated-buffer (buffer-list)))
        ))
    (defun cmdplay-advice-around-compile (f &rest args)
      "Advice function for cmdplay-clang-async and/or cmdplay-ifendif."
      (let ((ret (apply f args)))
        (message "cmdplay-advice-around-compile called")
        (set-process-sentinel (get-buffer-process ret) 'cmdplay-sentinel)
        ret))
    (advice-add 'compile :around #'cmdplay-advice-around-compile)
    (advice-add 'recompile :around #'cmdplay-advice-around-compile)
    ))

(provide 'cmdplay)
