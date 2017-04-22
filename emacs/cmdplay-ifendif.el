;;; CMDPLAY-IFENDIF.EL - Short description here
;;; Copyright (C) 2017 Dr.Sc.KAWAMOTO,Takuji (Ext)
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
;;; ifendif command related
;;;      gray out C/C++ source lines invalidated by `if directives'.
;;;

(defgroup cmdplay-ifendif nil
  "gray out invalidated code by #if #endif directives."
  :group 'c)

(defface cmdplay-ifendif-shadow '((t (:inherit shadow)))
  "Face for shadowing regions invalidated by #if #endif directives."
  :group 'cmdplay-ifendif
  :version "23.1")

(easy-mmode-define-minor-mode
 ifendif-mode
 "`ifendif-mode` is a minor-mode for gray out invalid lines by
`#if #endif directives`, with C/C++ language source codes.
It uses cmdrecplay and clang's pre-processor as the backend
then refers to the macro definitions at compile time,
such as -D, -I, -W, -f, -std, etc. on command line,
and at current time, such as #define, #undef in your source codes,
to judge validity.

So, no matter how complex your macro is, how complex your `if directive` is,
or how complex your make file is, `ifendif' correctly determines whether a line
is valid or invalid, just like at compile time.
"
 ;; initial value
 nil
 " IfEndifMode"
 '(("\C-ci0" . cmdplay-ifendif-mode)
   ("\C-cir" . cmdplay-ifendif-gray-out-invalidated)
   ("\C-ciR" . cmdplay-ifendif-gray-out-invalidated-all-buffers)
   ))

(defun cmdplay-ifendif-mode ()
  (interactive)
  (if ifendif-mode
      (progn
        (cmdplay-ifendif-show-all)
        (ifendif-mode 0))
    (when (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (ifendif-mode 1)
      (cmdplay-ifendif-gray-out-invalidated))))

(defun cmdplay-ifendif-gray-out-region (start end)
  (when ifendif-mode
    (remove-overlays start end 'cmdplay-ifendif t)
    (let ((o (make-overlay start end)))
      (overlay-put o 'cmdplay-ifendif t)
      (overlay-put o 'face 'cmdplay-ifendif-shadow))))

(defun cmdplay-ifendif-show-region (start end)
  "Everything between START and END is made visible."
  (remove-overlays start end 'cmdplay-ifendif t))

(defun cmdplay-ifendif-gray-out-line (point)
  "Gray out the line containing point."
  (when ifendif-mode
    (save-excursion
      (goto-char point)
      (cmdplay-ifendif-gray-out-region (line-beginning-position)
                                       (progn (end-of-line) (point))))))

(defun cmdplay-ifendif-show-line (point)
  "Show the line containing point."
  (save-excursion
    (goto-char point)
    (cmdplay-ifendif-show-region (line-beginning-position)
                                 (progn (end-of-line) (point)))))

(defun cmdplay-ifendif-gray-out-lines (start-line-number end-line-number)
  "Gray out the lines."
  (when ifendif-mode
    (save-excursion
      (cmdplay-ifendif-gray-out-region
       (progn (goto-line start-line-number) (line-beginning-position) (point))
       (progn (goto-line end-line-number) (end-of-line) (point))))))

(defun cmdplay-ifendif-show-lines (start-line-number end-line-number)
  "Show the lines."
  (save-excursion
    (cmdplay-ifendif-show-region
     (progn (goto-line start-line-number) (line-beginning-position) (point))
     (progn (goto-line end-line-number) (end-of-line) (point)))))

(defun cmdplay-ifendif-show-lines-to-end-of-file (start-line-number)
  "Show the lines."
  (save-excursion
    (cmdplay-ifendif-show-region
     (progn (goto-line start-line-number) (line-beginning-position) (point))
     (point-max))))

(defun cmdplay-ifendif-show-all ()
  "Show all lines."
  (interactive)
  (cmdplay-ifendif-show-region 1 (point-max)))

(defun cmdplay-ifendif-gray-out-current-line ()
  "Gray out the current line."
  (interactive)
  (when ifendif-mode
    (cmdplay-ifendif-gray-out-line (point))))

(defun cmdplay-ifendif-show-current-line ()
  "Show the current line."
  (interactive)
  (cmdplay-ifendif-show-line (point)))

(defun cmdplay-ifendif-gray-out-invalidated ()
  "Gray out the lines invalidated by #if #endif directives."
  (interactive)
  (when ifendif-mode
    (let ((arg (format "%s" buffer-file-name))
          (nextline 1)
          (doexec nil))
      (when buffer-file-name
        (save-excursion
          (set-buffer "*ifendif output*")
        (erase-buffer)
        (message (format "cmdplay-ifendif: `ifendif -e %s'" arg))
        (call-process "ifendif" nil "*ifendif output*" nil "-e" arg)
        (goto-line 2)
        (setq nextline (- (point) 1))
        (setq doeval (string= (buffer-substring 1 2) "("))
        (if doeval
            nil
          (goto-char 1)
          (search-forward " ")
          (message (buffer-substring (point) nextline))
          ))
        (if doeval (eval-buffer (get-buffer "*ifendif output*")))))))

(defun cmdplay-ifendif-gray-out-invalidated-buffer (buffer)
  (save-excursion
    (set-buffer buffer)
    (if ifendif-mode
        (cmdplay-ifendif-gray-out-invalidated))))

(defun cmdplay-ifendif-gray-out-invalidated-all-buffers ()
  "Gray out the lines invalidated by #if #endif directives for all buffers."
  (interactive)
  (mapcar 'cmdplay-ifendif-gray-out-invalidated-buffer (buffer-list)))

(add-hook 'after-save-hook 'cmdplay-ifendif-gray-out-invalidated-all-buffers)

(get-buffer-create "*ifendif output*")

(provide 'cmdplay-ifendif)
