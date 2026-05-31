;;; zathura.el --- Summary -*- lexical-binding: t -*-

;; Author: Aleksandr Kuzmin
;; Maintainer: Aleksandr Kuzmin
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))
;; Homepage: https://codeberg.org/treflip/zathura.el
;; Keywords: convenience

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Zathura <https://pwmt.org/projects/zathura/> is a highly customizable and
;; functional document viewer, external to Emacs.  This package provides a set
;; of simple commands for creating hyperlinks to open documents in Zathura.

;;; Code:

(require 'dbus)
(require 'cl-lib)
(require 'json)
(require 'outline)

(defvar zathura-service-path "/org/pwmt/zathura")
(defvar zathura-service-iname "org.pwmt.zathura")
(defvar zathura-session-proc nil
  "Current zathura D-Bus process bound to this buffer.")

(defcustom zathura-link-file-path-type 'absolute
  "How zathura inserts PDF file paths."
  :type '(choice (const :tag "Absolute path" absolute)
                 (const :tag "Relative to current buffer" relative))
  :group 'zathura)

(defcustom zathura-outline-page-column 80
  "Column used to display page number in zathura outline."
  :type 'integer
  :group 'zathura)

(defcustom zathura-outline-indent 2
  "Indent width for zathura outline."
  :type 'integer
  :group 'zathura)

(defcustom zathura-outline-numbered t
  "Whether to display hierarchical numbers in zathura outline."
  :type 'boolean
  :group 'zathura)

(defun zathura--get-procs ()
  "Retrieve all the running processes of zathura."
  (cl-remove-if-not (lambda (p) (string-match "zathura" p))
                    (dbus-list-names :session)))


(defun zathura--dump-interface ()
  "Get the D-Bus interface of zathura.  For dbug puproses."
  (dbus-introspect-get-interface :session
                                 (elt (zathura--get-procs) 0)
                                 zathura-service-path
                                 zathura-service-iname))


(defun zathura--annotate-candidate (proc)
  "Annotate `PROC' for completion with its file and page."
  (condition-case nil
      (propertize
       (format "   %s::%s"
               (zathura--get-file-path proc)
               (zathura--get-page-number proc))
       'face 'font-lock-comment-face)
    (dbus-error
     (propertize
      "   no document open"
      'face 'font-lock-comment-face))))


(defun zathura--pick-process (procs)
  "Pick a process out of `PROCS' interactively."
  (completing-read "Select process: "
                   (lambda (str pred action)
                     (if (eq action 'metadata)
                         `(metadata
                           (annotation-function . zathura--annotate-candidate))
                       (complete-with-action action procs str pred)))))


(defun zathura-get-link-details ()
  "Retrieve the link details: a file and page from one of the running
zathura processes."
  (let* ((procs (zathura--get-procs))
         (service)
         (page)
         (file))
    (cond ((= 1 (length procs))
           (setq service (car procs)))
          ((> (length procs) 1)
           (setq service (zathura--pick-process procs)))
          ((< (length procs) 1)
           (error "Zathura is not running")))
    (setq page (dbus-get-property :session service zathura-service-path
                                  zathura-service-iname "pagenumber"))
    (setq file (dbus-get-property :session service zathura-service-path
                                  zathura-service-iname "filename"))
    (cons file page)))


(defun zathura--get-file-path (proc)
  "Return the file path opened by zathura PROC."
  (condition-case nil
      (dbus-get-property :session
                         proc
                         zathura-service-path
                         zathura-service-iname
                         "filename")
    (dbus-error "")))


(defun zathura--get-page-number (proc)
  "Return the current page of file opened by zathura PROC."
  (+ (dbus-get-property :session
                        proc
                        zathura-service-path
                        zathura-service-iname
                        "pagenumber") 1))


(defun zathura--get-document-index (proc)
  "Return parsed document index of zathura PROC."
  (condition-case nil
      (alist-get 'index
                 (json-parse-string
                  (dbus-get-property :session
                                     proc
                                     zathura-service-path
                                     zathura-service-iname
                                     "documentinfo")
                  :object-type 'alist
                  :array-type 'list))
    (dbus-error nil)
    (json-parse-error nil)))


(defun zathura--open-document (proc file &optional page)
  "Open FILE and jump to PAGE (DEFAULT 0) in zathura PROC."
  (dbus-call-method :session
                    proc
                    zathura-service-path
                    zathura-service-iname
                    "OpenDocument"
                    file
                    ""
                    :int32 (or page 0)))

(defun zathura--goto-page (proc page)
  "Go to PAGE in zathura PROC."
  (dbus-call-method :session
                    proc
                    zathura-service-path
                    zathura-service-iname
                    "GotoPage"
                    :uint32
                    (- page 1)))

(defconst zathura--new-process-candidate "[New zathura process]")

(defun zathura--valid-proc-p (proc)
  (member proc (zathura--get-procs)))

(defun zathura--ensure-session-proc ()
  "Return live `zathura-session-proc', or signal an error."
  (unless (and zathura-session-proc
               (zathura--valid-proc-p zathura-session-proc))
    (zathura--assign-session-proc nil)
    (user-error "No zathura session process"))
  zathura-session-proc)

(defun zathura--assign-session-proc (proc)
  "Assign PROC as current `zathura-session-proc' and refresh outline buffer."
  (setq zathura-session-proc proc)
  (when (get-buffer "*zathura-outline*")
    (zathura-show-outline))
  zathura-session-proc)

(defun zathura--start-new-proc-with-file (file)
  "Start a new zathura process with FILE and return its D-Bus name."
  (let ((before (zathura--get-procs))
        after new)
    (start-process "zathura" nil "zathura" file)

    ;; wait zathura session bus
    (dotimes (_ 20)
      (sleep-for 0.1)
      (setq after (zathura--get-procs))
      (setq new (car (cl-set-difference after before :test #'string=)))
      (when new
        (cl-return)))

    (or new
        (error "Failed to start zathura D-Bus process"))))

(defun zathura--select-session-proc (file)
  "Select or create a zathura session process for FILE.
Set and return `zathura-session-proc'."
  (let ((procs (zathura--get-procs)))
    (zathura--assign-session-proc
     (cond
      ;; no proc, create proc and open file
      ((null procs)
       (zathura--start-new-proc-with-file file))

      ;; proc existed, let user to select existed proc or new proc
      (t
       (let* ((candidates
               (append procs (list zathura--new-process-candidate)))
              (choice
               (completing-read
                "Select zathura process: "
                (lambda (str pred action)
                  (if (eq action 'metadata)
                      '(metadata
                        (display-sort-function . identity)
                        (annotation-function
                         . (lambda (cand)
                             (if (string= cand zathura--new-process-candidate)
                                 (propertize "   start new zathura"
                                             'face 'font-lock-comment-face)
                               (zathura--annotate-candidate cand)))))
                    (complete-with-action action candidates str pred))))))
         (if (string= choice zathura--new-process-candidate)
             (zathura--start-new-proc-with-file file)
           choice))))))
  zathura-session-proc)

(defun zathura--pdf-file-p (file)
  "Return non-nil if FILE is a PDF file."
  (string-equal (downcase (or (file-name-extension file) ""))
                "pdf"))

(defun zathura--find-file-advice (orig-fun filename &rest args)
  "Open PDF files with `zathura-open-file' instead of visiting them."
  (if (zathura--pdf-file-p filename)
      (progn
        (zathura-open-file filename)
        nil)
    (apply orig-fun filename args)))

(defun zathura-open-link (path _)
  "Open Org pdf link PATH with zathura.
PATH format is FILE::PAGE."
  (let* ((parts (split-string path "::"))
         (file (car parts))
         (page (when-let ((page-str (cadr parts)))
                 (string-to-number page-str))))
    (zathura-open-file file page)))

(defun zathura--link-file-path (file)
  "Return FILE formatted according to `zathura-link-file-path-type'."
  (pcase zathura-link-file-path-type
    ('relative
     (file-relative-name (expand-file-name file) default-directory))
    (_
     (expand-file-name file))))


(define-derived-mode zathura-outline-mode outline-mode "Zathura-Outline"
  "Major mode for zathura outline."
  (setq-local outline-regexp "\\( *\\).")
  (setq-local outline-level
              (lambda ()
                (1+ (/ (length (match-string 1))
                       zathura-outline-indent))))
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defun zathura-outline--number-string (numbers)
  "Return outline number string from NUMBERS."
  (mapconcat #'number-to-string numbers "."))


(defun zathura-outline--insert-node (node numbers)
  "Insert outline NODE with hierarchical NUMBERS."
  (let* ((title (or (alist-get 'title node) ""))
         (page (alist-get 'page node))
         (children (alist-get 'sub-index node))
         (level (length numbers))
         (number (zathura-outline--number-string numbers))
         (beg (point)))
    (insert
     (format "%s%s%s"
             (make-string (* (1- level) zathura-outline-indent) ?\s)
             (if zathura-outline-numbered
                 (format "%s " number)
               "")
             title))

    (move-to-column zathura-outline-page-column t)
    (insert (format "%s\n" page))

    (add-text-properties
     beg (point)
     `(zathura-page ,page))
    (cl-loop for child in children
             for i from 1
             do (zathura-outline--insert-node
                 child
                 (append numbers (list i))))))

(defun zathura-outline-jump ()
  "Jump to the page of the outline item at point."
  (interactive)
  (let ((page (get-text-property (line-beginning-position)
                                 'zathura-page))
        (frame (selected-frame)))
    (unless page
      (user-error "No page on this line"))
    (zathura--goto-page
     (zathura--ensure-session-proc)
     page)
    (select-frame-set-input-focus frame)))

(defun zathura-outline--display (index)
  "Display zathura document INDEX."
  (let ((buf (get-buffer-create "*zathura-outline*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (zathura-outline-mode)
        (cl-loop for node in index
                 for i from 1
                 do (zathura-outline--insert-node node (list i)))
        (goto-char (point-min))))
    (pop-to-buffer buf)))

;;;###autoload
(define-minor-mode zathura-mode
  "View PDF files with zathura from Emacs."
  :global t
  :lighter " Zathura"
  (if zathura-mode
      (progn
        (advice-add 'find-file :around #'zathura--find-file-advice)
        (with-eval-after-load 'org
          (add-to-list 'org-file-apps '("\\.pdf\\'" . zathura-open-file))
          (org-link-set-parameters "pdf" :follow #'zathura-open-link)))
    (advice-remove 'find-file #'zathura--find-file-advice)
    (with-eval-after-load 'org
      (setq org-file-apps
            (remove '("\\.pdf\\'" . zathura-open-file) org-file-apps))
      )))

;;;###autoload
(defun zathura-open-file (file &optional page)
  "Open FILE in the current zathura session.
If `zathura-session-proc' is already bound and alive, use it.
Otherwise choose an existing zathura process or create a new one."
  (interactive
   (list (read-file-name "Open PDF: " nil nil t)
         current-prefix-arg))
  (setq file (expand-file-name file))
  (let* ((page (or page 0))
         (proc (condition-case nil
                   (zathura--ensure-session-proc)
                 (user-error
                  (zathura--select-session-proc file))))
         (current-file (zathura--get-file-path proc)))
    (if (string= current-file file)
        (zathura--goto-page proc page)
      (zathura--open-document proc file page))))

;;;###autoload
(defun zathura-select-proc ()
  "Select an existing zathura process and bind it as `zathura-session-proc'."
  (interactive)
  (let ((procs (zathura--get-procs)))
    (unless procs
      (user-error "No zathura process is running"))
    (zathura--assign-session-proc
     (zathura--pick-process procs))
    (message "Selected zathura process: %s" zathura-session-proc)
    zathura-session-proc))

;;;###autoload
(defun zathura (file &optional page)
  "Call zathura with the given `FILE' and `PAGE'."
  (if page
      (call-process "zathura" nil 0 nil "-P" (format "%s" page) file)
    (call-process "zathura" nil 0 nil file)))


;;;###autoload
(defun zathura-insert-hy-link ()
  "Insert a link in the format used by Hyperbole to the current page from
the chosen process of `zathura'."
  (interactive)
  (cl-destructuring-bind (file . page) (zathura-get-link-details)
    (insert (format "<zathura \"%s\" %s>" file page))))


;;;###autoload
(defun zathura-insert-org-link ()
  "Insert an Org pdf link to the current page from the zathura session."
  (interactive)
  (let* ((proc (zathura--ensure-session-proc))
         (file (zathura--get-file-path proc))
         (page (zathura--get-page-number proc)))
    (when (string= file "")
      (user-error "No document open in zathura"))
    (insert
     (format "[[pdf:%s::%s][%s]]"
             (zathura--link-file-path file)
             page
             (read-string "Description: ")))))

;;;###autoload
(defun zathura-insert-org-elisp-link ()
  "Insert an elisp org-link to the current page from the chosen process
of `zathura'"
  (interactive)
  (cl-destructuring-bind (file . page) (zathura-get-link-details)
    (insert (format "[[elisp:(zathura \"%s\" %s)][%s]]"
					file
					page
					(read-string "Description: ")))))


;;;###autoload
(defun zathura-show-outline ()
  "Show outline of current zathura document."
  (interactive)
  (zathura-outline--display
   (zathura--get-document-index
    (zathura--ensure-session-proc))))
(provide 'zathura)

;;; zathura.el ends here
