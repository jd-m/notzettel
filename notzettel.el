(require 'subr-x)
(require 's)
(require 'thingatpt)
(require 'notdeft)

(defvar notzettel-id-format "%Y%m%d%H%M%S")

(defvar notzettel-title-regex "[0-9A-Za-z ]+")
(defvar notzettel-id-regex "[0-9]\\\{14\\\}")

(defvar notzettel-link-prefix "[[")
(defvar notzettel-link-suffix "]]")


(defun notzettel-clean-string (string)
  "Remove extra whitespace and trim ends"
  (s-trim (s-collapse-whitespace string))
)

(defun notzettel-generate-id ()
  "Generate an ID using format time string and `notzettel-id-format`"
  (format-time-string notzettel-id-format)
  )

(defun notzettel--contains-id-p (str)
  "Return true is string contains id"
  (when (string-match notzettel-id-regex str))
  )

(defun notzettel--extract-id (str)
  "Return first match for id in string. If no matches return nil."
  (let ((extracted-id))
    (when (string-match notzettel-id-regex str)
      (setq extracted-id (substring str (match-beginning 0) (match-end 0)))
      extracted-id))
  )

(defun notzettel--add-link-affixes (str)
  "Add link prefix and link suffix to STR"
  (concat notzettel-link-prefix str notzettel-link-suffix)
  )

(defun notzettel--get-id-regex ()
  (concat "\\\(" notzettel-id-regex "\\\)")
  )

(defun notzettel--extract-title (str)
  "Requires id to be in the format =id= =title="
  (when (string-match ( concat "\\\(" notzettel-id-regex "\\\)" " +" "\\\(" notzettel-title-regex "\\\)") str)
    (match-string-no-properties 2 str)
    ))

(defun notzettel-current-file-id ()
  "Get id of current file"
    (notzettel--extract-id (notdeft-base-filename (notdeft-current-filename))))

(defun notzettel-current-base-filename ()
    "Current filename base"
  (file-name-base (notdeft-current-filename)))

(defun notzettel--replace-id-with-link (str)
  "replace id in string with a link"
  (when (string-match (concat "\\\(" notzettel-id-regex "\\\)") str)
    (replace-match (concat notzettel-link-prefix (match-string-no-properties 1 str) notzettel-link-suffix) nil nil str)
    ))

(defun notzettel-current-file-link ()
  "Get current file name as link. Enclose is affixes else return the whole name in affixes"

  (if (notzettel--contains-id-p (notzettel-current-base-filename))
      (notzettel--replace-id-with-link (notzettel-current-base-filename))
    (notzettel--add-link-affixes (notzettel-current-base-filename))
      ))

;Note creation

(defun notzettel--clean-string (string)
  "Remove extra whitespace and trim ends"
  (s-trim (s-collapse-whitespace string)))

(defun notzettel-generate-note-header (id &optional title parent-link)
  "Default function for generating note header"
  (insert (concat
	   "#+title:" id " " title "\n"
	   "#+date: " (format-time-string "%F %R") "\n"
	   "tags:" (read-string "tags: ") "\n"))
  (when (y-or-n-p "Add backlink?")
    (insert (concat "created_in: " parent-link)))
  (insert "\n\n"))

(defun notzettel-generate-note-footer (id &optional title parent-link)
  "Default function for generating note footer"
  nil
  )

(defvar notzettel-note-header-function 'notzettel-generate-note-header)
(defvar notzettel-note-footer-function 'notzettel-generate-note-footer)

(defun notzettel--generate-note (id title &optional content parent-link)
  "Create a note in the default notdeft directory"
  (let ((cleaned-title (notzettel-clean-string title)))

    (notdeft-find-file (concat (file-name-as-directory (notdeft-get-directory)) id " " cleaned-title "." notdeft-extension))

    (funcall notzettel-note-header-function id cleaned-title parent-link)
    (insert (concat content "\n"))
    (funcall notzettel-note-footer-function id cleaned-title parent-link)
    (save-buffer)
    (notzettel--extract-tags-from-file (notdeft-current-filename))
    ))

(defun notzettel-new-note-in-place (title)
  "Create new note from another file, with content if region was active leaving a link"
  (interactive "sTitle: ")
  (let ((id (notzettel-generate-id)) (text ""))
    (when (region-active-p)
      (setq text  (delete-and-extract-region (region-beginning) (region-end))))

    (insert (concat notzettel-link-prefix id notzettel-link-suffix " " title))

    (notzettel--generate-note id title text (notzettel--replace-id-with-link (notzettel-current-base-filename)))))

(defun notzettel-new-note (title)
  "Make new note without content or inserting link"
  (interactive "sTitle: ")
  (notzettel--generate-note (notzettel-generate-id) title nil nil))

;Links

(defun notzettel-link-at-point ()
  "Get the thing at point and if it matches the link regex "
  (let ((link-regex (concat (regexp-quote notzettel-link-prefix) "\\\(" notzettel-title-regex "\\\)" (regexp-quote notzettel-link-suffix))))
    (when (thing-at-point-looking-at link-regex)
      (match-string-no-properties 1)
      )
    ))

(defun notzettel-find-file-containing (id)
  "Search titles of all notdeft files and check for an exact match"
  (let (matched-file extracted-id)
    (when id
  (dolist (file (notdeft-find-all-files-in-dir (notdeft-get-directory) nil))
    (setq extracted-id (notzettel--extract-id file))

    (when (string-equal id extracted-id)
      (setq matched-file (file-name-base file))
      ))
  matched-file
  )))

(defun notzettel-follow-link-at-point ()
  "Get the link-text at point and if it matches the link regex follow ot"
  (interactive)
  (let (link-at-point link-target-file)
    (setq link-at-point (notzettel-link-at-point))
    (setq link-target-file (notzettel-find-file-containing (notzettel--extract-id link-at-point)))

    (when link-at-point
      (if link-target-file
	  (notdeft-find-file (concat (file-name-as-directory (notdeft-get-directory)) link-target-file "." notdeft-extension))
	(when (y-or-n-p "No Match. Search query instead?")
	  (notdeft-open-query link-at-point nil t)
	  )))))

(defun notzettel-query-thing-at-point ()
  "If "
  (interactive)
  (if (region-active-p)
      (notdeft-open-query (buffer-substring-no-properties (region-beginning) (region-end)) nil t)
      (notdeft-open-query (thing-at-point 'word 'no-properties) nil t)
      ))

;Insert link of file

(defvar notzettel-completing-read-function 'ivy-completing-read)

(defun notzettel-insert-link-to-note (file)
  ""
  (interactive
   (list (notzettel-completing-read-function "Insert link to note:" (notdeft-find-all-files-in-dir (notdeft-get-directory) nil))))
  (let ((id (notzettel-get-id-from-string file)))
    (when (string-match-p id file)
      (insert (notzettel--replace-id-with-link (file-name-base file)))
      )))

;Window layout


(defvar-local notzettel-pre-search-window-config nil)
(defvar-local notzettel-pre-view-window-config nil)
(defvar-local notzettel-preview-buffer nil)
;(defvar-local notzettel-pre-view-window-config nil)
(defvar-local notzettel-viewed-files-list nil)
(defvar-local notzettel-search-livep nil)
(defvar-local notzettel-line-goto nil)

(defconst notzettel-preview-buffer-name "*NotZettel Preview*")

(define-minor-mode notzettel-notdeft-mode
  "Get your foos in the right places."
  :lighter " nz"
  :keymap (let ((map (make-sparse-keymap)))
	    map)
  ;(notzettel-reset-search)
)

(defun notzettel-new-search ()
  ""
  (setq notzettel-pre-search-window-config (current-window-configuration))
  (setq notzettel-preview-buffer (get-buffer-create notzettel-preview-buffer-name))

)

(defun notzettel-clear-search ()
  ""

)

(defun notzettel-reset-search ()
  ""
  (setq notzettel-pre-search-window-config (current-window-configuration))

  )

(defun notzettel--split-window-if-necessary ()
  ""
  (when (eq 1 (length (window-list (window-frame))))
    (split-window-sensibly)))

(defcustom notzettel-notdeft-window-setup 'reorganize-frame
  ""
  :type '(choice
	  (const current-window)
	  (const reorganize-frame)
	  (const only-window)
	  (const other-window)))

(defun notzettel--setup-window ()
  "Setup the notdeft window buffer"

  (cond ((eq notzettel-notdeft-window-setup 'current-window)
	 )
	((eq notzettel-notdeft-window-setup 'reorganize-frame)
	 (let ((saved-buffer (current-buffer)))
	   (delete-other-windows)
	   (split-window-sensibly)
	   (set-window-buffer (selected-window) saved-buffer)
	   (select-window (next-window))))
	((eq notzettel-notdeft-window-setup 'only-window)
	 (delete-other-windows)
	 )
	((eq notzettel-notdeft-window-setup 'other-window)
	 (select-window (other-window))
	 )))

(defun notzettel-notdeft-start (&optional reset new)
  "Begin a new notzettel search session"
  (interactive)
  (notzettel-notdeft-mode)
  (let (win-config)
    (setq win-config (current-window-configuration))

    (notzettel--setup-window)
    (notdeft reset new)
    (print (current-buffer))
    (setq notzettel-pre-search-window-config win-config)

    (setq notzettel-preview-buffer (get-buffer-create notzettel-preview-buffer-name))
    (notzettel-follow-mode)))

(defun notzettel-notdeft-quit (prefix)
  "End the current notzettel search session"
  (interactive "P")
  (let (win-config)

    (setq win-config notzettel-pre-search-window-config)
    (print win-config)

    (while (< 0 (length notzettel-viewed-files-list))
    (let (buf))
    (setq buf (car notzettel-viewed-files-list))
    (kill-buffer buf)
    (setq notzettel-viewed-files-list (cdr notzettel-viewed-files-list))
    )

    (notdeft-quit prefix)

    (set-window-configuration win-config)
  ))

;Viewing
(defvar notzettel-min-letters-to-view 3)

(defun notzettel-file-path-at-point ()
  "Return the name of the file represented by the widget at the point.
Return nil if the point is not on a file widget or if not a valid title"
  (let ((file-under-cursor (widget-get (widget-at) :tag)))
    (when (stringp file-under-cursor)
	file-under-cursor
  )))

(defun notzettel--should-search-word (str)
  "If string is not empty and is a certain amount of chars return true else nil"
  (when (stringp str)
    (> (length (notzettel-clean-string str)) notzettel-min-letters-to-view)
    ))

(defun notzettel-copy-link-current-file-in-list ()
  "Copy link of current file in notdeft search buffer"
  (interactive)
  (let ((file (notzettel-file-path-at-point)))
    (kill-new (notzettel--replace-id-with-link (file)))))

(defun notzettel--highlight-string-in-buffer (string)
  "Highlight matches of a search term in buffer"
  (let ()
    (remove-overlays)
    (dolist (word (split-string string))
      (goto-char (point-max))
      (while (re-search-backward word nil t)
	(overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'isearch)
	))))

(defun notzettel-preview-file (file &optional highlight)
  "Open file in other window and highlight filter string. This should be used from the notdeft buffer"

  (let ((word notdeft-filter-string) (prev-buffer))
    (when (stringp file)

      (set-buffer (get-buffer-create notzettel-preview-buffer))
      (read-only-mode -1)
      (erase-buffer)
      (insert "***PREVIEW ONLY***\n\n")
      (insert-file-contents file nil)
      (visual-line-mode t)
      (read-only-mode 1)
      (display-buffer (buffer-name notzettel-preview-buffer) 'display-buffer-use-some-window)

      (when (notzettel--should-search-word word)
	(when highlight
	  (notzettel--highlight-string-in-buffer word)))
      )))


(defun notzettel-preview-highlight ()
  "In notdeft buffer show the current file in list in note-deft preview mode with highlighting"
  (notzettel-preview-file (notzettel-file-path-at-point) t))

(defun notzettel-preview ()
  ""
  (interactive)

  (notzettel-preview-file (notzettel-file-path-at-point))
  )

(defun notzettel-next-line-preview ()
  "Move to next line in notdeft buffer and preview file other window"
  (interactive)
  (next-line)
  (notzettel-preview-highlight))

(defun notzettel-previous-line-preview ()
  "Move to previous line in notdeft buffer and preview file other window"
  (interactive)
  (previous-line)
  (notzettel-preview-highlight))

;View

(defvar notzettel--view-window-config nil)

(defcustom notzettel-view-window-setup 'reorganize-frame
  ""
  :type '(choice
	  (const reorganize-frame)
	  (const current-window)
	  (const only-window)
	  (const other-window)))

(defun notzettel-find-file-in-list (&optional window-setup)
  ""
  (let ((file (notzettel-file-path-at-point)))
    (when file
      (cond
       ((eq window-setup 'current-window))
       ((eq window-setup 'only-window)
	(delete-other-windows))
       ((eq window-setup 'other-window)
	(notzettel--split-window-if-necessary)
	(select-window (next-window)))
       ((eq window-setup 'reorganize-frame)
	(delete-other-windows)
	(split-window-sensibly)))
       (find-file file)
       (notzettel-view-mode t)
     )))

(defun notzettel-view-file ()
  ""
  (interactive)

  (setq notzettel-pre-view-window-config (current-window-configuration))
  (setq notzettel-line-goto (line-number-at-pos))

  (let (notdeft-buf win-config view-buf)
    (setq win-config (current-window-configuration))
    (setq notdeft-buf (current-buffer))
    (notzettel-find-file-in-list notzettel-view-window-setup)
    (setq view-buf (current-buffer))
    (setq notzettel-pre-view-window-config win-config)
    (save-excursion
      (set-buffer notdeft-buf)
      (setq notzettel-viewed-files-list (cons view-buf notzettel-viewed-files-list)))))

(defun notzettel-quit-view (&optional restore-windows)
  "Whilst still in a search stop viewing/editing the current file"
  (interactive)
  (set-window-configuration notzettel-pre-view-window-config)
  (goto-line notzettel-line-goto)
  )

(defun notzettel-view-toggle-editable ()
  ""
  (interactive)
  (cond ((eq notzettel-view-only t)
	 (view-mode 1)
	  (setq notzettel-view-only nil))
	((eq notzettel-view-only nil)
	 (view-mode -1)
	  (setq notzettel-view-only t))))


(defun notzettel-select-file ()
  "Select the file in list and end the notzettel search"
  (interactive)
  (notzettel-find-file-in-list 'current-window 1))

;make tag list
(defvar notzettel-tag-regex "[@|#][A-Za-z0-9_]+")
(defvar notzettel-tag-list '())

(defun notzettel--extract-tags-from-file (file)
  (with-temp-buffer
      (progn
      (goto-char (point-min))
      (insert-file-contents file)
      (while (re-search-forward notzettel-tag-regex nil t)
	(let ((matched-tag (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
	(unless (member matched-tag notzettel-tag-list)
	  (push matched-tag notzettel-tag-list)
	  ))))))

(defun notzettel--extract-tags-from-all-files ()
  "Extract tags matching `notzettel-tag-regex` from notdeft dir"
  (dolist (file (notdeft-find-all-files-in-dir (notdeft-get-directory) t))
    (notzettel--extract-tags-from-file file)))

(defun notzettel-insert-tag-from-list (tag)
  "Insert tag from list"
  (interactive
   (list (ivy-completing-read "Tag:" notzettel-tag-list)))
  (insert tag))

; bindings

(define-minor-mode notzettel-follow-mode
  "Get your foos in the right places."
  :lighter " nz-follow"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map [remap next-line] 'notzettel-next-line-preview)
	    (define-key map [remap previous-line] 'notzettel-previous-line-preview)
	    map)
  (cond ((eq notzettel-follow-mode t)
	 (when (not (buffer-live-p notzettel-preview-buffer))
	   (setq notzettel-preview-buffer (get-buffer-create notzettel-preview-buffer-name))
	   )
	 (notzettel-preview)
	 )
	((eq notzettel-follow-mode nil)
	 (kill-buffer notzettel-preview-buffer)
	 ))
  )

(define-minor-mode notzettel-view-mode
  "Get your foos in the right places."
  :lighter " nz-view"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c C-q") 'notzettel-quit-view)
	    (define-key view-mode-map [remap View-exit] 'notzettel-quit-view)
	    (define-key view-mode-map [remap View-quit] 'notzettel-quit-view)
	    (define-key view-mode-map (kbd "<tab>") 'notzettel-quit-view)
	    map)

  (make-local-variable 'notzettel-view-only)
  (make-local-variable 'notzettel-pre-view-window-config)
  (setq notzettel-view-only t)
  (notzettel-view-toggle-editable)
  (notdeft-note-mode)
  )

(define-key notzettel-view-mode-map (kbd "C-c C-v") 'notzettel-view-toggle-editable)
(define-key notzettel-view-mode-map [remap read-only-mode] 'notzettel-view-toggle-editable)
(define-key notzettel-view-mode-map (kbd "s-<tab>") 'notzettel-view-toggle-editable)

(define-key notdeft-mode-map (kbd "C-c C-f") 'notzettel-follow-mode)
(define-key notdeft-mode-map (kbd "<tab>") 'notzettel-view-file)
(define-key notdeft-mode-map [remap notdeft-quit] 'notzettel-notdeft-quit)

(defalias 'nz 'notzettel-notdeft-start)
