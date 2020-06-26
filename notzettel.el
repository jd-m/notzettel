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

 (defun notzettel-quick-note-function ()
    "" )

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
      )
    ))

;Window layout
(defvar notzettel-active-search nil
  "Search has an associated notdeft buffer, currently viewed file, preview buffer, wnidow config that was saved on beginning of search" )

(defun notzettel-get-create-search ()
  ""
  '((search-buffer .  notdeft-buffer)
    (window-config-to-restore)
    (preview-buffer .  "*Notzettel View*")
    (viewed-files . '())))

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

(defun notzettel--setup-notdeft-buffer (&optional reset new)
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
	 ))
  (notdeft reset new)
  )

(defun notzettel-notdeft-start ()
  "Begin a new notzettel search session"
  (interactive)
  (setq notzettel-active-search (notzettel-get-create-search))
  (setcdr (assq 'window-config-to-restore notzettel-active-search) (current-window-configuration))
  (notzettel--setup-notdeft-buffer)
  (setcdr (assq 'search-buffer notzettel-active-search) (current-buffer))

  )

(defun notzettel-notdeft-quit (prefix)
  "End the current notzettel search session"
  (interactive "P")
  (set-buffer  (cdr (assq 'search-buffer notzettel-active-search)))
  (notdeft-quit prefix)
  (dolist  (viewed-file (cdr (assq 'viewed-files notzettel-active-search)))
    (print "ya")
    )
  (set-window-configuration (cdr (assq 'window-config-to-restore notzettel-active-search)))
  (setq notzettel-active-search nil)
  )

;Viewing
(define-minor-mode notzettel-view-mode
  "Minor mode for viewing notdeft files"
  :lighter "notzettel"

  (if notzettel-view-mode
      (progn
	(view-mode t)
	(notdeft-note-mode t))
    (progn
      (view-mode nil)
      )))

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

(defun notzettel-highlight-file-other-window (file)
  "Open file in other window and highlight filter string. This should be used from the notdeft buffer"
  (let ((word notdeft-filter-string) (prev-buffer))

    (when (stringp file)
      (when (notzettel--should-search-word word)

	(notzettel--split-window-if-necessary)

	(select-window (next-window))
	(set-buffer (get-buffer-create "*Notzettel View*"))
	(set-window-buffer (selected-window) "*Notzettel View*")
	(erase-buffer)
	(insert-file-contents file nil)
	(visual-line-mode t)
	(notzettel--highlight-string-in-buffer word)
	(select-window (previous-window))
	))))

(defun notzettel-highlight-file-in-list-other-window ()
  "In notdeft buffer show the current file in list in note-deft preview mode with highlighting"
  (notzettel-highlight-file-other-window (notzettel-file-path-at-point)))

(defun notzettel-next-line-preview ()
  "Move to next line in notdeft buffer and preview file other window"
  (interactive)
  (next-line)
  (notzettel-highlight-file-in-list-other-window))

(defun notzettel-previous-line-preview ()
  "Move to previous line in notdeft buffer and preview file other window"
  (interactive)
  (previous-line)
  (notzettel-highlight-file-in-list-other-window))

(define-key notdeft-mode-map (kbd "C-n") 'notzettel-next-line-preview)
(define-key notdeft-mode-map (kbd "C-p") 'notzettel-previous-line-preview)

;View

(defvar notzettel--view-window-config nil)

(defcustom notzettel-view-window-setup 'reorganize-frame
  ""
  :type '(choice
	  (const reorganize-frame)
	  (const current-window)
	  (const only-window)
	  (const other-window)))

(defcustom notzettel-edit-window-setup 'current-window
  ""
  :type '(choice
	  (const reorganize-frame)
	  (const current-window)
	  (const only-window)
	  (const other-window)))

(defun notzettel-find-file-in-list (&optional window-setup editable)
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
       (view-mode editable)
       (notdeft-note-mode 1)

     )))

(defun notzettel-view-file ()
  ""
  (interactive)
  (notzettel-find-file-in-list notzettel-view-window-setup t)
  (push (current-buffer) (cdr (assq 'viewed-files notzettel-active-search)))
  )

(defun notzettel-edit-file ()
  ""
  (interactive)
  (notzettel-find-file-in-list notzettel-edit-window-setup 0)
  (push (current-buffer) (cdr (assq 'viewed-files notzettel-active-search)))
  ;no
  )

(defun notzettel-quit-view (&optional restore-windows)
  "Whilst still in a search stop viewing/editing the current file"
  (interactive)
  (let (buf)
    (setq buf (current-buffer))
    (print buf)
    (delq buf (cdr (assq 'viewed-files notzettel-active-search)))
    (buf)
    )
  )

(defun notzettel-select-file ()
  "Select the file in list and end the notzettel search"
  (interactive)
  (notzettel-find-file-in-list 'current-window 1)
  ()
)
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
    (notzettel--extract-tags-from-file file)
    ))

(defun notzettel-insert-tag-from-list (tag)
  "Insert tag from list"
  (interactive
   (list (ivy-completing-read "Tag:" notzettel-tag-list)))
  (insert tag))
