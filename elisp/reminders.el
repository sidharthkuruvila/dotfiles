;;; reminders.el --- Apple Reminders integration -*- lexical-binding: t -*-

;; Reads incomplete reminders from the "Emacs Inbox" list in Apple Reminders
;; and routes them to the appropriate Notes2 file based on the :TAG: property
;; in each reminder's org notes field:
;;
;;   #journal → ~/Documents/Notes2/journal.org  (under the :CREATED: date heading)
;;   other    → ~/Documents/Notes2/inbox.org    (appended to end of file)
;;
;; journal.org structure produced:
;;
;;   * YYYY-MM-DD                             ← date from :CREATED:, newest first
;;   ** [HH:MM] Title of reminder             ← time from :CREATED:, level shifted
;;      :PROPERTIES:
;;      :CREATED:     YYYY-MM-DD HH:MM
;;      :REMINDER_ID: <id>
;;      :TAG:         #journal
;;      :END:
;;
;;      Body text verbatim from reminder notes.
;;
;; When a batch spans multiple dates, entries are grouped by :CREATED: date
;; and each group is inserted under its own date heading. Headings are created
;; in ascending date order so the newest heading ends up at the top of the file.
;;
;; Requires reminders-cli: brew install keith/formulae/reminders-cli

(defun my/reminders--tag (notes)
  "Return the :TAG: value from NOTES org text, or nil if absent."
  (when (string-match ":TAG: *\\(#[^\n]+\\)" notes)
    (string-trim (match-string 1 notes))))

(defun my/reminders--created-date (notes)
  "Return YYYY-MM-DD string from the :CREATED: property in NOTES, or nil."
  (when (string-match ":CREATED: \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" notes)
    (match-string 1 notes)))

(defun my/reminders--created-time (notes)
  "Return HH:MM string from the :CREATED: property in NOTES, or nil."
  (when (string-match ":CREATED: [0-9-]+ \\([0-9]+:[0-9]+\\)" notes)
    (match-string 1 notes)))

(defun my/reminders--inject-id (notes id)
  "Inject :REMINDER_ID: ID before :END: in the first PROPERTIES drawer of NOTES."
  (replace-regexp-in-string
   ":END:"
   (format ":REMINDER_ID: %s\n:END:" id)
   notes t t))

(defun my/reminders--shift-headings (notes time)
  "Shift org heading levels in NOTES up by one (** → ***, * → **).
If TIME (a HH:MM string) is non-nil, prepend [TIME] to each new ** heading."
  (with-temp-buffer
    (insert notes)
    ;; Shift deeper levels first to avoid double-shifting
    (goto-char (point-min))
    (while (re-search-forward "^\\*\\* " nil t)
      (replace-match "*** "))
    (goto-char (point-min))
    (while (re-search-forward "^\\* " nil t)
      (replace-match (if time (format "** [%s] " time) "** ")))
    (buffer-string)))

(defun my/reminders--insert-journal (journal-file date entries)
  "Insert ENTRIES under the * DATE heading in JOURNAL-FILE.
DATE is a YYYY-MM-DD string. Creates the heading at the top of the file
(after #+keywords) if it does not already exist. ENTRIES are formatted
org strings at ** level, separated by blank lines in the output."
  (let* ((date-re (format "^\\* %s\\b" (regexp-quote date)))
         ;; Join entries with blank lines, preserving content
         (block (concat "\n"
                        (mapconcat (lambda (e) (string-trim-right e "\n"))
                                   entries
                                   "\n\n")
                        "\n")))
    (with-current-buffer (find-file-noselect journal-file)
      (save-excursion
        (widen)
        (goto-char (point-min))
        (if (re-search-forward date-re nil t)
            ;; Found — append at end of this date's section
            (progn
              (end-of-line)
              (if (re-search-forward "^\\* " nil t)
                  (goto-char (match-beginning 0))
                (goto-char (point-max)))
              (insert block))
          ;; Not found — create heading at top, after #+keywords (newest first)
          (goto-char (point-min))
          (while (and (not (eobp)) (looking-at "#\\+"))
            (forward-line 1))
          (insert (format "\n* %s%s" date block))))
      (save-buffer))))

(defun my/journal-capture-position ()
  "Move point to the insertion position for a new journal entry.
Used as the target function for org-capture journal templates.
Finds or creates today's * YYYY-MM-DD heading, positioning point
at the end of that section so the new ** entry is appended."
  (let* ((date    (format-time-string "%Y-%m-%d"))
         (date-re (format "^\\* %s\\b" (regexp-quote date))))
    (widen)
    (goto-char (point-min))
    (if (re-search-forward date-re nil t)
        ;; Found — position at end of this date's section
        (progn
          (end-of-line)
          (if (re-search-forward "^\\* " nil t)
              (goto-char (match-beginning 0))
            (goto-char (point-max))))
      ;; Not found — create heading at top, after #+keywords (newest first)
      (goto-char (point-min))
      (while (and (not (eobp)) (looking-at "#\\+"))
        (forward-line 1))
      (insert (format "\n* %s\n" date))
      (forward-line -1)
      (end-of-line))))

(defun my/process-reminders ()
  "Fetch incomplete reminders from the \"Emacs Inbox\" Reminders list.
Routes #journal-tagged reminders to journal.org (grouped by :CREATED: date)
and all others to inbox.org. Marks every reminder complete after processing."
  (interactive)
  (let* ((inbox-file   (expand-file-name "~/Documents/Notes2/inbox.org"))
         (journal-file (expand-file-name "~/Documents/Notes2/journal.org"))
         (raw (shell-command-to-string "reminders show --format json \"Emacs Inbox\""))
         (reminders (condition-case err
                        (json-parse-string raw :array-type 'list :object-type 'alist)
                      (error
                       (message "my/process-reminders: failed to parse reminders-cli output: %s" err)
                       nil)))
         (count (length reminders))
         ;; journal-by-date: alist of (date-string . (list of entry strings))
         journal-by-date
         inbox-entries)
    (if (zerop count)
        (message "my/process-reminders: no reminders to process.")
      ;; Partition and transform reminders
      (dolist (reminder reminders)
        (let* ((notes (alist-get 'notes reminder))
               (id    (alist-get 'externalId reminder)))
          (when (and notes (not (string-empty-p notes)))
            (let* ((tag     (my/reminders--tag notes))
                   (date    (or (my/reminders--created-date notes)
                                (format-time-string "%Y-%m-%d")))
                   (time    (my/reminders--created-time notes))
                   (content (my/reminders--inject-id notes id)))
              (if (equal tag "#journal")
                  ;; Group by :CREATED: date
                  (let ((existing (assoc date journal-by-date)))
                    (if existing
                        (setcdr existing (append (cdr existing)
                                                 (list (my/reminders--shift-headings content time))))
                      (push (cons date (list (my/reminders--shift-headings content time)))
                            journal-by-date)))
                (push content inbox-entries))))))
      ;; Write inbox entries (append to file)
      (when inbox-entries
        (with-temp-buffer
          (insert (format "\n# Imported %s\n" (format-time-string "%Y-%m-%d %H:%M")))
          (dolist (entry (nreverse inbox-entries))
            (insert entry)
            (unless (string-suffix-p "\n" entry)
              (insert "\n")))
          (append-to-file (point-min) (point-max) inbox-file))
        (let ((buf (get-file-buffer inbox-file)))
          (when buf
            (with-current-buffer buf
              (revert-buffer t t t)))))
      ;; Write journal entries — process dates in ascending order so the
      ;; newest date ends up at the top of journal.org
      (when journal-by-date
        (let ((sorted-dates (sort (mapcar #'car journal-by-date) #'string<)))
          (dolist (date sorted-dates)
            (my/reminders--insert-journal
             journal-file date (cdr (assoc date journal-by-date))))))
      ;; Mark all reminders complete
      (dolist (reminder reminders)
        (shell-command (format "reminders complete \"Emacs Inbox\" %s"
                               (alist-get 'externalId reminder))))
      ;; Open the file that received entries
      (find-file (if journal-by-date journal-file inbox-file))
      (goto-char (point-min))
      (message "my/process-reminders: processed %d reminder(s) (%d journal, %d inbox)."
               count
               (apply #'+ (mapcar (lambda (d) (length (cdr d))) journal-by-date))
               (length inbox-entries)))))

;;; Stack ↔ Reminders sync ────────────────────────────────────────────────────
;;
;; Bidirectional sync between stack.org and the "Tasks" Apple Reminders list.
;;
;; Sync rules:
;;   New stack.org entry (no :REMINDERS_ID:) → push to Reminders, store ID.
;;   New Reminders task (no matching entry)   → import to bottom of stack.org.
;;   Title mismatch                           → stack.org title wins.
;;   :STATUS: archived                        → Denote file + delete from both.
;;   :STATUS: deleted                         → delete from both, no archive.
;;   Task gone from Reminders                 → prompt: archive or delete.
;;
;; Only the task title is synced. Org content beneath the heading in stack.org
;; is local only and is never sent to Reminders.

(defvar my/stack-reminders-list "Tasks"
  "Name of the Apple Reminders list used for stack.org sync.")

(defvar my/stack-archive-dir
  (expand-file-name "~/Documents/Notes2/archive/")
  "Directory where archived stack entries are saved as Denote files.")

(defvar my/stack-file
  (expand-file-name "~/Documents/Notes2/stack.org")
  "Path to stack.org.")

;; ---- CLI helpers ------------------------------------------------------------

(defun my/stack--fetch-reminders ()
  "Return parsed list of incomplete tasks in `my/stack-reminders-list'."
  (let ((raw (shell-command-to-string
              (format "reminders show --format json %S" my/stack-reminders-list))))
    (condition-case err
        (json-parse-string raw :array-type 'list :object-type 'alist)
      (error
       (message "my/stack--fetch-reminders: parse error: %s" err)
       nil))))

(defun my/stack--create-reminder (title)
  "Create a task with TITLE in `my/stack-reminders-list'.
Returns the externalId of the new task, or nil on failure."
  (let* ((raw (shell-command-to-string
               (format "reminders add %S %S --format json"
                       my/stack-reminders-list title)))
         (result (condition-case err
                     (json-parse-string raw :object-type 'alist)
                   (error
                    (message "my/stack--create-reminder: parse error: %s" err)
                    nil))))
    (alist-get 'externalId result)))

(defun my/stack--delete-reminder (id)
  "Delete the task with ID from `my/stack-reminders-list'."
  (shell-command (format "reminders delete %S %s" my/stack-reminders-list id)))

(defun my/stack--rename-reminder (id new-title)
  "Rename the task with ID to NEW-TITLE in `my/stack-reminders-list'."
  (shell-command (format "reminders edit %S %s %S"
                         my/stack-reminders-list id new-title)))

;; ---- stack.org entry helpers ------------------------------------------------

(defun my/stack--get-entries ()
  "Return all top-level entries in `my/stack-file' as a list of plists.
Each plist has keys: :title :id :status :begin :end."
  (with-current-buffer (find-file-noselect my/stack-file)
    (save-excursion
      (widen)
      (goto-char (point-min))
      (let (entries)
        (while (re-search-forward "^\\* " nil t)
          (let* ((begin  (match-beginning 0))
                 (title  (substring-no-properties (org-get-heading t t t t)))
                 (id     (org-entry-get (point) "REMINDERS_ID"))
                 (status (org-entry-get (point) "STATUS"))
                 (end    (save-excursion
                           (org-end-of-subtree t)
                           (point))))
            (push (list :title title :id id :status status
                        :begin begin :end end)
                  entries)))
        (nreverse entries)))))

(defun my/stack--entry-content (entry)
  "Return the full org text of ENTRY from `my/stack-file'."
  (with-current-buffer (find-file-noselect my/stack-file)
    (buffer-substring-no-properties
     (plist-get entry :begin)
     (plist-get entry :end))))

(defun my/stack--add-entry (title id)
  "Append a new entry with TITLE and :REMINDERS_ID: ID to `my/stack-file'."
  (with-current-buffer (find-file-noselect my/stack-file)
    (save-excursion
      (widen)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* %s\n:PROPERTIES:\n:CREATED: %s\n:REMINDERS_ID: %s\n:END:\n"
                      title
                      (format-time-string "%Y-%m-%d %H:%M")
                      id))
      (save-buffer))))

(defun my/stack--set-property (entry prop value)
  "Set property PROP to VALUE on the org heading at ENTRY's position."
  (with-current-buffer (find-file-noselect my/stack-file)
    (save-excursion
      (widen)
      (goto-char (plist-get entry :begin))
      (org-entry-put (point) prop value)
      (save-buffer))))

(defun my/stack--rename-heading (entry new-title)
  "Replace the heading text of ENTRY with NEW-TITLE in `my/stack-file'."
  (with-current-buffer (find-file-noselect my/stack-file)
    (save-excursion
      (widen)
      (goto-char (plist-get entry :begin))
      (re-search-forward "^\\*+ " (line-end-position) t)
      (delete-region (point) (line-end-position))
      (insert new-title)
      (save-buffer))))

(defun my/stack--delete-region (begin end)
  "Delete the region from BEGIN to END in `my/stack-file' and save."
  (with-current-buffer (find-file-noselect my/stack-file)
    (widen)
    (delete-region begin end)
    (save-buffer)))

;; ---- Denote archive ---------------------------------------------------------

(defun my/stack--archive-to-denote (entry)
  "Write ENTRY's content to a new Denote file in `my/stack-archive-dir'."
  (let* ((title   (plist-get entry :title))
         (id      (format-time-string "%Y%m%dT%H%M%S"))
         (slug    (replace-regexp-in-string
                   "^-+\\|-+$" ""
                   (downcase (replace-regexp-in-string "[^a-z0-9]+" "-" title))))
         (fname   (format "%s--%s__task.org" id slug))
         (fpath   (expand-file-name fname my/stack-archive-dir))
         (content (my/stack--entry-content entry))
         ;; Strip the top-level heading line — title goes in #+TITLE
         (body    (replace-regexp-in-string "^\\* [^\n]*\n?" "" content)))
    (with-temp-file fpath
      (insert (format "#+TITLE:    %s\n#+DATE:     %s\n#+FILETAGS: :task:\n\n"
                      title (format-time-string "%Y-%m-%dT%H:%M:%S")))
      (insert body))))

;; ---- Combined actions -------------------------------------------------------

(defun my/stack--do-archive (entry)
  "Archive ENTRY: save to Denote, delete its Reminder, remove from stack.org."
  (my/stack--archive-to-denote entry)
  (when (plist-get entry :id)
    (my/stack--delete-reminder (plist-get entry :id)))
  (my/stack--delete-region (plist-get entry :begin) (plist-get entry :end)))

(defun my/stack--do-delete (entry)
  "Delete ENTRY from Reminders and stack.org, no archive."
  (when (plist-get entry :id)
    (my/stack--delete-reminder (plist-get entry :id)))
  (my/stack--delete-region (plist-get entry :begin) (plist-get entry :end)))

;; ---- Main sync function -----------------------------------------------------

(defun my/sync-stack ()
  "Bidirectionally sync stack.org with the Tasks Apple Reminders list."
  (interactive)
  (let* ((entries      (my/stack--get-entries))
         (reminders    (my/stack--fetch-reminders))
         (reminder-ids (mapcar (lambda (r) (alist-get 'externalId r)) reminders))
         to-archive
         to-delete)

    ;; Pass 1: process each stack.org entry
    (dolist (entry entries)
      (let ((id     (plist-get entry :id))
            (title  (plist-get entry :title))
            (status (plist-get entry :status)))
        (cond
         ;; Collect status:archived/deleted — applied bottom-up in Pass 3
         ((equal status "archived") (push entry to-archive))
         ((equal status "deleted")  (push entry to-delete))
         ;; No ID — new local entry, push to Reminders
         ((null id)
          (let ((new-id (my/stack--create-reminder title)))
            (when new-id
              (my/stack--set-property entry "REMINDERS_ID" new-id))))
         ;; Has ID — check if still in Reminders
         (t
          (if (not (member id reminder-ids))
              ;; Gone from Reminders (completed or deleted there)
              (let ((choice (completing-read
                             (format "Task \"%s\" is gone from Reminders. Archive or delete? "
                                     title)
                             '("archive" "delete") nil t)))
                (if (equal choice "archive")
                    (push entry to-archive)
                  (push entry to-delete)))
            ;; Still present — sync title, stack.org wins
            (let* ((match (seq-find (lambda (r)
                                      (equal (alist-get 'externalId r) id))
                                    reminders))
                   (r-title (alist-get 'title match)))
              (unless (equal r-title title)
                (my/stack--rename-reminder id title))))))))

    ;; Pass 2: import new Reminders tasks not yet in stack.org
    (let ((known-ids (remove nil (mapcar (lambda (e) (plist-get e :id))
                                         (my/stack--get-entries)))))
      (dolist (reminder reminders)
        (let ((rid    (alist-get 'externalId reminder))
              (rtitle (alist-get 'title reminder)))
          (unless (member rid known-ids)
            (my/stack--add-entry rtitle rid)))))

    ;; Pass 3: apply archive/delete bottom-up (highest :begin first)
    ;; to keep buffer positions valid across deletions
    (let ((ordered (sort (append to-archive to-delete)
                         (lambda (a b) (> (plist-get a :begin)
                                          (plist-get b :begin))))))
      (dolist (entry ordered)
        (if (member entry to-archive)
            (my/stack--do-archive entry)
          (my/stack--do-delete entry)))))

  (find-file my/stack-file)
  (message "my/sync-stack: sync complete."))

;; ---- Interactive tagging ----------------------------------------------------

(defun my/stack-tag-entry ()
  "Tag the stack.org entry at point as archived or deleted.
Sets the :STATUS: property on the current org heading.
Run `my/sync-stack' afterwards to apply the action."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (let ((status (completing-read "Set status: " '("archived" "deleted") nil t)))
    (org-entry-put (point) "STATUS" status)
    (message "Entry tagged as %s. Run C-c s to sync." status)))

(provide 'reminders)
