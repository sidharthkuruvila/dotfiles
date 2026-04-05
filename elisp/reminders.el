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

(provide 'reminders)
