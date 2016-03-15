;;; chat-pack.el --- jabber setup

;;; Commentary:

;;; Code:

(use-package jabber)
(use-package creds)
(use-package jabber-otr
  :load-path "./jabber-otr.el"
  :ensure nil)

;; ===================== lib deps

(use-package netrc)

;; ===================== setup file

(defvar chat-pack--credentials-file "~/.authinfo.gpg" "Default credentials file.")

;; ===================== setup functions

(defun chat-pack--log (&rest str)
  "Log the message STR."
  (apply 'message (format "chat-pack - %s" (car str)) (cdr str)))

(defun chat-pack/setup-possible-p (creds-file)
  "Check if the setup is possible through the file CREDS-FILE's existence.
But also that the entry 'jabber' exists."
  (let ((parsed-file (netrc-parse creds-file)))
    (and parsed-file ;; nil if the file does not exist
         (netrc-machine parsed-file "jabber"))))

(defun chat-pack--setup (creds-file)
  "Chat-pack setup from the CREDS-FILE."
  (let* ((creds-file-content (creds/read-lines creds-file))
         (jabber-description (creds/get creds-file-content "jabber"))
         (login              (creds/get-entry jabber-description "login"))
         (password           (creds/get-entry jabber-description "password"))
         (server             (creds/get-entry jabber-description "server"))
         (connection-type    (creds/get-entry jabber-description "connection-type"))
         (connection-port    (creds/get-entry jabber-description "connection-port"))
         (jabber-server      (creds/get-entry jabber-description "jabber-server")))
    (custom-set-variables '(jabber-account-list
                            `((,login
                               (:password . ,password)
                               (:nickname . ,login)
                               (:network-server . ,server)
                               (:connection-type . ,(intern connection-type))
                               (:port . ,(string-to-int connection-port)))))
                          '(jabber-vcard-avatars-retrieve nil)
                          '(jabber-chat-buffer-show-avatar nil))))

(defun chat-pack-connect-or-switch-to-buffer ()
  "Start the chat."
  (interactive)
  (let* ((buffer (get-buffer jabber-roster-buffer)))
    (if (and buffer jabber-connections)
        (jabber-switch-to-roster-buffer)
      (call-interactively #'jabber-connect)
      (jabber-switch-to-roster-buffer))))

(defalias 'chat-pack-disconnect 'jabber-disconnect)

;; ===================== setup routine

(defun chat-pack-load ()
  "Load or reload the pack.
This can be used if the .authinfo file has been updated."
  (interactive)
  (if (chat-pack/setup-possible-p chat-pack--credentials-file)
      (progn (chat-pack--log (concat chat-pack--credentials-file " found! Running setup..."))
             (chat-pack--setup chat-pack--credentials-file)
             (chat-pack--log "Setup done!"))
    (chat-pack--log "You need to setup the credentials file %s for this to work
Here is the needed content to setup to your need into '%s':
machine jabber login <your-login> password <your-password> server <server> connection-type <connection-type> connection-port <connection-port>"
                    chat-pack--credentials-file
                    chat-pack--credentials-file)))

(defvar chat-pack-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c l") 'chat-pack-load)
    (define-key map (kbd "C-c c c") 'chat-pack-connect-or-switch-to-buffer)
    (define-key map (kbd "C-c c d") 'chat-pack-disconnect)
    (define-key map (kbd "C-c c o") 'jabber-otr-encrypt)
    (define-key map (kbd "C-c c w") 'jabber-chat-with)
    map)
  "Keymap for git-pack mode.")

(define-minor-mode chat-pack-mode
  "Minor mode to consolidate chat-pack extensions.

\\{chat-pack-mode-map}"
  :lighter " CP"
  :keymap chat-pack-mode-map)

(define-globalized-minor-mode global-chat-pack-mode chat-pack-mode chat-pack-on)

(defun chat-pack-on ()
  "Turn on `chat-pack-mode'."
  (chat-pack-mode +1))

(global-chat-pack-mode)

(provide 'chat-pack)
;;; chat-pack.el ends here
