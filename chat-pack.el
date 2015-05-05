;;; chat-pack.el --- jabber setup

;;; Commentary:

;;; Code:

(require 'install-packages-pack)
(install-packages-pack/install-packs '(jabber creds))

;; ===================== lib deps

(require 'netrc)

;; ===================== setup file

(defvar *CHAT-PACK-CREDENTIALS-FILE* "~/.authinfo.gpg" "Default credentials file.")

;; ===================== setup functions

(defun chat-pack/log (str)
  "Log the message STR."
  (message "chat-pack - %s" str))

(defun chat-pack/setup-possible-p (creds-file)
  "Check if the setup is possible by checking the existence of the file CREDS-FILE and that the entry 'jabber' exists."
  (let ((parsed-file (netrc-parse creds-file)))
    (and parsed-file ;; nil if the file does not exist
         (netrc-machine parsed-file "jabber"))))

(defun chat-pack/setup (creds-file)
  "Chat-pack setup from the CREDS-FILE."
  (let* ((creds-file-content (creds/read-lines creds-file))
         (jabber-description (creds/get creds-file-content "jabber"))
         (login              (creds/get-entry jabber-description "login"))
         (password           (creds/get-entry jabber-description "password"))
         (server             (creds/get-entry jabber-description "server")))
    ;; Jabber client configuration
    (setq jabber-account-list
          `((,login
             (:password . ,password)
             (:nickname . ,login)
             (:network-server . ,server))))

    (setq jabber-vcard-avatars-retrieve nil
          jabber-chat-buffer-show-avatar nil)))

;; ===================== setup routine

(defun chat-pack/load-pack! ()
  (interactive)
  "(Re)load the chat-pack."
  (if (chat-pack/setup-possible-p *CHAT-PACK-CREDENTIALS-FILE*)
      (progn (chat-pack/log (concat *CHAT-PACK-CREDENTIALS-FILE* " found! Running setup..."))
             (chat-pack/setup *CHAT-PACK-CREDENTIALS-FILE*)
             (chat-pack/log "Setup done!"))
    (chat-pack/log (concat "You need to setup the credentials file " *CHAT-PACK-CREDENTIALS-FILE* " for this to work.\n"
                           "Here is the needed content to setup to your need into '" *CHAT-PACK-CREDENTIALS-FILE* "':\n"
                           "machine jabber login <your-gmail-login> password <your-gmail-password>"))))

(provide 'chat-pack)
;;; chat-pack.el ends here
