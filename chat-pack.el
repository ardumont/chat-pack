;;; chat-pack.el --- jabber setup

;;; Commentary:

;;; Code:

(require 'install-packages-pack)
(install-packs '(jabber))

;; ===================== lib deps

(require 'netrc)

;; ===================== setup file

(setq *CHAT-PACK-CREDENTIALS-FILE* "~/.authinfo")

;; ===================== setup functions

(defun chat-pack/log (str) "A log function for the pack."
  (message "chat-pack - %s" str))

(defun chat-pack/setup-possible-p (creds-file)
  "Check if the setup is possible by checking the existence of the file and that the entry 'jabber' exists."
  (let ((parsed-file (netrc-parse creds-file)))
    (and parsed-file ;; nil if the file does not exist
         (netrc-machine parsed-file "jabber"))))

(defun chat-pack/setup (creds-file)
  ;; load the entry jabber in the ~/.netrc, we obtain a hash-map with the needed data
  (setq cred (netrc-machine (netrc-parse creds-file) "jabber" t))

  ;; Jabber client configuration
  (setq jabber-account-list
        `((,(netrc-get cred "login")
           (:password . ,(netrc-get cred "password"))
           (:nickname . ,(netrc-get cred "login"))
           (:network-server . "talk.google.com")
           (:connection-type . ssl)
           (:port . 5223))))

  (setq jabber-vcard-avatars-retrieve nil
        jabber-chat-buffer-show-avatar nil))

;; ===================== setup routine

(if (chat-pack/setup-possible-p *CHAT-PACK-CREDENTIALS-FILE*)
    (progn (chat-pack/log (concat *CHAT-PACK-CREDENTIALS-FILE* " found! Running setup..."))
           (chat-pack/setup *CHAT-PACK-CREDENTIALS-FILE*)
           (chat-pack/log "Setup done!"))
  (chat-pack/log (concat "You need to setup the credentials file " *CHAT-PACK-CREDENTIALS-FILE* " for this to work.\n"
                   "Here is the needed content to setup to your need into '" *CHAT-PACK-CREDENTIALS-FILE* "':\n"
                   "machine jabber login <your-gmail-login> password <your-gmail-password>")))

(provide 'chat-pack)
;;; chat-pack.el ends here
