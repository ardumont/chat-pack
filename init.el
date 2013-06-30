(install-packs '(jabber))

;; ===================== lib deps

(require 'netrc)

;; ===================== setup file

(setq chat-pack--credentials-file "~/.authinfo")

;; ===================== setup functions

(defun chat-pack--setup-possible-p (creds-file)
  "Check if the setup is possible by checking the existence of the file and that the entry 'jabber' exists."
  (let ((parsed-file (netrc-parse creds-file)))
    (and parsed-file ;; nil if the file does not exist
         (netrc-machine parsed-file "jabber"))))

(defun chat-pack--setup (creds-file)
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

(if (chat-pack--setup-possible-p chat-pack--credentials-file)
    (progn (message (concat chat-pack--credentials-file " found! Running setup..."))
           (chat-pack--setup chat-pack--credentials-file)
           (message "Setup done!"))
  (message (concat "You need to setup the credentials file " chat-pack--credentials-file " for this to work.\n"
                   "Here is the needed content to setup to your need into '" chat-pack--credentials-file "':\n"
                   "machine jabber login <your-gmail-login> password <your-gmail-password>")))
