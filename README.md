chat-pack
=========

A pack to setup one's irc credentials.

# install

In your `.emacs-live.el` add this snippet:
```elisp
(live-add-packs '("/path/to/chat-pack"))
```

# setup

Adding a line to the file `~/.authinfo`:

```txt
machine jabber login your-email@gmail.com password your-password-for-this-account
```

*Note* Do not change `machine jabber`, this is static and used by this pack.

Example:
```txt
machine jabber login tony@gmail.com password your-password-for-this-account
```

# run

`M-x jabber-connect`
