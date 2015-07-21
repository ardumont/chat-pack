chat-pack
=========

A pack to setup one's jabber credentials.

# Install

This is compatible with [emacs-live-packs](https://github.com/ardumont/emacs-live-packs) and [prelude-packs](https://github.com/ardumont/prelude-packs).

## [emacs-live-packs](https://github.com/ardumont/emacs-live-packs)

Add this snippet in your `.emacs-live.el`:
```elisp
(emacs-live-packs/add-live-packs "~/.emacs-live-packs/" '("chat-pack"))
```

## [prelude-packs](https://github.com/ardumont/prelude-packs)

Add this snippet in your `prelude-packs.el`:
```elisp
(prelude-packs/load-packs "~/.prelude-packs/" '("chat-pack"))
```
# Setup

Adding a line to the file `~/.authinfo(.gpg)`:

```txt
machine jabber login your-email@gmail.com password your-password-for-this-account
```

*Note* Do not change `machine jabber`, this is static and used by this pack.

Example:
```txt
machine jabber login tony@gmail.com password your-password-for-this-account
```

# Run

`M-x jabber-connect`
