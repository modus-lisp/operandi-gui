# operandi-gui

A chat GUI for the [operandi](https://github.com/modus-lisp/operandi) agent, built as a
[warp](https://github.com/modus-lisp/warp) app. warp renders the *conversation* — a keyed,
delta-updated list, which is what warp is good at — and the browser owns the *keyboard*: text is
composed in a plain `<input>`, since warp's gesture vocabulary is deliberately closed and carries no
free text. So: warp for the transcript, the browser for typing — a hybrid, exactly as intended.

It has two surfaces, from one core:

- **Standalone HTTP page** (`:operandi-gui/serve`) — a browser page at `127.0.0.1:8790` with message
  bubbles, light markdown, and a **cold-start session picker** (resume a saved chat or start a new
  one; conversations persist to `~/.operandi/chat-sessions/` via `operandi.session`). Bookmarkable:
  `?s=<id>` opens a chat, `?new` starts one.
- **In-process on a host** (`:operandi-gui` core) — the model, the warp projection, sessions, and the
  agent worker, with **no transport and no framebuffer** (depends on `:warp` — bordeaux-threads,
  nothing pixel — plus `:operandi`). That's what lets a host that already has a link to a phone load
  it and serve the projection directly. It rides the [glass](https://github.com/modus-lisp/glass)
  WebRTC gateway as a warp-dom app on the same data-channel stream as the device manager, so the
  transcript reaches a phone with the keyboard in the phone's real browser.

On the phone surface it also does voice, both ways, on the from-scratch audio stack:

- **speak** — replies are voiced by [chord](https://github.com/modus-lisp/chord) (markdown stripped
  first, so it reads instead of saying "hash hash hash"), and each sentence lights up as it's read.
- **dictate** — [stave](https://github.com/modus-lisp/stave) transcribes the phone's mic into the
  input box for review before you send.

## Run the standalone

```
sbcl --non-interactive --load bin/standalone.lisp     # serves http://127.0.0.1:8790/
```

`OPERANDI_CHAT_PORT` / `OPERANDI_CHAT_MODEL` override the defaults.

## Systems

| system | what | depends on |
| --- | --- | --- |
| `operandi-gui` | model, projection, sessions, agent — no transport | `warp`, `operandi` |
| `operandi-gui/serve` | the standalone HTTP host | `operandi-gui`, `warp-dom/serve` |

MIT.
