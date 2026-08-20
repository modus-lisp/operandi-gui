;;;; operandi-gui — the operandi agent as a warp app.
;;;;
;;;; :operandi-gui is the CORE: the conversation model, the warp projection, sessions,
;;;; and the agent worker. It carries no transport and no framebuffer (depends on
;;;; :warp — bordeaux-threads, nothing pixel — plus :operandi), so it can be loaded
;;;; in-process by a host that already has a link to the phone: the WebRTC gateway
;;;; hosts this projection on stream 102 the same way it hosts the device manager.
;;;;
;;;; :operandi-gui/serve is the STANDALONE HTTP server (a browser page with a warp list
;;;; + an <input> + a session picker) for local use at 127.0.0.1:8790.

(defsystem "operandi-gui"
  :description "operandi chat as a warp projection — model, sessions, agent; no transport."
  :version "0.1.0"
  :author "ynniv"
  :license "MIT"
  :depends-on ("warp" "operandi" "bordeaux-threads")
  :serial t
  :components ((:file "core")))

(defsystem "operandi-gui/serve"
  :description "Standalone HTTP host for operandi-gui: the browser page + a plain-WebSocket warp link."
  :depends-on ("operandi-gui" "warp-dom/serve")
  :serial t
  :components ((:file "serve")))
