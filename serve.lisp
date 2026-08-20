;;;; serve.lisp — standalone HTTP server for operandi-gui (local browser at :8790).
;;;;
;;;; The transport half of the app: it serves the page (warp list host + a browser
;;;; <input> + the session picker) and bridges the browser to the core projection
;;;; over a plain WebSocket. The gateway path does NOT use this file — it hosts the
;;;; SAME core projection in-process (see warp-channel.lisp). Core owns the model,
;;;; the agent, and sessions; this file owns HTTP.

(in-package #:operandi-gui)

;; warp-dom is only needed by this server, so its nickname is added here (at load of
;; the :operandi-gui/serve system) rather than on the core package — core loads in a
;; process that may not have warp-dom at all.
;; :compile-toplevel too — COMPILE-FILE reads `wd::…` forms below and must resolve the
;; nickname at READ time, which is during compilation, not just at load.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:add-package-local-nickname '#:wd '#:warp-dom (find-package '#:operandi-gui)))

(defparameter *port* (or (ignore-errors (parse-integer (uiop:getenv "OPERANDI_CHAT_PORT"))) 8790))

;;; ------------------------------- the page -----------------------------
(defparameter *page*
  "<!doctype html><html><head><meta charset=utf-8>
<meta name=viewport content='width=device-width,initial-scale=1,user-scalable=no'>
<title>operandi</title>
<style>
 :root{color-scheme:dark}
 html,body{margin:0;height:100%;background:#0d1117;color:#e6edf3;
   font:15px/1.5 -apple-system,system-ui,sans-serif;overflow:hidden}
 #wrap{display:flex;flex-direction:column;height:100%}
 header{padding:9px 14px;font-weight:600;letter-spacing:.02em;color:#9fb4c8;
   border-bottom:1px solid #1b2430;flex:0 0 auto;display:flex;align-items:center;gap:8px}
 header small{color:#5a6b7a;font-weight:400;flex:1 1 auto}
 #sess{background:none;color:#9fb4c8;border:1px solid #2a3644;border-radius:16px;
   padding:5px 12px;font:600 13px/1 inherit}
 #rows{list-style:none;margin:0;padding:12px;flex:1 1 auto;overflow-y:auto;-webkit-overflow-scrolling:touch}
 #rows li{max-width:82%;margin:8px 0;padding:9px 13px;border-radius:14px;
   white-space:pre-wrap;word-break:break-word}
 #rows li .l,#rows li .stale{display:none}      /* kind tag (read by JS) + as-of: not shown */
 #rows li .v{font-size:15px}
 #rows li .v b{font-weight:600}
 #rows li .v code{background:#0d1117;border:1px solid #2a3644;border-radius:5px;
   padding:1px 5px;font:13px/1.4 ui-monospace,SFMono-Regular,Menlo,monospace}
 /* sender styling, applied from the (hidden) kind tag by decorate() */
 #rows li.you{margin-left:auto;background:#1f6feb;color:#fff;border-bottom-right-radius:4px}
 #rows li.bot{margin-right:auto;background:#1b2430;border-bottom-left-radius:4px}
 #rows li.think{margin-right:auto;background:#161d27;color:#7d8b98;font-style:italic}
 #rows li.err{margin-right:auto;background:#3a1c1c;color:#ffb0b0}
 #menu{display:none}
 form{display:flex;gap:8px;padding:10px;border-top:1px solid #1b2430;flex:0 0 auto;background:#0d1117}
 #say{flex:1 1 auto;background:#161d27;border:1px solid #2a3644;border-radius:20px;
   color:#e6edf3;padding:11px 15px;font:15px/1.4 inherit;outline:none}
 #say:focus{border-color:#1f6feb}
 button{background:#1f6feb;color:#fff;border:0;border-radius:20px;padding:0 18px;font:600 15px/1 inherit}
 button:disabled{opacity:.5}
 /* cold-start / Sessions picker */
 #picker{position:absolute;inset:0;background:#0d1117;z-index:10;display:none;
   flex-direction:column;padding:16px}
 #picker.show{display:flex}
 #picker h1{font:600 17px/1.3 inherit;color:#e6edf3;margin:6px 4px 14px}
 #new{align-self:stretch;background:#1f6feb;color:#fff;border:0;border-radius:12px;
   padding:13px;font:600 15px/1 inherit;margin-bottom:14px}
 #slist{list-style:none;margin:0;padding:0;overflow-y:auto;flex:1 1 auto}
 #slist li{padding:13px 14px;margin-bottom:8px;background:#161d27;border:1px solid #1b2430;
   border-radius:12px;cursor:pointer}
 #slist li:hover{border-color:#2a3644}
 #slist .t{color:#e6edf3;font-size:15px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}
 #slist .m{color:#5a6b7a;font-size:12px;margin-top:3px}
 #slist .empty{color:#5a6b7a;text-align:center;padding:24px;background:none;border:0}
</style></head><body>
<div id=wrap>
 <header>operandi <small id=st>· connecting…</small>
   <button id=sess type=button>Sessions</button></header>
 <ul id=rows></ul><ul id=menu></ul>
 <form id=f autocomplete=off>
   <input id=say placeholder='Message operandi…' autofocus>
   <button type=submit>Send</button>
 </form>
</div>
<div id=picker>
 <h1>Chats</h1>
 <button id=new type=button>＋ New chat</button>
 <ul id=slist></ul>
</div>
<script src='/client.js'></script>
<script>
'use strict';
const rowsEl=document.getElementById('rows'), stEl=document.getElementById('st');
const ROWS=200;
let ws=null;                                     // (re)connected when a chat is entered
const client=makeWarpClient({rows:rowsEl,menu:document.getElementById('menu'),viewportRows:ROWS,
  send:o=>{if(ws&&ws.readyState===1)ws.send(JSON.stringify(o));}});
let atBottom=true;
rowsEl.addEventListener('scroll',()=>{atBottom=rowsEl.scrollHeight-rowsEl.scrollTop-rowsEl.clientHeight<40;});
const stick=()=>{if(atBottom)rowsEl.scrollTop=rowsEl.scrollHeight;};
function esc(s){return s.replace(/[&<>]/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;'}[c]));}
function md(s){                                  // just enough for the agent's plain-ish replies.
  // NB: char-classes ([*], not \\*) — a backslash before * is eaten by the Lisp string reader,
  // and /*...*/ would then open a JS comment. No backslashes in these literals on purpose.
  return esc(s).replace(/-{3,}/g,' ')                     // --- horizontal rule -> gone
               .replace(/^#{1,6} +(.+)$/gm,'<b>$1</b>')   // ### Heading -> bold line
               .replace(/`([^`]+)`/g,'<code>$1</code>')
               .replace(/[*][*]([^*]+)[*][*]/g,'<b>$1</b>')
               .replace(/^[-*] (.+)$/gm,'• $1');
}
function decorate(){                             // idempotent: warp repaints rows (fresh as_of each
  rowsEl.querySelectorAll('li').forEach(li=>{    // delta), so reconcile every call, don't one-shot flag
    const tag=((li.querySelector('.l')||{}).textContent||'').trim();
    const cls=tag==='you'?'you':tag==='think'?'think':tag==='err'?'err':'bot';
    if(li.className!==cls)li.className=cls;
  });
  rowsEl.querySelectorAll('.v').forEach(v=>{
    if(v.dataset.raw===undefined)v.dataset.raw=v.textContent;   // capture source before we rewrite it
    const html=md(v.dataset.raw);
    if(v.innerHTML!==html)v.innerHTML=html;      // guard so we don't thrash the DOM
  });
}
function connectWs(){                             // fresh consumer -> hello pulls the current transcript
  if(ws){try{ws.close();}catch(_){}}
  rowsEl.innerHTML='';                            // drop the previous session's rows before the snapshot
  ws=new WebSocket('ws://'+location.host+'/warp');
  ws.onopen=()=>{stEl.textContent='· live';atBottom=true;client.hello(ROWS,0);};
  ws.onclose=()=>{stEl.textContent='· disconnected';};
  ws.onmessage=ev=>{client.apply(ev.data);decorate();requestAnimationFrame(stick);};
}

const f=document.getElementById('f'), say=document.getElementById('say');
f.addEventListener('submit',async e=>{
  e.preventDefault();
  const t=say.value.trim(); if(!t)return;
  say.value=''; atBottom=true;
  try{await fetch('/say',{method:'POST',body:t});}catch(_){}
  say.focus();
});

// ---- session picker: shown cold, and via the Sessions button ----
const picker=document.getElementById('picker'), slist=document.getElementById('slist');
function fmtWhen(id){                             // ids are YYYYMMDD-HHMMSS
  // [0-9], not \\d — a backslash-d is eaten by the Lisp string reader (becomes 'd').
  const m=/^([0-9]{4})([0-9][0-9])([0-9][0-9])-([0-9][0-9])([0-9][0-9])/.exec(id||'');
  return m?`${m[1]}-${m[2]}-${m[3]} ${m[4]}:${m[5]}`:'';
}
async function showPicker(){
  slist.innerHTML='<li class=empty>loading…</li>';
  picker.classList.add('show');
  let items=[];
  try{items=await (await fetch('/sessions')).json();}catch(_){}
  if(!items.length){slist.innerHTML='<li class=empty>No saved chats yet.</li>';return;}
  slist.innerHTML='';
  for(const s of items){
    const li=document.createElement('li');
    const t=document.createElement('div');t.className='t';t.textContent=s.title||'(empty chat)';
    const m=document.createElement('div');m.className='m';
    m.textContent=`${fmtWhen(s.id)} · ${s.turns} turn${s.turns===1?'':'s'}`;
    li.append(t,m);
    li.onclick=async()=>{await fetch('/open',{method:'POST',body:s.id});enterChat();};
    slist.append(li);
  }
}
function enterChat(){picker.classList.remove('show');connectWs();setTimeout(()=>say.focus(),50);}
document.getElementById('new').onclick=async()=>{await fetch('/new',{method:'POST'});enterChat();};
document.getElementById('sess').onclick=showPicker;
// Deep links: ?s=<id> opens a chat, ?new starts one; otherwise the picker (cold start).
const params=new URLSearchParams(location.search);
if(params.get('s')){fetch('/open',{method:'POST',body:params.get('s')}).then(enterChat);}
else if(params.has('new')){fetch('/new',{method:'POST'}).then(enterChat);}
else showPicker();
</script></body></html>")

;;; ------------------------------- server -------------------------------
(defvar *sock* nil) (defvar *accept* nil)

(defun read-body (stream headers)
  (let ((n (or (ignore-errors (parse-integer (cdr (assoc "content-length" headers :test #'string=)))) 0)))
    (if (plusp n)
        (let ((buf (make-array n :element-type '(unsigned-byte 8))))
          (when (= n (read-sequence buf stream))
            (sb-ext:octets-to-string buf :external-format :utf-8)))
        "")))

(defun run-list-consumer (stream)
  "Seat a warp-dom consumer for the message list on this WebSocket."
  (let* ((lock (bt:make-lock))
         (ch (wd:open-channel *proj*
                              :send (lambda (frame) (bt:with-lock-held (lock) (wd::ws-send-text stream frame)))
                              :view 'chat-view :invoker :allowlist :budget 200000 :rows 200 :hz 8
                              :name "chat")))
    (unwind-protect
         (loop for text = (wd::ws-read-message stream) while text
               do (wd:channel-receive ch text))
      (wd:channel-close ch))))

(defun serve-connection (conn)
  (let ((stream (sb-bsd-sockets:socket-make-stream conn :input t :output t
                                                        :element-type '(unsigned-byte 8))))
    (unwind-protect
         (multiple-value-bind (method path headers) (wd::read-http-request stream)
           (cond
             ((null method))
             ((and (string= method "GET") (eql 0 (search "/warp" path)))
              (when (wd::ws-accept stream headers) (run-list-consumer stream)))
             ((and (string= method "GET") (eql 0 (search "/client.js" path)))
              (wd::http-respond stream "200 OK" "application/javascript; charset=utf-8"
                                (wd::client-file "client.js")))
             ((and (string= method "POST") (eql 0 (search "/say" path)))
              (let ((body (read-body stream headers)))
                (when (and body (plusp (length body))) (ignore-errors (say body))))
              (wd::http-respond stream "200 OK" "text/plain" "ok"))
             ((and (string= method "GET") (eql 0 (search "/sessions" path)))
              (wd::http-respond stream "200 OK" "application/json; charset=utf-8"
                                (or (ignore-errors (sessions-json)) "[]")))
             ((and (string= method "POST") (eql 0 (search "/new" path)))
              (read-body stream headers)
              (new-session!)
              (wd::http-respond stream "200 OK" "text/plain" "ok"))
             ((and (string= method "POST") (eql 0 (search "/open" path)))
              (let* ((id (string-trim '(#\Space #\Newline #\Return #\Tab)
                                      (or (read-body stream headers) "")))
                     (ok (and (plusp (length id)) (ignore-errors (open-session! id)))))
                (wd::http-respond stream (if ok "200 OK" "404 Not Found")
                                  "text/plain" (if ok "ok" "no such session"))))
             ((string= method "GET")
              (wd::http-respond stream "200 OK" "text/html; charset=utf-8" *page*))
             (t (wd::http-respond stream "405 Method Not Allowed" "text/plain" "no"))))
      (ignore-errors (close stream))
      (ignore-errors (sb-bsd-sockets:socket-close conn)))))

(defun start (&key (port *port*) (model *model*))
  (start-agent :model model)   ; core: LLM backend + the single worker thread
  ;; No session at boot: the browser shows the picker (resume a chat, or start new).
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address sock) t)
    (sb-bsd-sockets:socket-bind sock #(127 0 0 1) port)
    (sb-bsd-sockets:socket-listen sock 8)
    (setf *sock* sock
          *accept* (bt:make-thread
                    (lambda ()
                      (loop while *running* do
                        (handler-case
                            (let ((conn (sb-bsd-sockets:socket-accept sock)))
                              (bt:make-thread (lambda () (handler-case (serve-connection conn)
                                                           (serious-condition () nil)))
                                              :name "chat-conn"))
                          (serious-condition () (return)))))
                    :name "chat-accept")))
  (format t "~&operandi chat :: http://127.0.0.1:~d/  (model ~a)~%" port model)
  (force-output)
  port)

(defun stop ()
  (setf *running* nil)
  (bt:with-lock-held (*qlock*) (bt:condition-notify *qcv*))
  (ignore-errors (sb-bsd-sockets:socket-close *sock*))
  (setf *sock* nil))

