;;;; serve.lisp — standalone HTTP server for operandi-gui (local browser at :8790).
;;;;
;;;; The transport half of the app: it serves the page (warp list host + a browser
;;;; <input> + the session picker) and bridges the browser to the core projection
;;;; over a plain WebSocket. The gateway path does NOT use this file — it hosts the
;;;; SAME core projection in-process (see warp-channel.lisp). Core owns the model,
;;;; the agent, and sessions; this file owns HTTP.

(in-package #:operandi-gui)

(defparameter *port* (or (ignore-errors (parse-integer (uiop:getenv "OPERANDI_CHAT_PORT"))) 8790))
(defparameter *host* (or (uiop:getenv "OPERANDI_CHAT_HOST") "127.0.0.1")
  "The address to listen on.  Loopback by default: the page drives an agent with tools.")

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
 #rows li.file{margin-right:auto;padding:0;background:none;white-space:normal}
 #rows li.file .file{display:inline-block;max-width:100%;vertical-align:top}
 #rows li.file img{display:block;max-width:100%;max-height:min(70vh,calc(100vh - 210px));min-height:0;width:auto;height:auto;border-radius:10px 10px 0 0}
 #rows li.file .dl{display:block;padding:8px 13px;background:#1b2430;color:#dce4ec;text-decoration:none;
   border-radius:0 0 10px 10px;font-size:14px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}
 #rows li.file .dl:only-child{border-radius:10px}
 #rows li.file .note{padding:8px 13px 0;background:#1b2430;font-size:14px;white-space:normal}
 #rows li.file .note:first-child{border-radius:10px 10px 0 0}
 #rows li.file .note+.dl{color:#8a949c;font-size:13px}
 @media (max-width:600px){#rows li.file{max-width:100%}}  /* a phone: a picture takes the column */
 #rows li.think{margin-right:auto;background:#161d27;color:#7d8b98;font-style:italic}
 #rows li.think .dt{display:none}
 #rows li.think .dt .rs{font:italic 13px/1.5 -apple-system,system-ui,sans-serif;white-space:normal;color:#8b98a5;
   border-left:2px solid #3a4a5c;padding:1px 0 1px 9px;margin:6px 0}
 #rows li.think .dt .tc{margin:6px 0;padding-left:11px;white-space:pre-wrap}
 #rows li.think .dt .rs p{margin:0 0 6px} #rows li.think .dt .rs p:last-child{margin:0}
 #rows li.think .dt>:first-child{margin-top:0}
 #rows li.think .dt>:last-child{margin-bottom:0}
 #rows li.think.bare{display:none}
 body.tools #rows li.think .dt{display:block;margin-top:7px;font:11px/1.6 ui-monospace,Menlo,monospace;
   font-style:normal;color:#6f7c88;border-top:1px solid rgba(255,255,255,.08);padding-top:6px}
 body.tools #rows li.think.more::after{content:''}
 #tools{background:none;color:#9fb4c8;border:1px solid #2a3644;border-radius:16px;
   padding:5px 12px;font:12px/1 inherit;cursor:pointer;margin-left:auto;margin-right:8px}
 body.tools #tools{background:#1b2430;color:#dce4ec}
 #rows li.think.open .dt{display:block;margin-top:7px;font:11px/1.6 ui-monospace,Menlo,monospace;
   font-style:normal;color:#6f7c88;border-top:1px solid rgba(255,255,255,.08);padding-top:6px}
 #rows li.think.more{cursor:pointer}
 #rows li.think.more::after{content:' ▾';color:#4d5862;font-style:normal}
 #rows li.think.more.open::after{content:' ▴'}
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
   <button id=tools type=button title='tool calls: one line each, or all of them'>Tools</button>
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
const NL=String.fromCharCode(10), LS=String.fromCharCode(8232);
function paras(t){                              // a thought's own lines: blank runs as one gap
  return t.split(LS).map(x=>x.trim()).join(LS).split(LS+LS)
          .map(x=>x.split(LS).filter(Boolean).map(md).join('<br>')).filter(Boolean).map(x=>'<p>'+x+'</p>').join('');
}
function detail(rest){                           // one entry per line: reasoning (its breaks sent as
  let out='', calls=[];                          // U+2028) as a quoted block, a run of calls as a list
  const end=()=>{if(calls.length){out+='<div class=tc>'+calls.join('')+'</div>';calls=[];}};
  rest.split(NL).forEach(e=>{
    if(!e.trim())return;
    if(e.startsWith('✻')){end();out+='<div class=rs>'+paras(e.slice(1))+'</div>';}
    else calls.push('<div>'+md(e.trim())+'</div>');
  });
  end(); return out;
}
let live=false;                                  // the link is up (the header shows a turn's status then)
function fmtSize(n){return n<1024?n+' B':n<1048576?Math.round(n/1024)+' KB':(n/1048576).toFixed(1)+' MB';}
function fileEl(f){                              // built, not written as HTML: a name is never markup
  const box=document.createElement('div');box.className='file';
  const link=()=>{const a=document.createElement('a');a.href=f.href;a.download=f.name;return a;};
  if(f.preview&&/^data:image[/](png|jpeg|gif|webp);base64,[A-Za-z0-9+/=]+$/.test(f.preview)){
    const a=link(),im=document.createElement('img');im.src=f.preview;im.alt=f.name;a.appendChild(im);box.appendChild(a);}
  if(f.note){const n=document.createElement('div');n.className='note';n.textContent=f.note;box.appendChild(n);}
  const d=link();d.className='dl';d.textContent='⬇ '+f.name+' · '+fmtSize(f.size);box.appendChild(d);
  return box;
}
function decorate(){                             // idempotent: warp repaints rows (fresh as_of each
  let bare=null;
  rowsEl.querySelectorAll('li').forEach(li=>{    // delta), so reconcile every call, don't one-shot flag
    const tag=((li.querySelector('.l')||{}).textContent||'').trim();
    const cls=tag==='you'?'you':tag==='think'?'think':tag==='err'?'err':'bot';
    const v=li.querySelector('.v'); if(!v){if(li.className!==cls)li.className=cls;return;}
    if(v.dataset.raw===undefined)v.dataset.raw=v.textContent;   // capture source before we rewrite it
    if(tag==='file'){                            // a file: a download, with its picture when it is one
      if(li.className!=='file')li.className='file';
      if(!v.querySelector('.file')){let f=null;try{f=JSON.parse(v.dataset.raw);}catch(_){}
        if(f){v.textContent='';v.appendChild(fileEl(f));}}
      return;
    }
    if(cls==='think'){                           // <status> NL one line per finished call (client/chat.js):
      const raw=v.dataset.raw, nl=raw.indexOf(NL);              // the list folds under the status, a tap opens it
      const head=nl<0?raw:raw.slice(0,nl), rest=nl<0?'':raw.slice(nl+1).trim();
      const html=md(head)+(rest?'<div class=dt>'+detail(rest)+'</div>':'');
      if(v.innerHTML!==html)v.innerHTML=html;
      if(!rest)bare=head;                         // no calls yet: no row, its status goes in the header
      const want='think'+(rest?' more':' bare')+(rest&&li.dataset.open==='1'?' open':'');
      if(li.className!==want)li.className=want;  // the open flag lives in the dataset, where a repaint keeps it
      return;
    }
    if(li.className!==cls)li.className=cls;
    const html=md(v.dataset.raw);
    if(v.innerHTML!==html)v.innerHTML=html;      // guard so we don't thrash the DOM
  });
  if(live){const st='· '+(bare||'live'); if(stEl.textContent!==st)stEl.textContent=st;}
}
const toolsBtn=document.getElementById('tools');   // tool rows one line each (a tap opens one), or all open;
function setTools(on){document.body.classList.toggle('tools',on);try{localStorage.setItem('tools',on?'1':'0');}catch(_){}}
setTools((()=>{try{return localStorage.getItem('tools')==='1';}catch(_){return false;}})());   // kept
toolsBtn.onclick=()=>setTools(!document.body.classList.contains('tools'));
rowsEl.addEventListener('click',e=>{             // delegated: warp replaces rows wholesale
  const li=e.target.closest&&e.target.closest('li.think.more'); if(!li)return;
  li.dataset.open=li.dataset.open==='1'?'':'1'; decorate();
});
let chatId=null, retry=null;                     // the chat this page shows; a pending reconnect
function setChat(id){                             // the URL names the open chat, so a reload resumes it
  chatId=id||null;
  history.replaceState(null,'',chatId?'?s='+encodeURIComponent(chatId):location.pathname);
}
function connectWs(){                             // fresh consumer -> hello pulls the current transcript
  if(retry){clearTimeout(retry);retry=null;}
  if(ws){ws.onclose=null;try{ws.close();}catch(_){}}
  rowsEl.innerHTML='';                            // drop the previous session's rows before the snapshot
  const me=ws=new WebSocket((location.protocol==='https:'?'wss://':'ws://')+location.host+'/warp');
  ws.onopen=()=>{live=true;stEl.textContent='· live';atBottom=true;client.hello(ROWS,0);};
  ws.onclose=()=>{if(ws!==me)return;live=false;stEl.textContent='· reconnecting…';retry=setTimeout(reconnect,2000);};
  ws.onmessage=ev=>{client.apply(ev.data);decorate();requestAnimationFrame(stick);};
}
async function reconnect(){                       // the server may have restarted: put our chat back first
  retry=null;
  try{
    const cur=(await (await fetch('/current')).text()).trim();
    if(chatId&&cur!==chatId){
      const r=await fetch('/open',{method:'POST',body:chatId});
      if(!r.ok){setChat(null);stEl.textContent='· live';showPicker();return;}
    }
    connectWs();
  }catch(_){stEl.textContent='· reconnecting…';retry=setTimeout(reconnect,3000);}
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
    li.onclick=async()=>{await fetch('/open',{method:'POST',body:s.id});setChat(s.id);enterChat();};
    slist.append(li);
  }
}
function enterChat(){picker.classList.remove('show');connectWs();setTimeout(()=>say.focus(),50);}
async function newChat(){setChat((await (await fetch('/new',{method:'POST'})).text()).trim());enterChat();}
document.getElementById('new').onclick=newChat;
document.getElementById('sess').onclick=showPicker;
// Deep links: ?s=<id> opens a chat, ?new starts one; otherwise the picker (cold start).
const params=new URLSearchParams(location.search);
if(params.get('s')){fetch('/open',{method:'POST',body:params.get('s')})
  .then(r=>{if(r.ok){setChat(params.get('s'));enterChat();}else{setChat(null);showPicker();}});}
else if(params.has('new')){newChat();}
else showPicker();
</script></body></html>")

;;; ------------------------------- server -------------------------------
(defvar *sock* nil) (defvar *accept* nil)

(defun read-body (stream headers)
  (let ((n (or (ignore-errors (parse-integer (cdr (assoc "content-length" headers :test #'string=)))) 0)))
    (if (plusp n)
        (let ((buf (make-array n :element-type '(unsigned-byte 8))))
          (when (= n (read-sequence buf stream))
            (babel:octets-to-string buf :encoding :utf-8 :errorp nil)))
        "")))

(defun run-list-consumer (stream)
  "Seat a warp-dom consumer for the message list on this WebSocket."
  (let* ((lock (bt:make-lock))
         (ch (warp-dom:open-channel *proj*
                              :send (lambda (frame) (bt:with-lock-held (lock) (warp-dom::ws-send-text stream frame)))
                              :view 'chat-view :invoker :allowlist :budget 200000 :rows 200 :hz 8
                              :name "chat")))
    (unwind-protect
         (loop for text = (warp-dom::ws-read-message stream) while text
               do (warp-dom:channel-receive ch text))
      (warp-dom:channel-close ch))))

(defun respond-file (stream path name mime)
  "A stored file's bytes, as a download named NAME."
  (let ((bytes (with-open-file (in path :element-type '(unsigned-byte 8))
                 (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8)))) (read-sequence v in) v)))
        (safe (substitute-if #\_ (lambda (c) (or (char= c #\") (char= c #\\) (char< c #\Space) (char> c #\~))) name)))
    (write-sequence (sb-ext:string-to-octets
                     (format nil "HTTP/1.1 200 OK~c~cContent-Type: ~a~c~cContent-Length: ~d~c~cContent-Disposition: attachment; filename=\"~a\"~c~cCache-Control: private, max-age=86400~c~cConnection: close~c~c~c~c"
                             #\Return #\Newline mime #\Return #\Newline (length bytes) #\Return #\Newline safe
                             #\Return #\Newline #\Return #\Newline #\Return #\Newline #\Return #\Newline)
                     :external-format :latin-1)
                    stream)
    (write-sequence bytes stream)
    (finish-output stream)))

(defun serve-connection (conn)
  (let ((stream (sb-bsd-sockets:socket-make-stream conn :input t :output t
                                                        :element-type '(unsigned-byte 8))))
    (unwind-protect
         (multiple-value-bind (method path headers) (warp-dom::read-http-request stream)
           (cond
             ((null method))
             ((and (string= method "GET") (eql 0 (search "/warp" path)))
              (when (warp-dom::ws-accept stream headers) (run-list-consumer stream)))
             ((and (string= method "GET") (eql 0 (search "/client.js" path)))
              (warp-dom::http-respond stream "200 OK" "application/javascript; charset=utf-8"
                                (warp-dom::client-file "client.js")))
             ((and (string= method "POST") (eql 0 (search "/say" path)))
              (let ((body (read-body stream headers)))
                (when (and body (plusp (length body))) (ignore-errors (say body))))
              (warp-dom::http-respond stream "200 OK" "text/plain" "ok"))
             ;; GET /file/<session>/<id>: a file the agent handed over, from that chat's store
             ((and (string= method "GET") (eql 0 (search "/file/" path)))
              (let* ((parts (uiop:split-string (subseq path 6) :separator "/"))
                     (sid (first parts)) (id (second parts)))
                (multiple-value-bind (f name size mime) (and sid id (stored-file id sid))
                  (declare (ignore size))
                  (if f
                      (respond-file stream f name mime)
                      (warp-dom::http-respond stream "404 Not Found" "text/plain" "no such file")))))
             ((and (string= method "GET") (eql 0 (search "/sessions" path)))
              (warp-dom::http-respond stream "200 OK" "application/json; charset=utf-8"
                                (or (ignore-errors (sessions-json)) "[]")))
             ((and (string= method "POST") (eql 0 (search "/new" path)))
              (read-body stream headers)
              (warp-dom::http-respond stream "200 OK" "text/plain" (or (new-session!) "")))
             ((and (string= method "GET") (eql 0 (search "/current" path)))
              (warp-dom::http-respond stream "200 OK" "text/plain" (or (current-session-id) "")))
             ((and (string= method "POST") (eql 0 (search "/open" path)))
              (let* ((id (string-trim '(#\Space #\Newline #\Return #\Tab)
                                      (or (read-body stream headers) "")))
                     (ok (and (plusp (length id))
                              ;; the open chat (perhaps mid-turn, not yet saved) stays as it is
                              (or (equal id (current-session-id)) (ignore-errors (open-session! id))))))
                (warp-dom::http-respond stream (if ok "200 OK" "404 Not Found")
                                  "text/plain" (if ok "ok" "no such session"))))
             ((string= method "GET")
              (warp-dom::http-respond stream "200 OK" "text/html; charset=utf-8" *page*))
             (t (warp-dom::http-respond stream "405 Method Not Allowed" "text/plain" "no"))))
      (ignore-errors (close stream))
      (ignore-errors (sb-bsd-sockets:socket-close conn)))))

(defun start (&key (host *host*) (port *port*) (model *model*))
  (start-agent :model model)   ; core: LLM backend + the single worker thread
  ;; No session at boot: the browser shows the picker (resume a chat, or start new).
  (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address sock) t)
    (sb-bsd-sockets:socket-bind sock (sb-bsd-sockets:make-inet-address host) port)
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
  (format t "~&operandi chat :: http://~a:~d/  (model ~a)~%" host port *model*)
  (force-output)
  port)

(defun stop ()
  (setf *running* nil)
  (bt:with-lock-held (*qlock*) (bt:condition-notify *qcv*))
  (ignore-errors (sb-bsd-sockets:socket-close *sock*))
  (setf *sock* nil))

