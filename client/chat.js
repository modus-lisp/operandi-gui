// operandi-gui/client/chat.js — the operandi chat panel for a WebRTC-gateway payload.
//
// A gateway's client (webrtc-data's payload.js) imports mountChat and calls it once.  The whole
// panel lives HERE, in operandi-gui, not in the gateway's demo: bubbles + markdown, the phone
// transcript, the voice controls (speak / stop / dictate), dictation into the input box, and the
// sentence karaoke that lights each line as chord reads it.  It borrows the shell's warp client and
// its mic/speaker buttons — passed in — rather than re-acquiring anything.
//
// The gateway side (warp-channel.lisp) has the matching Lisp half in :operandi-gui/gateway: chord
// voice out, stave dictation in, and the {a:chat,...} control frames this panel sends.

const CHAT_CSS = "    /* ==== BEGIN the operandi chat's stylesheet ===============================================\n       THE THIRD WARP APP, ON THE SAME CHANNEL.  Rows arrive as .v (the message text) + .l (a\n       hidden kind tag: you|bot|think|err) exactly like the panels above; chatDecorate() reads the\n       tag to pick a bubble class and renders a little markdown into .v.  The one thing warp does\n       not carry is free text IN \u2014 so this panel, unlike the two above, has an <input>, and its\n       sends are {a:'chat',say:\u2026} which the gateway hands to the agent.\n\n       GEOMETRY IS NOT HERE.  This panel carries .richPanel, and that class owns where it\n       sits -- windowed cap, and the full-screen rule the switcher needs.  An ID selector\n       beats two classes, so a left/top/bottom here would silently win over .richPanel.full\n       and the panel would wear the class while keeping its inset.  That exact bug already\n       cost #warpPanel a debugging session; this rule keeps only what is its own. */\n    #chatPanel{position:fixed;z-index:23;display:none;\n      flex-direction:column;background:rgba(8,10,14,.93);border:1px solid rgba(255,255,255,.12);\n      border-radius:12px;overflow:hidden;\n      font:13px/1.4 -apple-system,system-ui,sans-serif;color:#dce4ec}\n    #chatHead{display:flex;justify-content:space-between;align-items:baseline;gap:10px;\n      padding:9px 12px;border-bottom:1px solid rgba(255,255,255,.1);color:#8a949c;flex:0 0 auto;\n      font:12px/1.35 ui-monospace,SFMono-Regular,Menlo,monospace}\n    #chatHead b{color:#dce4ec;font-weight:600;letter-spacing:.04em}\n    /* voice controls: a strip just above the input, not in the header */\n    /* settings: a sheet above the input, so a phone is never asked to type a slash */\n    #chatSet{display:none;flex-direction:column;gap:9px;padding:11px 12px;\n      border-top:1px solid rgba(255,255,255,.12);background:#0d1117;flex:0 0 auto}\n    #chatSet .r{display:flex;align-items:center;gap:10px;justify-content:space-between}\n    #chatSet b{color:#dce4ec;font:600 13px/1.3 ui-monospace,Menlo,monospace;letter-spacing:.04em}\n    #chatSet .l{color:#8a949c;font-size:11px}\n    #chatSet .l span{color:#dce4ec;font-family:ui-monospace,Menlo,monospace}\n    #chatSetList{display:flex;flex-wrap:wrap;gap:7px}\n    #chatSetList button,#chatSet .r button{background:none;border:1px solid rgba(255,255,255,.18);\n      color:#b3bcc4;border-radius:8px;padding:7px 11px;font-size:12px;cursor:pointer;touch-action:manipulation}\n    #chatSetList button.on{background:#1f6feb;border-color:#1f6feb;color:#fff}\n    #chatSetForm{display:flex;gap:7px}\n    #chatSetSlug{flex:1 1 auto;background:#161d27;border:1px solid #2a3644;border-radius:8px;\n      color:#e6edf3;padding:7px 10px;font:12px/1.3 ui-monospace,Menlo,monospace;outline:none}\n    #chatSetForm button{background:#1f6feb;color:#fff;border:0;border-radius:8px;padding:0 13px;font:600 12px/1 inherit}\n    #chatGear{background:none;border:1px solid rgba(255,255,255,.18);color:#b3bcc4;border-radius:8px;\n      padding:3px 9px;font-size:13px;line-height:1.3;cursor:pointer;margin-left:auto;touch-action:manipulation}\n    #chatCtl{display:flex;align-items:center;gap:12px;padding:6px 12px 2px;flex:0 0 auto}\n    #chatSpeakLbl{color:#8a949c;display:flex;align-items:center;gap:5px;\n      cursor:pointer;user-select:none;font-size:12px}\n    #chatSpeakLbl input{accent-color:#1f6feb;width:16px;height:16px}\n    #chatStop,#chatMic{background:none;border:1px solid rgba(255,255,255,.18);color:#b3bcc4;\n      border-radius:8px;padding:3px 10px;font-size:12px;line-height:1.3;cursor:pointer;touch-action:manipulation}\n    #chatStop:active,#chatMic:active{background:rgba(255,255,255,.12)}\n    #chatMic.on{background:#c0392b;border-color:#c0392b;color:#fff}   /* recording = red */\n    #chatBody{flex:1 1 auto;overflow-y:auto;-webkit-overflow-scrolling:touch;padding:10px}\n    #chatRows{list-style:none;margin:0;padding:0}\n    #chatRows li{max-width:82%;margin:7px 0;padding:8px 12px;border-radius:14px;\n      white-space:pre-wrap;word-break:break-word}\n    #chatRows li .l,#chatRows li .stale{display:none}\n    #chatRows li .v b{font-weight:600}\n    #chatRows li .v code{background:#0b0e12;border:1px solid #2a3644;border-radius:5px;\n      padding:1px 5px;font:12px/1.3 ui-monospace,SFMono-Regular,Menlo,monospace}\n    #chatRows li .v .sent{border-radius:5px;transition:background .1s}\n    #chatRows li .v .sent.on{background:rgba(31,111,235,.5);box-shadow:0 0 0 3px rgba(31,111,235,.5);color:#fff}\n    #chatRows li.you{margin-left:auto;background:#1f6feb;color:#fff;border-bottom-right-radius:4px}\n    #chatRows li.bot{margin-right:auto;background:#1b2430;border-bottom-left-radius:4px}\n    #chatRows li.think{margin-right:auto;background:#161d27;color:#7d8b98;font-style:italic}\n    #chatRows li.err{margin-right:auto;background:#3a1c1c;color:#ffb0b0}\n    #chatForm{display:flex;gap:8px;padding:9px;border-top:1px solid rgba(255,255,255,.1);flex:0 0 auto}\n    #chatInput{flex:1 1 auto;background:#161d27;border:1px solid #2a3644;border-radius:18px;\n      color:#e6edf3;padding:9px 14px;font:14px/1.3 -apple-system,system-ui,sans-serif;outline:none}\n    #chatInput:focus{border-color:#1f6feb}\n    #chatForm button{background:#1f6feb;color:#fff;border:0;border-radius:18px;padding:0 16px;\n      font:600 14px/1 inherit}\n    #chatNote{padding:6px 12px;color:#8a949c;flex:0 0 auto;font:11px/1.35 ui-monospace,Menlo,monospace;\n      border-top:1px solid rgba(255,255,255,.08)}\n    #chatNote:empty{display:none}\n    /* ==== END the operandi chat's stylesheet ================================================= */";

export function mountChat({ warpCh, makeWarpClient, warpSend, richApps, micBtn, spkBtn, isOn, diag }) {
  const st = document.createElement('style'); st.textContent = CHAT_CSS; document.head.appendChild(st);

    // ==== BEGIN the operandi chat — client three, and the first one you can TYPE at ===========
    //
    // Same shape as the two apps above — a makeWarpClient on stream 102, labelled `a:'chat'` — for
    // the TRANSCRIPT, which is a flat list and exactly what warp is good at.  The difference is the
    // one warp deliberately does not carry: free text IN.  Its gesture vocabulary is closed, so the
    // panel grows an <input>, and a send is {a:'chat',say:…} — a frame the gateway peels off before
    // the mux and hands to the in-process agent (warp-channel.lisp:MAYBE-WARP-CHAT-SAY).  This runs
    // in the phone's REAL browser, so unlike loom it has fetch/DOM/regex — the markdown + bubble
    // styling below is the same code the standalone page uses.
    const chatEsc = s => s.replace(/[&<>]/g, c => ({ '&':'&amp;','<':'&lt;','>':'&gt;' }[c]));
    const chatMd = s => chatEsc(s)                // [*] char-classes, no backslashes (see the core)
      .replace(/-{3,}/g, ' ')                     // --- horizontal rule -> gone
      .replace(/^#{1,6} +(.+)$/gm, '<b>$1</b>')   // ### Heading -> bold line (no more "hash hash hash")
      .replace(/`([^`]+)`/g, '<code>$1</code>')
      .replace(/[*][*]([^*]+)[*][*]/g, '<b>$1</b>')
      .replace(/^[-*] (.+)$/gm, '• $1');
    // sentence splitter — MUST match core.lisp SPLIT-SENTENCES (same text -> same chunks) so the
    // gateway's "highlight sentence K" lines up with the K-th span we wrap here.
    const chatSplit = t => { const o = []; let s = 0;
      const push = e => { const c = t.slice(s, e).trim(); if (c) o.push(c); };
      for (let i = 0; i < t.length; i++) { const ch = t[i];
        if (ch === '\n') { push(i); s = i + 1; }
        else if ((ch === '.' || ch === '!' || ch === '?') &&
                 (i + 1 >= t.length || t[i + 1] === ' ' || t[i + 1] === '\n') &&
                 !(ch === '.' && i > 0 && t[i - 1] >= '0' && t[i - 1] <= '9')) {  // not "1." / "3.14"
          push(i + 1); s = i + 1; } }
      push(t.length); return o; };
    const chatMdSent = (raw, active) => chatSplit(raw)
      .map((s, i) => '<span class="sent' + (i === active ? ' on' : '') + '">' + chatMd(s) + '</span>')
      .join(' ');
    const chatPanel = document.createElement('div');
    chatPanel.id = 'chatPanel';
    // .richPanel is what makes this a first-class warp app: the shared windowed cap, and
    // the .full rule showApp() toggles when the panel would be most of the screen.
    chatPanel.className = 'richPanel';
    chatPanel.innerHTML =
      '<div id="chatHead"><b>operandi</b><span id="chatStat">—</span></div>' +
      '<div id="chatBody"><ul id="chatRows"></ul></div>' +
      '<div id="chatNote"></div>' +
      // SETTINGS, because a phone keyboard makes a slash expensive.  Every control here sends the
      // same /command the box already parses, so a tap and a typed line are one code path.
      '<div id="chatSet"><div class="r"><b>Settings</b>' +
      '<button type="button" id="chatSetDone">done</button></div>' +
      '<div class="l">Model — now <span id="chatSetNow">…</span></div>' +
      '<div id="chatSetList"></div>' +
      '<div class="l">Effort — now <span id="chatSetEffortNow">…</span></div>' +
      '<div id="chatSetEffort"></div>' +
      '<div class="l">Context — now <span id="chatSetCtxNow">…</span></div>' +
      '<div id="chatSetCtx"></div>' +
      '<form id="chatSetForm"><input id="chatSetSlug" placeholder="another slug, e.g. vendor/model">' +
      '<button type="submit">set</button></form>' +
      '<div class="r"><button type="button" id="chatSetNew">＋ New conversation</button></div></div>' +
      '<div id="chatCtl">' +
      '<button type="button" id="chatMic" title="dictate a message">🎤 dictate</button>' +
      '<label id="chatSpeakLbl"><input type="checkbox" id="chatSpeak"> 🔊 speak</label>' +
      '<button type="button" id="chatStop" title="stop speaking">⏹ stop</button>' +
      '<button type="button" id="chatGear" title="settings">⚙</button>' +
      '</div>' +
      '<form id="chatForm" autocomplete="off"><input id="chatInput" ' +
      'placeholder="Message operandi…"><button type="submit">Send</button></form>';
    document.body.appendChild(chatPanel);
    const chatStat = chatPanel.querySelector('#chatStat');
    const chatNote = chatPanel.querySelector('#chatNote');
    const chatBody = chatPanel.querySelector('#chatBody');
    const chatRowsEl = chatPanel.querySelector('#chatRows');
    const chatInput = chatPanel.querySelector('#chatInput');
    const chatSpeak = chatPanel.querySelector('#chatSpeak');
    // the voice toggle: tell the box to speak (or stop speaking) replies via chord on the desktop
    chatSpeak.addEventListener('change', () => {
      warpSend({ a: 'chat', speak: chatSpeak.checked });
      // hearing replies means the speaker is on — unmute it (and let the shell's 🔈 show it)
      if (chatSpeak.checked && !isOn(spkBtn)) spkBtn.click();
    });
    // the stop button: silence the current utterance now, without changing the toggle
    chatPanel.querySelector('#chatStop').addEventListener('click', () => warpSend({ a: 'chat', hush: true }));
    // ---- settings -------------------------------------------------------------------------
    // Each control sends {cmd:"/…"} — the SAME command the box parses when typed — so switching is
    // still preflighted and still lands in the transcript.  Then it asks {status:true} and paints
    // what the box ACTUALLY has, rather than assuming what it asked for stuck.
    const CHAT_MODELS = [
      ['z-ai/glm-5.3-flash',         'glm-5.3-flash'],
      ['deepseek/deepseek-v4.1-flash', 'deepseek-v4.1-flash'],
      ['minimax/minimax-m2.7',       'minimax-m2.7'],
      ['anthropic/claude-haiku-4.5', 'claude-haiku-4.5'],
    ];
    // Effort and context are ENGINE settings, not chat settings, but they belong on the same sheet
    // because the phone has no other way to reach them.  Same contract as the models above: send
    // the /command a person would type, then repaint from what the box reports back.
    const CHAT_EFFORTS = ['off', 'low', 'medium', 'high', 'default'];
    const CHAT_CTX     = [8, 16, 24, 48, 96];          // thousands of tokens
    const chatSet = chatPanel.querySelector('#chatSet');
    const chatSetNow = chatPanel.querySelector('#chatSetNow');
    const chatSetList = chatPanel.querySelector('#chatSetList');
    const chatSetSlug = chatPanel.querySelector('#chatSetSlug');
    const chatSetEffort = chatPanel.querySelector('#chatSetEffort');
    const chatSetEffortNow = chatPanel.querySelector('#chatSetEffortNow');
    const chatSetCtx = chatPanel.querySelector('#chatSetCtx');
    const chatSetCtxNow = chatPanel.querySelector('#chatSetCtxNow');
    let curModel = null, curEffort = null, curCtx = null;
    const paintModels = () => {
      chatSetList.innerHTML = '';
      for (const pair of CHAT_MODELS) {
        const b = document.createElement('button');
        b.type = 'button'; b.textContent = pair[1];
        if (pair[0] === curModel) b.className = 'on';
        b.onclick = () => { warpSend({ a: 'chat', cmd: '/model ' + pair[0] }); chatSetNow.textContent = 'switching…'; };
        chatSetList.append(b);
      }
    };
    // one painter for both strips: the values differ, the behaviour does not
    const paintChoices = (el, values, current, label, cmd) => {
      el.innerHTML = '';
      for (const v of values) {
        const b = document.createElement('button');
        b.type = 'button'; b.textContent = label(v);
        if (v === current) b.className = 'on';
        b.onclick = () => warpSend({ a: 'chat', cmd: cmd(v) });
        el.append(b);
      }
    };
    const paintEfforts = () =>
      paintChoices(chatSetEffort, CHAT_EFFORTS, curEffort, v => v, v => '/effort ' + v);
    const paintCtx = () =>
      paintChoices(chatSetCtx, CHAT_CTX, curCtx, v => v + 'k', v => '/context ' + v);

    const openSet = on => {
      chatSet.style.display = on ? 'flex' : 'none';
      if (on) { chatInput.blur(); warpSend({ a: 'chat', status: true }); paintModels(); paintEfforts(); paintCtx(); }
    };
    chatPanel.querySelector('#chatGear').addEventListener('click', () => openSet(chatSet.style.display !== 'flex'));
    chatPanel.querySelector('#chatSetDone').addEventListener('click', () => openSet(false));
    chatPanel.querySelector('#chatSetNew').addEventListener('click', () => {
      warpSend({ a: 'chat', cmd: '/new' }); openSet(false);
    });
    chatPanel.querySelector('#chatSetForm').addEventListener('submit', e => {
      e.preventDefault();
      const v = chatSetSlug.value.trim(); if (!v) return;
      chatSetSlug.value = ''; chatSetSlug.blur();
      warpSend({ a: 'chat', cmd: '/model ' + v }); chatSetNow.textContent = 'switching…';
    });

    // the dictate button: unmute the phone mic (by driving the shell's OWN 🎙, so its getUserMedia,
    // its sender, and its button state all move together and the UI reflects the live mic) and tell
    // the box to start its ear; the transcript streams back into the input for review.
    const chatMic = chatPanel.querySelector('#chatMic');
    let chatDidMic = false;  // did dictation turn the shell mic on (so it should turn it back off)?
    let lastDictate = '';    // the exact text dictation last wrote to the box, so we can tell edits apart
    chatMic.addEventListener('click', () => {
      if (!chatMic.classList.contains('on')) {
        if (!isOn(micBtn)) { micBtn.click(); chatDidMic = true; }  // shell unmutes + 🎙 goes green
        // ...AND THE SPEAKER, because dictating is a conversation.  You are talking to something
        // that answers out loud, and having to find a second control -- one the full-screen panel
        // covers -- before you can hear the answer is a worse default than the alternative.
        //
        // THE ALTERNATIVE BEING ACOUSTIC FEEDBACK: a handset with the speaker on and the mic open
        // is a path from the voice back into the ear.  It is not the path that produced the
        // self-transcription we chased (the ear no longer falls back to the desktop's mix at all;
        // it is pinned to the phone's microphone), but it is a real one on a speakerphone, and it
        // is the operator's to accept.  This is switched on deliberately, for headphones.
        //
        // It STAYS on when dictation stops, which is not an oversight: the reply you dictated
        // arrives after you stop talking, and turning the speaker off at exactly that moment would
        // mute the thing you asked for.  The button is visible and green; turning it back off is
        // one tap.
        if (!isOn(spkBtn)) spkBtn.click();
        chatInput.value = ''; lastDictate = ''; chatInput.blur();  // fresh box, and drop the keyboard
        warpSend({ a: 'chat', listen: true });
        chatMic.classList.add('on'); chatInput.placeholder = 'listening… tap 🎤 to stop';
      } else {
        warpSend({ a: 'chat', listen: false });
        if (chatDidMic && isOn(micBtn)) micBtn.click();           // shell re-mutes + 🎙 goes back
        chatDidMic = false;
        chatMic.classList.remove('on'); chatInput.placeholder = 'Message operandi…'; chatInput.focus();
      }
    });
    let chatAtBottom = true;
    chatBody.addEventListener('scroll', () => {
      chatAtBottom = chatBody.scrollHeight - chatBody.scrollTop - chatBody.clientHeight < 48; });
    let hlMsg = -1, hlSent = -1;                   // the message + sentence the voice is on (-1 = none)
    function chatDecorate() {                       // idempotent: bubble class from the kind tag + md
      chatRowsEl.querySelectorAll('li').forEach(li => {
        const tag = ((li.querySelector('.l') || {}).textContent || '').trim();
        const cls = tag === 'you' ? 'you' : tag === 'think' ? 'think' : tag === 'err' ? 'err' : 'bot';
        if (li.className !== cls) li.className = cls;
        const v = li.querySelector('.v'); if (!v) return;
        if (v.dataset.raw === undefined) v.dataset.raw = v.textContent;
        const active = hlSent >= 0 && String(li.dataset.key) === String(hlMsg);
        const html = active ? chatMdSent(v.dataset.raw, hlSent) : chatMd(v.dataset.raw);
        if (v.innerHTML !== html) v.innerHTML = html;
      });
      if (chatAtBottom) chatBody.scrollTop = chatBody.scrollHeight;
    }
    const chat = makeWarpClient({
      app: 'chat',                                 // the label on this app's frames, both ways
      rows: chatRowsEl,
      viewportRows: 200,
      send: warpSend,                              // one link — the device manager's
      onStat: s => { chatStat.textContent = s.nodes + ' msgs · ' + s.bytes + ' B';
                     if (s.deltas) chatNote.textContent = ''; }
    });
    warpCh.addEventListener('message', e => {
      // a highlight tick ({a:chat, m, hl}) rides the same stream — peel it off before the warp
      // client, which would choke on a frame with no deltas.  hl>=0 lights sentence hl of message m.
      let d = null; try { d = JSON.parse(e.data); } catch (_) {}
      if (d && d.a === 'chat' && d.model !== undefined) {     // the box saying what it actually has
        curModel = d.model; chatSetNow.textContent = d.model;
        if (d.speaking !== undefined) chatSpeak.checked = !!d.speaking;
        if (d.effort !== undefined) { curEffort = d.effort; chatSetEffortNow.textContent = d.effort; }
        if (d.context !== undefined) { curCtx = d.context; chatSetCtxNow.textContent = d.context + 'k'; }
        paintModels(); paintEfforts(); paintCtx(); return;
      }
      if (d && d.a === 'chat' && d.dictate !== undefined) {   // live transcript -> the input box
        // ...but only while the box still holds exactly what dictation last put there.  The moment
        // you edit it (to fix a mishearing) it differs, and we leave your text alone.
        if (chatInput.value === lastDictate) { chatInput.value = d.dictate; lastDictate = d.dictate; }
        return;
      }
      if (d && d.a === 'chat' && d.hl !== undefined && d.gen === undefined) {
        hlMsg = (d.m === undefined ? -1 : d.m); hlSent = d.hl; chatDecorate(); return;
      }
      chat.apply(e.data); chatDecorate();
    });
    warpCh.addEventListener('close', () => {
      chat.reset(); chatSpoke = false; chatApp.served = null;
      chatStat.textContent = '—'; chatNote.textContent = 'channel closed';
    });
    chatPanel.querySelector('#chatForm').addEventListener('submit', e => {
      e.preventDefault();
      const t = chatInput.value.trim(); if (!t) return;
      chatInput.value = ''; lastDictate = ''; chatAtBottom = true;
      warpSend({ a: 'chat', say: t });             // gateway -> warp-chat:say -> the agent turn
      warpSend({ a: 'chat', clear: true });        // reset the ear so the sent text can't re-appear
      chatInput.focus();
    });

    let chatOn = false, chatSpoke = false;
    const chatApp = {
      id: 'chat', glyph: '\u{1F4AC}', name: 'operandi chat', served: null,
      // WITHOUT THIS, CHAT IS NEVER FULL SCREEN.  showApp() reads `to.panel` to measure
      // whether the panel would fill the view, and skips any app that does not name one --
      // so chat opened windowed on a phone while devices and files went edge to edge.
      panel: chatPanel,
      show: on => {
        chatOn = on;
        chatPanel.style.display = on ? 'flex' : 'none';
        if (!on) return;
        chat.viewport(200, 0);
        setTimeout(() => chatInput.focus(), 60);
        if (!chatSpoke) {
          chatSpoke = true;
          // First open of a gateway lifetime lazy-loads operandi (~10 s), so the note is patient
          // and the verdict is late; onStat clears it the instant a frame (the greeting) lands.
          chatNote.textContent = 'waking the agent… (first open takes a few seconds)';
          diag('warp chat: hello on stream 102, app chat');
          setTimeout(() => {
            if (chatOn && chat.stats().frames === 0) {
              chatNote.textContent = 'no answer — this box is not serving the chat (WARP_CHAT)';
              chatApp.served = false;
              diag('warp chat: no answer from the box');
            }
          }, 30000);
        }
      }
    };
    richApps.push(chatApp);
    // ==== END the operandi chat ===============================================================
}
