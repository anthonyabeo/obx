// Monaco integration + editor shim
const results     = document.getElementById('results');
const statusEl    = document.getElementById('statusText');
const cfgView     = document.getElementById('cfgView');
const cfgInner    = document.getElementById('cfgInner');
const cfgHolder   = document.getElementById('cfgPlaceholder');
const fnTabsEl    = document.getElementById('fnTabs');

const STORAGE_KEY = 'obx.playground.current';
let monacoEditor = null;
let pendingSource = null;
let pendingOnChange = null;

// editor shim used by the rest of the script
const editor = {
  getValue() { return monacoEditor ? monacoEditor.getValue() : (pendingSource || ''); },
  setValue(v) { if (monacoEditor) monacoEditor.setValue(v); else pendingSource = v; },
  onDidChange(cb) { if (monacoEditor) monacoEditor.onDidChangeModelContent(cb); else pendingOnChange = cb; }
};

function setButtonText(id, text) {
  const el = document.getElementById(id);
  if (!el) return;
  const span = el.querySelector('.btn-text');
  if (span) span.textContent = text;
  else el.setAttribute('aria-label', text);
}

// Load initial content from localStorage (single-file persistence)
function loadInitialSource() {
  try {
    const raw = localStorage.getItem(STORAGE_KEY);
    if (!raw) return null;
    const obj = JSON.parse(raw);
    if (obj && obj.source) {
      const fn = obj.filename || document.getElementById('filename').value || 'Main.obx';
      document.getElementById('filename').value = fn;
      return obj.source;
    }
  } catch (e) { /* ignore */ }
  return null;
}

function saveCurrentToStorage() {
  try {
    const fn = document.getElementById('filename').value.trim() || 'Main.obx';
    const src = editor.getValue();
    localStorage.setItem(STORAGE_KEY, JSON.stringify({ filename: fn, source: src }));
  } catch (e) { /* ignore */ }
}

// debounce helper
function debounce(fn, ms) {
  let t = null;
  return (...a) => { clearTimeout(t); t = setTimeout(() => fn(...a), ms); };
}

// Load Monaco from CDN
(function loadMonaco() {
  const loader = document.createElement('script');
  loader.src = 'https://cdnjs.cloudflare.com/ajax/libs/monaco-editor/0.33.0/min/vs/loader.js';
  loader.onload = () => {
    // eslint-disable-next-line no-undef
    require.config({ paths: { 'vs': 'https://cdnjs.cloudflare.com/ajax/libs/monaco-editor/0.33.0/min/vs' } });
    // eslint-disable-next-line no-undef
    require(['vs/editor/editor.main'], function() {
      const initial = loadInitialSource() || `MODULE Main;\n\nVAR x: INTEGER;\n\nBEGIN\n  x := 42\nEND Main.`;

      // Register Oberon+ language via external file if available
      try {
        if (window.registerOberonLanguage) {
          window.registerOberonLanguage(monaco);
        }
      } catch (e) {
        console.warn('Oberon language registration failed', e);
      }

      // define a small theme override so 'type' and hex/float numbers stand out
      try {
        monaco.editor.defineTheme('obx-dark', {
          base: 'vs-dark',
          inherit: true,
          rules: [
            { token: 'type', foreground: 'C792EA' },
            { token: 'number.hex', foreground: 'F78C6C' },
            { token: 'number.float', foreground: '89DDFF' },
            { token: 'number', foreground: 'F78C6C' }
          ],
          colors: {}
        });
      } catch (e) { /* ignore theme errors */ }

      monacoEditor = monaco.editor.create(document.getElementById('editor'), {
        value: initial,
        language: 'oberon',
        theme: 'obx-dark',
        automaticLayout: true,
        fontFamily: 'JetBrains Mono, Fira Code, ui-monospace, monospace',
        fontSize: 13,
        minimap: { enabled: false }
      });

      // wire change -> persist (debounced)
      monacoEditor.onDidChangeModelContent(debounce(() => saveCurrentToStorage(), 500));

      // expose pending content if any
      if (pendingSource) monacoEditor.setValue(pendingSource);
      if (pendingOnChange) monacoEditor.onDidChangeModelContent(pendingOnChange);

      // add keybinding: Cmd/Ctrl+Enter to run check
      monacoEditor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter, () => { runCheck(); });
    });
  };
  // Load the external Monarch grammar script (provides registerOberonLanguage)
  const monarchScript = document.createElement('script');
  monarchScript.src = '/static/js/oberon-monarch.js';
  document.head.appendChild(monarchScript);

  document.head.appendChild(loader);
})();

/* ── viz.js instance ───────────────────────── */
let viz = null;
try { viz = new Viz(); } catch(e) { console.warn('viz.js unavailable:', e); }

// Ensure panes are hidden on initial load (CSS already hides them, this
// guarantees consistent runtime state in all browsers).
const _diagPane = document.getElementById('diagPane'); if (_diagPane) _diagPane.classList.remove('open');
const _cfgPane = document.getElementById('cfgPane'); if (_cfgPane) _cfgPane.classList.remove('open');

/* ── version badge ─────────────────────────── */
fetch('/api/version')
  .then(r => r.ok ? r.json() : null)
  .then(d => { if (d) document.getElementById('versionBadge').textContent = `obx ${d.version}  ·  go ${d.go}`; })
  .catch(() => {});

/* ── run check (diagnostics only) ───────────── */
async function runCheck() {
  const source   = editor.getValue();
  const filename = document.getElementById('filename').value.trim() || 'Main.obx';
  const btn = document.getElementById('checkBtn');
  // Show diagnostics pane for check and hide CFG pane
  const diagPane = document.getElementById('diagPane');
  const cfgPane = document.getElementById('cfgPane');
  if (diagPane) diagPane.classList.add('open');
  if (cfgPane) cfgPane.classList.remove('open');
  btn.disabled = true; setButtonText('checkBtn', 'Checking…');
  results.innerHTML = ''; setStatus('idle', '…');

  const body = JSON.stringify({ source, filename });
  const resp = await fetch('/api/check', {method:'POST', headers:{'Content-Type':'application/json'}, body})
    .then(r => r.ok ? r.json() : r.text().then(t => { throw new Error('HTTP ' + r.status + ': ' + t.trim()); }))
    .catch(e => ({_err: e.message}));

  btn.disabled = false; setButtonText('checkBtn', 'Check');

  if (resp._err) {
    results.innerHTML = diagHtml('error','error', esc(resp._err), null);
    setStatus('fail', 'error');
  } else {
    renderDiags(resp);
    // Also print diagnostics to the terminal for Check
    printDiagsToTerminal(resp);
  }
}

/* ── CFG (on-demand) ───────────────────────── */
let cfgShown = false;
function toggleCfg() {
  if (!cfgShown) {
    showCfg();
  } else {
    // hide
    cfgInner.innerHTML = '';
    cfgHolder.style.display = 'flex';
    cfgHolder.innerHTML = '🔍&nbsp; Press <strong style="color:#4b5563">Show CFG</strong> to build';
    // hide the entire cfg pane
    const cfgPane = document.getElementById('cfgPane');
    if (cfgPane) cfgPane.classList.remove('open');
    cfgShown = false;
  }
}

async function showCfg() {
  const source   = editor.getValue();
  const filename = document.getElementById('filename').value.trim() || 'Main.obx';
  const cfgBtn = document.getElementById('cfgBtn');
  // Show CFG pane for CFG and hide diagnostics pane
  const cfgPane = document.getElementById('cfgPane');
  const diagPane = document.getElementById('diagPane');
  if (cfgPane) cfgPane.classList.add('open');
  if (diagPane) diagPane.classList.remove('open');
  cfgBtn.disabled = true; setButtonText('cfgBtn', 'Loading…');
  setCfgState('loading');

  const body = JSON.stringify({ source, filename });
  const resp = await fetch('/api/cfg', {method:'POST', headers:{'Content-Type':'application/json'}, body})
    .then(r => r.ok ? r.json() : r.text().then(t => { throw new Error('HTTP ' + r.status + ': ' + t.trim()); }))
    .catch(e => ({_err: e.message}));

  cfgBtn.disabled = false; setButtonText('cfgBtn', 'CFG');

  if (resp._err || !resp.ok) {
    setCfgState('error', resp._err || resp.error || 'CFG unavailable');
    cfgShown = false;
  } else {
    renderCFG(resp.graphs || []);
    cfgShown = true;
  }
}

/* ── Run (placeholder) ─────────────────────── */
async function runProgram() {
  const source   = editor.getValue();
  const filename = document.getElementById('filename').value.trim() || 'Main.obx';
  const btn = document.getElementById('runBtn');
  // hide diagnostic and cfg panes for Run
  const diagPane = document.getElementById('diagPane');
  const cfgPane = document.getElementById('cfgPane');
  if (diagPane) diagPane.classList.remove('open');
  if (cfgPane) cfgPane.classList.remove('open');
  btn.disabled = true; setButtonText('runBtn', 'Running…');
  appendTerminal('\n$ Running…\n');

  const body = JSON.stringify({ source, filename });
  const resp = await fetch('/api/run', {method:'POST', headers:{'Content-Type':'application/json'}, body})
    .then(r => r.ok ? r.json() : r.text().then(t => { throw new Error('HTTP ' + r.status + ': ' + t.trim()); }))
    .catch(e => ({_err: e.message}));

  btn.disabled = false; setButtonText('runBtn', 'Run');

  if (resp._err) {
    appendTerminal('\n[error] ' + resp._err + '\n');
  } else {
    appendTerminal('\n' + (resp.output || 'No output') + '\n');
    if (resp.diagnostics) {
      renderDiags(resp);
    }
  }
}

function clearTerminal() {
  const t = document.getElementById('terminal');
  t.textContent = '$';
}

function appendTerminal(s) {
  const t = document.getElementById('terminal');
  if (t.textContent === '$') t.textContent = '';
  t.textContent += s;
  t.scrollTop = t.scrollHeight;
}

/* ── file upload handling ─────────────────── */
// the visible file input was moved to a header button; we keep a hidden
// file input element that the upload button triggers.
let fileInput = document.getElementById('fileInput');
if (!fileInput) {
  fileInput = document.createElement('input');
  fileInput.type = 'file';
  fileInput.id = 'fileInput';
  fileInput.style.display = 'none';
  fileInput.title = 'Upload a .obx file';
  fileInput.setAttribute('aria-label','Upload file');
  document.body.appendChild(fileInput);
}

const uploadBtn = document.getElementById('uploadBtn');
if (uploadBtn) uploadBtn.addEventListener('click', () => fileInput.click());

fileInput.addEventListener('change', e => {
  const f = e.target.files && e.target.files[0];
  if (!f) return;
  const reader = new FileReader();
  reader.onload = ev => {
    editor.setValue(String(ev.target.result));
    document.getElementById('filename').value = f.name;
    // persist
    saveCurrentToStorage();
  };
  reader.readAsText(f);
});

/* ── diagnostics ───────────────────────────── */
function renderDiags(data) {
  const diags = data.diagnostics || [];
  if (!diags.length) {
    results.innerHTML = '<div class="empty">✅&nbsp; No diagnostics — looks clean</div>';
    return setStatus('ok', 'ok');
  }
  const counts = {};
  diags.forEach(d => { counts[d.severity] = (counts[d.severity]||0)+1; });
  const summary = Object.entries(counts).map(([s,n]) => `${n}\u202f${s}${n!==1?'s':''}`).join(' · ');
  setStatus(data.ok ? 'ok' : 'fail', summary);
  results.innerHTML = diags.map(d => {
    const loc = d.location
      ? `${esc(d.location.file)}:${d.location.start_line}:${d.location.start_col}` : null;
    return diagHtml(d.severity||'info', d.severity||'info', esc(d.message||''), loc);
  }).join('');
}

// Print diagnostics to the terminal (used for Check only)
function printDiagsToTerminal(data) {
  const diags = data.diagnostics || [];
  if (!diags.length) return;
  try {
    const counts = {};
    diags.forEach(d => { counts[d.severity] = (counts[d.severity]||0)+1; });
    const summary = Object.entries(counts).map(([s,n]) => `${n}
${s}${n!==1?'s':''}`).join(' · ');
    const termLines = [];
    termLines.push('\n[diagnostics] ' + summary + '\n');
    diags.forEach(d => {
      const loc = d.location ? `${d.location.file}:${d.location.start_line}:${d.location.start_col}` : '';
      termLines.push(`${(d.severity||'info').toUpperCase()}: ${d.message}${loc ? ' ('+loc+')' : ''}`);
    });
    appendTerminal(termLines.join('\n') + '\n');
  } catch (e) { /* ignore terminal errors */ }
}

function diagHtml(cls, sev, msg, loc) {
  const locRow = loc ? `<div class="diag-loc">${loc}</div>` : '';
  return `<div class="diag-item ${esc(cls)}"><div class="diag-row">
    <span class="badge ${esc(cls)}">${esc(sev)}</span>
    <span class="diag-msg">${msg}</span></div>${locRow}</div>`;
}

function setStatus(cls, text) { statusEl.className='status '+cls; statusEl.textContent=text; }

/* ── CFG helpers ───────────────────────────── */
function setCfgState(kind, msg) {
  cfgInner.innerHTML = '';
  cfgHolder.style.display = 'flex';
  fnTabsEl.innerHTML = '';
  resetPanZoom();
  if (kind === 'loading') { cfgHolder.innerHTML = '⌛&nbsp; Building CFG…'; return; }
  if (kind === 'error')   { cfgHolder.innerHTML = '⚠️&nbsp; ' + esc(msg); return; }
  cfgHolder.innerHTML = '🔍&nbsp; Press <strong style="color:#4b5563">Check</strong> to see the CFG';
}

let allGraphs = [], activeIdx = 0;

function renderCFG(graphs) {
  allGraphs = graphs; activeIdx = 0;
  buildTabs();
  if (!graphs.length) { setCfgState('error', 'no functions found'); return; }
  showGraph(0);
}

function buildTabs() {
  fnTabsEl.innerHTML = '';
  allGraphs.forEach((g, i) => {
    const b = document.createElement('button');
    b.className = 'fn-tab' + (i===activeIdx ? ' active' : '');
    b.textContent = g.function;
    b.title = g.module + '.' + g.function;
    b.onclick = () => { activeIdx=i; buildTabs(); showGraph(i); };
    fnTabsEl.appendChild(b);
  });
}

async function showGraph(idx) {
  cfgHolder.style.display = 'none';
  cfgInner.innerHTML = '';
  resetPanZoom();

  if (!viz) {
    cfgHolder.style.display = 'flex';
    cfgHolder.innerHTML = '⚠️&nbsp; viz.js renderer not loaded (check network)';
    return;
  }
  try {
    const svg = await viz.renderSVGElement(allGraphs[idx].dot);
    // Keep the SVG's original pt-based width/height so the browser
    // gives it a proper intrinsic size; do NOT remove those attributes.
    cfgInner.appendChild(svg);
    // Wait two animation frames so the browser has finished layout
    // before we measure getBoundingClientRect.
    requestAnimationFrame(() => requestAnimationFrame(fitCfg));
  } catch (err) {
    viz = new Viz();
    cfgHolder.style.display = 'flex';
    cfgHolder.innerHTML = '⚠️&nbsp; Render error: ' + esc(err.message);
  }
}

/* ── Zoom / pan ────────────────────────────── */
let scale=1, tx=0, ty=0, dragging=false, dragX=0, dragY=0;

function applyTransform() {
  cfgInner.style.transform = `translate(${tx}px,${ty}px) scale(${scale})`;
}
function resetPanZoom() { scale=1; tx=0; ty=0; cfgInner.style.transform=''; }

function resetCfg() { scale=1; tx=0; ty=0; applyTransform(); }

function fitCfg() {
  const svg = cfgInner.querySelector('svg');
  if (!svg) return;
  // Read the actual rendered CSS-pixel size.  getBoundingClientRect() accounts
  // for the pt→px conversion the browser applies to the SVG's width/height
  // attributes, which is what we need for scale calculations.
  // Temporarily clear any existing transform so measurements are unscaled.
  const saved = cfgInner.style.transform;
  cfgInner.style.transform = '';
  const r = svg.getBoundingClientRect();
  const gw = r.width, gh = r.height;
  cfgInner.style.transform = saved;

  if (!gw || !gh) return;
  const pad = 24;
  const cw = cfgView.clientWidth  - pad;
  const ch = cfgView.clientHeight - pad;
  scale = Math.min(cw / gw, ch / gh, 1);
  tx = (cw - gw * scale) / 2 + pad / 2;
  ty = pad / 2;
  applyTransform();
}

/* wheel → zoom toward cursor */
cfgView.addEventListener('wheel', e => {
  e.preventDefault();
  const factor = e.deltaY < 0 ? 1.12 : 1/1.12;
  const rect = cfgView.getBoundingClientRect();
  const mx = e.clientX - rect.left, my = e.clientY - rect.top;
  tx = mx - (mx - tx)*factor;
  ty = my - (my - ty)*factor;
  scale = Math.max(0.05, Math.min(10, scale*factor));
  applyTransform();
}, {passive: false});

/* drag → pan */
cfgView.addEventListener('mousedown', e => {
  if (e.button !== 0) return;
  dragging=true; dragX=e.clientX-tx; dragY=e.clientY-ty;
  cfgView.classList.add('dragging');
});
document.addEventListener('mousemove', e => {
  if (!dragging) return;
  tx=e.clientX-dragX; ty=e.clientY-dragY; applyTransform();
});
document.addEventListener('mouseup', () => { dragging=false; cfgView.classList.remove('dragging'); });

/* double-click → fit */
cfgView.addEventListener('dblclick', fitCfg);

/* ── keyboard shortcut ─────────────────────── */
document.addEventListener('keydown', e => {
  if ((e.metaKey||e.ctrlKey) && e.key==='Enter') { e.preventDefault(); runCheck(); }
});

function esc(s) {
  return String(s??'').replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/"/g,'&quot;');
}

