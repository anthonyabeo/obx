// Monaco integration + editor shim
const results     = document.getElementById('results');
const statusEl    = document.getElementById('statusText');
const cfgView     = document.getElementById('cfgView');
const cfgInner    = document.getElementById('cfgInner');
const cfgHolder   = document.getElementById('cfgPlaceholder');
const fnTabsEl    = document.getElementById('fnTabs');
const minirView      = document.getElementById('minirView');
const minirTextEl    = document.getElementById('minirText');
const minirModTabsEl = document.getElementById('minirModTabs');
const cfgControls    = document.getElementById('cfgControls');

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
  // Load the vendored Monaco loader from the embedded static assets. Place
  // the monaco `min/vs` tree under /cmd/web/static/js/monaco/vs so it is
  // served at /static/js/monaco/vs/loader.js by the server's static handler.
  loader.src = '/static/js/monaco/vs/loader.js';
  loader.onload = () => {
    // eslint-disable-next-line no-undef
    // point require to the local vendored monaco 'vs' path (now under /static/js)
    require.config({ paths: { 'vs': 'static/js/monaco/vs' } });

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

/* ── IR format state ───────────────────────── */
// 'graph' → CFG SVG (existing behaviour)   'text' → textual minir via /api/minir
let irFormat = 'graph';

/**
 * setIrFormat switches the active format in the IR viewer pane.
 * Exposed on window so the inline nonce script in index.html can call it.
 */
window.setIrFormat = function setIrFormat(fmt) {
  irFormat = fmt;

  // update toggle button visual state
  const gBtn = document.getElementById('irFmtGraph');
  const tBtn = document.getElementById('irFmtText');
  if (gBtn) gBtn.classList.toggle('active', fmt === 'graph');
  if (tBtn) tBtn.classList.toggle('active', fmt === 'text');

  // show / hide the sub-views and per-view controls
  if (cfgView)     cfgView.style.display     = fmt === 'graph' ? '' : 'none';
  if (minirView)   minirView.style.display   = fmt === 'text'  ? 'flex' : 'none';
  if (cfgControls) cfgControls.style.display = fmt === 'graph' ? '' : 'none';
  if (fnTabsEl)    fnTabsEl.style.display    = fmt === 'graph' ? '' : 'none';

  // if the pane is already open, refresh the active view immediately
  const cfgPane = document.getElementById('cfgPane');
  if (cfgPane && cfgPane.classList.contains('open')) {
    if (fmt === 'graph') showCfgGraph();
    else                 showMinir();
  }
};

// Apply initial display state (graph is default — minir view hidden)
if (minirView)   minirView.style.display   = 'none';
if (cfgControls) cfgControls.style.display = '';

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
window.runCheck = runCheck;

/* ── CFG / IR pane toggle ──────────────────── */
let cfgShown = false;
function toggleCfg() {
  if (!cfgShown) {
    if (irFormat === 'text') showMinir();
    else                     showCfgGraph();
  } else {
    // collapse
    cfgInner.innerHTML = '';
    cfgHolder.style.display = 'flex';
    cfgHolder.innerHTML = '🔍&nbsp; Press <strong style="color:#4b5563">Show CFG</strong> to build';
    clearMinirView();
    const cfgPane = document.getElementById('cfgPane');
    if (cfgPane) cfgPane.classList.remove('open');
    cfgShown = false;
  }
}
window.toggleCfg = toggleCfg;

/* ── Graph format (CFG SVG) ─────────────────── */
async function showCfgGraph() {
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
window.showCfg = showCfgGraph;

/* ── Text format (textual minir) ────────────── */
let allMinirModules = [], activeMinirIdx = 0;

async function showMinir() {
  const source   = editor.getValue();
  const filename = document.getElementById('filename').value.trim() || 'Main.obx';
  const cfgBtn  = document.getElementById('cfgBtn');
  const cfgPane = document.getElementById('cfgPane');
  const diagPane = document.getElementById('diagPane');
  if (cfgPane)  cfgPane.classList.add('open');
  if (diagPane) diagPane.classList.remove('open');
  cfgBtn.disabled = true; setButtonText('cfgBtn', 'Loading…');
  setMinirState('loading');

  const body = JSON.stringify({ source, filename });
  const resp = await fetch('/api/minir', {method:'POST', headers:{'Content-Type':'application/json'}, body})
    .then(r => r.ok ? r.json() : r.text().then(t => { throw new Error('HTTP ' + r.status + ': ' + t.trim()); }))
    .catch(e => ({_err: e.message}));

  cfgBtn.disabled = false; setButtonText('cfgBtn', 'CFG');

  if (resp._err || !resp.ok) {
    setMinirState('error', resp._err || resp.error || 'minir unavailable');
    cfgShown = false;
  } else {
    renderMinirModules(resp.modules || []);
    cfgShown = true;
  }
}

function renderMinirModules(modules) {
  allMinirModules = modules;
  activeMinirIdx  = 0;
  buildMinirTabs();
  if (!modules.length) { setMinirState('error', 'no modules found'); return; }
  showMinirModule(0);
}

function buildMinirTabs() {
  if (!minirModTabsEl) return;
  minirModTabsEl.innerHTML = '';
  allMinirModules.forEach((m, i) => {
    const b = document.createElement('button');
    b.className = 'fn-tab' + (i === activeMinirIdx ? ' active' : '');
    b.textContent = m.name || `module ${i}`;
    b.title = m.name;
    b.onclick = () => { activeMinirIdx = i; buildMinirTabs(); showMinirModule(i); };
    minirModTabsEl.appendChild(b);
  });
}

function showMinirModule(idx) {
  if (!minirTextEl) return;
  const m = allMinirModules[idx];
  if (!m) { minirTextEl.textContent = '(no output)'; return; }
  // Replace placeholder HTML with plain text content
  minirTextEl.innerHTML = '';
  minirTextEl.textContent = m.ir || '(empty)';
}

function setMinirState(kind, msg) {
  if (!minirTextEl) return;
  if (minirModTabsEl) minirModTabsEl.innerHTML = '';
  if (kind === 'loading') {
    minirTextEl.innerHTML = '<span class="minir-placeholder">⌛&nbsp; Lowering to minir…</span>';
    return;
  }
  if (kind === 'error') {
    minirTextEl.innerHTML = `<span class="minir-placeholder" style="color:#f87171">⚠️&nbsp; ${esc(msg)}</span>`;
    return;
  }
  minirTextEl.innerHTML = '<span class="minir-placeholder">🔍&nbsp; Switch to <strong>Text</strong> and press the IR button</span>';
}

function clearMinirView() {
  allMinirModules = [];
  setMinirState('idle');
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
window.runProgram = runProgram;

function clearTerminal() {
  const t = document.getElementById('terminal');
  t.textContent = '$';
}
window.clearTerminal = clearTerminal;

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
    const summary = Object.entries(counts).map(([s,n]) => `${n}\u202f${s}${n!==1?'s':''}`).join(' · ');
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

  const g = allGraphs[idx];

  // ── path 1: server-rendered SVG (bypasses viz.js / Graphviz 2.40.1 bugs) ──
  if (g && g.svg) {
    cfgInner.innerHTML = g.svg;
    requestAnimationFrame(() => requestAnimationFrame(fitCfg));
    return;
  }

  // ── path 2: client-side viz.js fallback (dot binary not available on server) ──
  if (!viz) {
    cfgHolder.style.display = 'flex';
    cfgHolder.innerHTML = '⚠️&nbsp; viz.js renderer not loaded (check network)';
    return;
  }
  try {
    const svg = await viz.renderSVGElement(g.dot);
    // Keep the SVG's original pt-based width/height so the browser
    // gives it a proper intrinsic size; do NOT remove those attributes.
    cfgInner.appendChild(svg);
    // Wait two animation frames so the browser has finished layout
    // before we measure getBoundingClientRect.
    requestAnimationFrame(() => requestAnimationFrame(fitCfg));
  } catch (err) {
    viz = new Viz();
    // Log the failing DOT to the browser console so it can be inspected.
    const failingDot = g ? g.dot : '(none)';
    console.error('[obx CFG] Render error for function', g && g.function, '\nError:', err.message, '\nDOT:\n', failingDot);
    cfgHolder.style.display = 'flex';
    cfgHolder.innerHTML = '⚠️&nbsp; Render error: ' + esc(err.message) +
      '<br><small style="color:#6b7280">Check browser DevTools console (F12) for the DOT source.</small>';
  }
}

/* ── Zoom / pan ────────────────────────────── */
let scale=1, tx=0, ty=0, dragging=false, dragX=0, dragY=0;

function applyTransform() {
  cfgInner.style.transform = `translate(${tx}px,${ty}px) scale(${scale})`;
}
function resetPanZoom() { scale=1; tx=0; ty=0; cfgInner.style.transform=''; }

function resetCfg() { scale=1; tx=0; ty=0; applyTransform(); }
window.resetCfg = resetCfg;

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
window.fitCfg = fitCfg;

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

