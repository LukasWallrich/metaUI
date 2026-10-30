// Runs before webR starts; never show full-data results for a linked selection.
(function () {
  var query = new URLSearchParams(location.search);
  if (query.get('metaui') === '1' && query.has('selection')) return;
  window.metauiPrecomputed = true;
  var style = document.createElement('style');
  style.textContent = '#root{position:fixed!important;inset:0;opacity:0;pointer-events:none}#metaui-preview{position:fixed;inset:0;width:100%;height:100%;border:0;background:white}';
  document.head.appendChild(style);
  var preview = document.createElement('iframe');
  preview.id = 'metaui-preview'; preview.title = 'Precomputed full-dataset results; interactive app loading';
  preview.src = 'metaui-preview/index.html';
  document.body.appendChild(preview);
  var status, finished = false;
  var timeout = Number(document.documentElement.dataset.metauiStartupTimeout) || 180000;
  function unavailable() {
    if (finished || status) return;
    preview.title = 'Precomputed full-dataset results; interactive app unavailable';
    status = document.createElement('p');
    status.id = 'metaui-startup-status'; status.setAttribute('role', 'status');
    status.setAttribute('aria-live', 'polite');
    status.style.cssText = 'position:fixed;bottom:0;left:0;right:0;margin:0;padding:0.6em 1em;background:#fff8e5;color:#333;border-top:1px solid #ddd;z-index:10';
    status.textContent = 'The interactive version could not start. The precomputed Summary remains available. Try reloading in a modern desktop browser.';
    document.body.appendChild(status);
  }
  var timer = setTimeout(unavailable, timeout);
  document.addEventListener('DOMContentLoaded', function () { document.getElementById('root').inert = true; });
  window.addEventListener('message', function (event) {
    var root = document.getElementById('root');
    var live = root && root.querySelector('iframe');
    if (!live || event.source !== live.contentWindow || event.origin !== location.origin) return;
    if (event.data === 'metaui-startup-failed') { unavailable(); return; }
    if (event.data !== 'metaui-live-ready') return;
    finished = true; clearTimeout(timer);
    if (status) status.remove();
    // Match the scroll position where possible; avoid stealing focus from readers.
    try { live.contentWindow.scrollTo(0, preview.contentWindow.scrollY); } catch (e) {}
    var focused = document.activeElement === preview;
    root.inert = false; style.remove(); preview.remove();
    document.documentElement.dataset.metauiLive = 'true';
    if (focused) live.focus();
  });
})();
