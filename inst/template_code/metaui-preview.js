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
  document.addEventListener('DOMContentLoaded', function () { document.getElementById('root').inert = true; });
  window.addEventListener('message', function (event) {
    var root = document.getElementById('root');
    var live = root && root.querySelector('iframe');
    if (!live || event.source !== live.contentWindow || event.origin !== location.origin || event.data !== 'metaui-live-ready') return;
    // Match the scroll position where possible; avoid stealing focus from readers.
    try { live.contentWindow.scrollTo(0, preview.contentWindow.scrollY); } catch (e) {}
    var focused = document.activeElement === preview;
    root.inert = false; style.remove(); preview.remove();
    document.documentElement.dataset.metauiLive = 'true';
    if (focused) live.focus();
  });
})();
