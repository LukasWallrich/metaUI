// metaUI static-hosting bridge, appended to www/metaui.js by export_static(). GPL-3-or-later.
// Static hosting (shinylive): the app runs in a same-origin iframe whose URL lacks
// the page's query string. Mirror selection links between the page and the app.
// Inert when the app is served by a Shiny server.
(function () {
  if (window.parent === window || !/\/app_[^/]+\/$/.test(location.pathname)) return;
  var page;
  try { page = window.parent.location; page.search; } catch (e) { return; }
  // Restore: hand the page's ?metaui= query to Shiny before it connects.
  if (!location.search && /[?&]metaui=1(&|$)/.test(page.search))
    history.replaceState(null, '', location.pathname + page.search);
  // Publish: updateQueryString() changes the iframe URL; copy it to the page.
  ['replaceState', 'pushState'].forEach(function (method) {
    var original = history[method].bind(history);
    history[method] = function (state, title, url) {
      original(state, title, url);
      try { window.parent.history.replaceState(null, '', page.pathname + location.search + page.hash); } catch (e) {}
    };
  });
  // Share links are relative ("?metaui=..."); point them at the page, not the iframe.
  function fixLinks() {
    document.querySelectorAll('.metaui-share a[href^="?"]').forEach(function (a) {
      a.href = page.origin + page.pathname + a.getAttribute('href');
      a.target = '_top';
    });
  }
  // Downloads: fetch through the service worker and save a blob, rather than relying
  // on the browser to route an <a download> navigation through the service worker.
  document.addEventListener('click', function (event) {
    var link = event.target.closest && event.target.closest('a.shiny-download-link');
    if (!link || link.classList.contains('disabled')) return;
    event.preventDefault(); event.stopImmediatePropagation();
    fetch(link.href).then(function (response) {
      if (!response.ok) throw new Error('HTTP ' + response.status);
      var header = response.headers.get('Content-Disposition') || '';
      var match = /filename\*?=(?:UTF-8'')?"?([^";]+)"?/i.exec(header);
      return response.blob().then(function (blob) {
        var a = document.createElement('a');
        a.href = URL.createObjectURL(blob);
        a.download = match ? decodeURIComponent(match[1]) : 'download';
        document.body.appendChild(a); a.click(); a.remove();
        setTimeout(function () { URL.revokeObjectURL(a.href); }, 60000);
      });
    }).catch(function (error) { alert('Download failed: ' + error.message); });
  }, true);
  document.addEventListener('DOMContentLoaded', function () {
    new MutationObserver(fixLinks).observe(document.body, {childList: true, subtree: true});
  });
})();

// Complete the initial analysis behind the export-time Summary before hand-over.
(function () {
  if (window.parent === window || !/\/app_[^/]+\/$/.test(location.pathname)) return;
  try { if (!window.parent.metauiPrecomputed) return; } catch (e) { return; }
  var touched = false, started = false;
  ['pointerdown', 'keydown', 'input', 'change'].forEach(function (type) {
    document.addEventListener(type, function (event) { if (event.isTrusted) touched = true; }, true);
  });
  var timer = setInterval(function () {
    var app = window.Shiny && Shiny.shinyapp, go = document.getElementById('go');
    if (!app || !app.isConnected() || !go || go.disabled ||
        !app.$inputValues || !Array.isArray(app.$inputValues.outliers_z_scores)) return;
    var selection = new URLSearchParams(location.search);
    if (!started) {
      started = true;
      if (touched || app.$inputValues.uploadData || (app.$inputValues.go || app.$inputValues['go:shiny.action']) > 0 ||
          (selection.get('metaui') === '1' && selection.has('selection'))) {
        clearInterval(timer); window.parent.postMessage('metaui-live-ready', location.origin); return;
      }
      go.click();
    }
    var table = document.getElementById('effectestimate');
    var primary = document.getElementById('primary_estimate');
    if (table && table.textContent.trim() && primary && primary.textContent.trim() &&
        !document.documentElement.classList.contains('shiny-busy')) {
      clearInterval(timer); window.parent.postMessage('metaui-live-ready', location.origin);
    }
  }, 100);
})();
