// metaUI app behaviour. Copied into each generated app's www/ folder; edit freely.
// Slider grid labels can still collide in narrow sidebars or after uploads change a
// slider's range. Hide any label that would overlap a neighbour, keeping both ends.
(function () {
  function declutter() {
    // The chosen range is written beside the filter name; the scale ends stay under the track.
    document.documentElement.classList.add('metaui-js');
    document.querySelectorAll('.irs').forEach(function (slider) {
      var from = slider.querySelector('.irs-from'), to = slider.querySelector('.irs-to');
      var container = slider.closest('.shiny-input-container');
      var label = container && container.querySelector('.control-label');
      if (!from || !to || !label) return;
      var range = label.querySelector('.metaui-range');
      if (!range) { range = document.createElement('span'); range.className = 'metaui-range'; label.appendChild(range); }
      range.textContent = from.textContent + ' to ' + to.textContent;
    });
    document.querySelectorAll('.irs').forEach(function (slider) {
      var labels = Array.prototype.slice.call(slider.querySelectorAll(':scope > .irs-grid > .irs-grid-text'));
      if (labels.length < 3) return;
      labels.forEach(function (label) { label.style.visibility = ''; });
      var boxes = labels.map(function (label) { return label.getBoundingClientRect(); });
      if (!boxes[0].width) return; // not rendered yet (hidden)
      var gap = 4, last = labels.length - 1;
      // Keep every k-th label (plus the last) for the smallest k without collisions,
      // so the remaining scale stays evenly spaced.
      for (var k = 1; k <= last; k++) {
        var keep = [];
        for (var i = 0; i < last; i += k) keep.push(i);
        if (keep[keep.length - 1] !== last) {
          if (boxes[last].left < boxes[keep[keep.length - 1]].right + gap) keep.pop();
          keep.push(last);
        }
        var fits = keep.every(function (index, j) {
          return j === 0 || boxes[index].left >= boxes[keep[j - 1]].right + gap;
        });
        if (fits) {
          labels.forEach(function (label, index) {
            if (keep.indexOf(index) < 0) label.style.visibility = 'hidden';
          });
          return;
        }
      }
    });
  }
  var timer;
  function schedule() { clearTimeout(timer); timer = setTimeout(declutter, 60); }
  window.addEventListener('resize', schedule);
  document.addEventListener('DOMContentLoaded', function () {
    new MutationObserver(function (mutations) {
      for (var i = 0; i < mutations.length; i++) {
        var target = mutations[i].target;
        if (target.closest && target.closest('.irs, .shiny-input-container')) { schedule(); return; }
      }
    }).observe(document.body, { childList: true, subtree: true });
    schedule();
  });
})();

/* Wide screens: the filters stay in view down long tabs, in the height left under the header. */
(function () {
  function fit() {
    var form = document.querySelector('form.well');
    if (!form || !form.parentElement) return;
    var top = Math.max(0, form.parentElement.getBoundingClientRect().top);
    form.style.setProperty('--metaui-sidebar-height', (window.innerHeight - top) + 'px');
  }
  window.addEventListener('scroll', fit, { passive: true });
  window.addEventListener('resize', fit);
  document.addEventListener('DOMContentLoaded', fit);
})();

/* Expandable author-written filter help; independent of Bootstrap version. */
(function ($) {
  'use strict';
  function closeHelp(button) {
    button.setAttribute('aria-expanded', 'false');
    var panel = document.getElementById(button.getAttribute('aria-controls'));
    if (panel) panel.hidden = true;
  }
  $(document).on('click', '.metaui-filter-help', function (event) {
    event.preventDefault();
    var panel = document.getElementById(this.getAttribute('aria-controls'));
    if (!panel) return;
    var expanded = this.getAttribute('aria-expanded') === 'true';
    this.setAttribute('aria-expanded', String(!expanded));
    panel.hidden = expanded;
  });
  $(document).on('keydown', function (event) {
    if (event.key === 'Escape') {
      document.querySelectorAll('.metaui-filter-help[aria-expanded="true"]').forEach(closeHelp);
    }
  });
})(jQuery);

/* Study details from the authors' build-time study information, shown as a
   DataTables child row under the effect. The toggle is a native button; DT selects
   rows on mousedown, so that event is stopped (in the capture phase) for the toggle. */
(function ($) {
  'use strict';
  var counter = 0;
  // DataTables selects rows on mousedown; study toggles and the details below them must not.
  document.addEventListener('mousedown', function (event) {
    if (event.target.closest && event.target.closest('.metaui-study-toggle, .metaui-study-child')) event.stopPropagation();
  }, true);
  $(document).on('click', '.metaui-study-toggle', function () {
    var button = this;
    var tr = $(button).closest('tr');
    var row = tr.closest('table').DataTable().row(tr);
    if (row.child.isShown()) {
      row.child.hide();
      button.setAttribute('aria-expanded', 'false');
      return;
    }
    var data = row.data();
    var id = button.getAttribute('aria-controls') || 'metaui-study-' + (++counter);
    row.child('<div id="' + id + '" role="region" aria-label="Study details">' + data[data.length - 1] + '</div>',
      'metaui-study-child').show();
    button.setAttribute('aria-controls', id);
    button.setAttribute('aria-expanded', 'true');
  });

  // Copies the reference as rich text (italics, link) with a plain-text alternative. The copy
  // event works without a secure context, which the async clipboard API requires.
  function copyReference(html, text) {
    var copied = false;
    var listener = function (event) {
      event.clipboardData.setData('text/html', html);
      event.clipboardData.setData('text/plain', text);
      event.preventDefault();
      copied = true;
    };
    document.addEventListener('copy', listener);
    try { document.execCommand('copy'); } catch (error) {}
    document.removeEventListener('copy', listener);
    return copied;
  }
  $(document).on('click', '.metaui-copy-ref', function () {
    var button = this;
    var cite = $(button).closest('.metaui-study-ref').find('.metaui-study-cite')[0];
    var status = $(button).siblings('.metaui-copy-status')[0];
    var text = cite.textContent.replace(/\s+/g, ' ').trim();
    var report = function (ok) {
      status.textContent = ok ? 'Reference copied' : 'Could not copy; select the reference instead';
      clearTimeout(button.metauiCopyTimer);
      button.metauiCopyTimer = setTimeout(function () { status.textContent = ''; }, 3000);
    };
    if (copyReference(cite.innerHTML, text)) return report(true);
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(text).then(function () { report(true); }, function () { report(false); });
    } else report(false);
  });
})(jQuery);

/* Phones: the filters collapse behind a toggle so that results follow closely, and
   updating the results collapses them again and moves to the results. metaui.css hides the
   toggle and keeps the filters open on wider screens. */
(function ($) {
  'use strict';
  var phone = window.matchMedia('(max-width: 767px)');
  function setOpen(open) {
    var toggle = document.querySelector('.metaui-filters-toggle');
    if (!toggle) return;
    toggle.setAttribute('aria-expanded', String(open));
    toggle.textContent = open ? 'Hide filters and downloads' : 'Filters and downloads';
    document.getElementById(toggle.getAttribute('aria-controls')).classList.toggle('metaui-open', open);
  }
  $(document).on('click', '.metaui-filters-toggle', function () {
    setOpen(this.getAttribute('aria-expanded') !== 'true');
  });
  $(document).on('click', '#go', function (event, detail) {
    if (!phone.matches || (detail && detail.auto)) return;
    setOpen(false);
    var results = document.getElementById('metaui_results');
    if (!results) return;
    // Re-rendering outputs briefly collapse the results, and the browser then resets the
    // scroll position. Hold their height and keep them in view until the analysis has
    // settled, unless the reader moves first.
    results.style.minHeight = Math.max(results.offsetHeight, window.innerHeight) + 'px';
    var show = function () { results.scrollIntoView({ block: 'start' }); };
    var done = false;
    var watch = function () {
      if (!done && Math.abs(results.getBoundingClientRect().top) > 40) window.requestAnimationFrame(show);
    };
    var finish = function () {
      if (done) return;
      done = true;
      results.style.minHeight = '';
      $(window).off('scroll', watch).off('touchstart wheel keydown', finish);
    };
    $(window).on('scroll', watch).one('touchstart wheel keydown', finish);
    setTimeout(function () { if (!done) show(); }, 50);
    $(document).one('shiny:idle', function () {
      setTimeout(function () { if (!done) show(); setTimeout(finish, 300); }, 150);
    });
    setTimeout(finish, 20000);
  });
})(jQuery);

/* The empty state's reset link uses the panel's Reset button. */
(function ($) {
  'use strict';
  $(document).on('click', '.metaui-reset-link', function () { document.getElementById('resetFilters').click(); });
})(jQuery);

/* Forest plot: a study's combined row opens to show its effects. */
(function ($) {
  'use strict';
  function setStudy(row, open) {
    var button = row.querySelector('.metaui-forest-toggle');
    if (button) button.setAttribute('aria-expanded', String(open));
    for (var next = row.nextElementSibling; next && next.classList.contains('metaui-forest-effect'); next = next.nextElementSibling)
      next.classList.toggle('metaui-forest-closed', !open);
  }
  $(document).on('click', '.metaui-forest-toggle', function () {
    setStudy(this.closest('tr'), this.getAttribute('aria-expanded') !== 'true');
  });
  // Effects under each moderator level: no dot covers another, at whatever width the row has.
  var phone = window.matchMedia('(max-width: 767px)');
  function spreadDots(again) {
    var moved = false, anyBars = false;
    document.querySelectorAll('.metaui-moderators').forEach(function (table) {
      var tracks = Array.prototype.slice.call(table.querySelectorAll('tbody .metaui-track'));
      var dotCount = function (track) { return track.querySelectorAll('.metaui-track-dot').length; };
      // On phones a level with many effects would run very tall, so it becomes a histogram.
      var crowded = function (track) { return phone.matches && dotCount(track) >= 25; };
      var binsOf = function (track, width) {
        var bins = {};
        track.querySelectorAll('.metaui-track-dot').forEach(function (dot) {
          var bin = Math.floor(parseFloat(dot.style.left) / 100 * width / 4);
          bins[bin] = (bins[bin] || 0) + 1;
        });
        return bins;
      };
      // Bars share one scale, so that their heights compare across levels.
      var tallestBin = 1;
      tracks.filter(crowded).forEach(function (track) {
        var bins = binsOf(track, track.clientWidth);
        Object.keys(bins).forEach(function (bin) { tallestBin = Math.max(tallestBin, bins[bin]); });
      });
      if (tracks.some(crowded)) anyBars = true;
      // One shrink per table, so a dot's size means the same in every row.
      var largest = Math.max.apply(null, tracks.filter(function (track) { return !crowded(track); }).map(dotCount).concat(1));
      var crowd = Math.max(.75, Math.min(1, Math.sqrt(60 / Math.max(largest, 1))));
      tracks.forEach(function (track) {
        var width = track.clientWidth;
        if (!width || track.dataset.spreadWidth === String(width)) return;
        track.dataset.spreadWidth = width;
        var oldBars = track.querySelector('.metaui-track-hist');
        if (oldBars) oldBars.remove();
        var markY = parseFloat(getComputedStyle(track).getPropertyValue('--metaui-mark-y')) || 19;
        if (crowded(track)) {
          var bins = binsOf(track, width), hist = document.createElement('div');
          hist.className = 'metaui-track-hist';
          hist.style.top = (markY + 9) + 'px';
          Object.keys(bins).forEach(function (bin) {
            var bar = document.createElement('i');
            bar.className = 'metaui-track-bar';
            bar.dataset.count = bins[bin];
            bar.style.left = (bin * 4) + 'px';
            bar.style.height = Math.max(2, bins[bin] / tallestBin * 30) + 'px';
            hist.appendChild(bar);
          });
          track.appendChild(hist);
          track.querySelectorAll('.metaui-track-dot').forEach(function (dot) { dot.style.display = 'none'; });
          track.style.minHeight = track.closest('tr').style.minHeight = (markY + 42) + 'px';
          if (track.clientWidth !== width) moved = true;
          return;
        }
        track.querySelectorAll('.metaui-track-dot').forEach(function (dot) { dot.style.display = ''; });
        var shrink = crowd * (width < 220 ? .75 : 1), placed = [], lowest = 0;
        var dots = Array.prototype.map.call(track.querySelectorAll('.metaui-track-dot'), function (dot) {
          if (!dot.dataset.size) dot.dataset.size = parseFloat(dot.style.width) || 6;
          return { el: dot, x: parseFloat(dot.style.left) / 100 * width, d: dot.dataset.size * shrink };
        }).sort(function (a, b) { return b.d - a.d || a.x - b.x; });
        dots.forEach(function (dot) {
          var y = 0;
          var clash = function (other) { return Math.hypot(other.x - dot.x, other.y - y) < (other.d + dot.d) / 2 + 1; };
          while (placed.some(clash)) y += .5;
          dot.y = y;
          placed.push(dot);
          lowest = Math.max(lowest, y + dot.d);
          dot.el.style.width = dot.el.style.height = dot.d + 'px';
          dot.el.style.margin = (-dot.d / 2) + 'px 0 0 ' + (-dot.d / 2) + 'px';
          dot.el.style.transform = 'translateY(' + (9 + dot.d / 2 + y) + 'px)';
        });
        var height = markY + 9 + lowest + 3;
        track.style.minHeight = height + 'px';
        track.closest('tr').style.minHeight = height + 'px';
        // Taller rows can bring in a scrollbar, which narrows the track.
        if (track.clientWidth !== width) moved = true;
      });
    });
    document.querySelectorAll('.metaui-hist-note').forEach(function (note) { note.hidden = !anyBars; });
    if (moved && !again) spreadDots(true);
  }
  // These tables are restyled as blocks on phones, which would otherwise hide that they are tables.
  function markTables() {
    document.querySelectorAll('table.metaui-estimates:not([role])').forEach(function (table) {
      table.setAttribute('role', 'table');
      table.querySelectorAll('thead, tbody, tfoot').forEach(function (part) { part.setAttribute('role', 'rowgroup'); });
      table.querySelectorAll('tr').forEach(function (row) { row.setAttribute('role', 'row'); });
      table.querySelectorAll('th').forEach(function (cell) { cell.setAttribute('role', 'columnheader'); });
      table.querySelectorAll('td').forEach(function (cell) { cell.setAttribute('role', 'cell'); });
    });
  }
  $(markTables);
  var spreadTimer;
  $(document).on('shiny:value shown.bs.tab', function () { setTimeout(function () { markTables(); spreadDots(); }, 0); });
  // Covers window resizes and a scrollbar arriving as later outputs render.
  new ResizeObserver(function () { clearTimeout(spreadTimer); spreadTimer = setTimeout(function () { spreadDots(); }, 120); }).observe(document.documentElement);
  $(document).on('click', '.metaui-forest-all', function () {
    var open = this.getAttribute('aria-expanded') !== 'true';
    this.setAttribute('aria-expanded', String(open));
    this.textContent = open ? 'Show studies only' : 'Show every effect';
    this.closest('.shiny-html-output').querySelectorAll('.metaui-forest-study').forEach(function (row) { setStudy(row, open); });
  });
  // Studies move with their effects; a second click restores the order by name.
  $(document).on('click', '.metaui-forest-sort', function () {
    var byEstimate = this.getAttribute('aria-pressed') !== 'true';
    this.setAttribute('aria-pressed', String(byEstimate));
    this.textContent = byEstimate ? 'Sort by study name' : 'Sort by estimate';
    var body = this.closest('.shiny-html-output').querySelector('.metaui-forest tbody');
    var groups = [];
    Array.prototype.forEach.call(body.rows, function (row) {
      if (row.classList.contains('metaui-primary-row')) return;
      if (row.classList.contains('metaui-forest-effect')) groups[groups.length - 1].rows.push(row);
      else groups.push({ rows: [row], es: Number(row.dataset.es), name: row.cells[0].textContent });
    });
    groups.sort(function (a, b) { return byEstimate ? b.es - a.es : a.name.localeCompare(b.name); });
    groups.forEach(function (group) { group.rows.forEach(function (row) { body.appendChild(row); }); });
  });
})(jQuery);

/* Phones: tables with four or more columns reflow into one card per row (metaui.css).
   Each cell is labelled with its column heading. Tables whose first column names the
   row (models, studies) use it as the card title. static_stack_table() in the package
   applies the same labels to the script-free static preview. */
(function ($) {
  'use strict';
  var numeric = /^[\s\d.,%<>=+\-−[\]()]*$|^NA$/;
  function stack(table) {
    var head = table.tHead;
    if (!head || !head.rows.length || !table.tBodies.length) return;
    var names = Array.prototype.map.call(head.rows[head.rows.length - 1].cells,
      function (cell) { return cell.textContent.replace(/\s+/g, ' ').trim(); });
    if (names.length < 4) return;
    var rows = [];
    Array.prototype.forEach.call(table.tBodies, function (body) {
      Array.prototype.forEach.call(body.rows, function (row) {
        if (row.cells.length === names.length) rows.push(row); // not child or "no data" rows
      });
    });
    rows.forEach(function (row) {
      Array.prototype.forEach.call(row.cells, function (cell, i) {
        cell.setAttribute('data-label', names[i]);
        cell.toggleAttribute('data-empty', !cell.textContent.trim() && !cell.querySelector('img, svg, button, input'));
      });
    });
    // Columns empty in every row are hidden on cards, so their sort buttons go too.
    Array.prototype.forEach.call(head.rows[head.rows.length - 1].cells, function (th, i) {
      th.toggleAttribute('data-empty', rows.length > 0 && rows.every(function (row) { return row.cells[i].hasAttribute('data-empty'); }));
    });
    var named = rows.length && rows.every(function (row) { return !numeric.test(row.cells[0].textContent.trim()); });
    table.classList.add('metaui-stack');
    table.classList.toggle('metaui-stack-named', named);
    // DataTables headers stay visible on phones as sort buttons when sorting is useful.
    table.classList.toggle('metaui-stack-sortable', rows.length > 1 && !!head.querySelector('.sorting, .sorting_asc, .sorting_desc, .dt-orderable-asc, .dt-orderable-desc'));
  }
  function stackAll() { document.querySelectorAll('.tab-content table:not(.metaui-custom)').forEach(stack); }
  var timer;
  function schedule() { clearTimeout(timer); timer = setTimeout(stackAll, 30); }
  $(document).on('draw.dt', function (event, settings) { if (settings && settings.nTable) stack(settings.nTable); });
  document.addEventListener('DOMContentLoaded', function () {
    var results = document.querySelector('.tab-content') || document.body;
    new MutationObserver(schedule).observe(results, { childList: true, subtree: true });
    schedule();
  });
})(jQuery);
