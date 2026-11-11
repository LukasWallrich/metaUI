// metaUI app behaviour. Copied into each generated app's www/ folder; edit freely.
// Slider grid labels can still collide in narrow sidebars or after uploads change a
// slider's range. Hide any label that would overlap a neighbour, keeping both ends.
(function () {
  function declutter() {
    // The chosen range is written beside the filter name; the scale ends stay under the track.
    document.documentElement.classList.add('metaui-js');
    // Selectize types into its own box, which the input's <label for> does not reach;
    // give that box the label's name, and the file input's text box one of its own.
    document.querySelectorAll('.shiny-input-container .selectize-control').forEach(function (control) {
      var box = control.querySelector('.selectize-input input');
      var label = control.closest('.shiny-input-container').querySelector('.control-label');
      if (box && label && !box.hasAttribute('aria-label')) box.setAttribute('aria-label', label.textContent.trim());
    });
    document.querySelectorAll('.shiny-input-container input[type="text"][readonly]').forEach(function (box) {
      if (!box.hasAttribute('aria-label')) box.setAttribute('aria-label', 'Selected file');
    });
    document.querySelectorAll('.irs').forEach(function (slider) {
      // Negative values get a true minus sign, as elsewhere in the app.
      slider.querySelectorAll('.irs-grid-text, .irs-min, .irs-max, .irs-from, .irs-to, .irs-single').forEach(function (el) {
        if (el.textContent.indexOf('-') >= 0) el.textContent = el.textContent.replace(/-/g, '\u2212');
      });
      var from = slider.querySelector('.irs-from'), to = slider.querySelector('.irs-to');
      var container = slider.closest('.shiny-input-container');
      var label = container && container.querySelector('.control-label');
      if (!from || !to || !label) return;
      // The range is a button: pressed, it turns into two boxes for typing exact ends.
      var range = label.querySelector('.metaui-range');
      if (!range) {
        range = document.createElement('button'); range.type = 'button'; range.className = 'metaui-range';
        label.appendChild(range);
      }
      if (range.tagName !== 'BUTTON') return; // being edited
      var text = from.textContent + ' to ' + to.textContent;
      if (range.textContent !== text) {
        range.textContent = text;
        // The filter's name comes first, so that the button makes sense on its own.
        var name = label.firstChild && label.firstChild.nodeType === 3 ? label.firstChild.textContent.trim() : '';
        range.setAttribute('aria-label', (name ? name + ': ' : '') + text + '. Type exact values');
      }
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
  $(document).on('click', 'button.metaui-range', function (event) {
    event.preventDefault(); event.stopPropagation();
    var button = this, container = button.closest('.shiny-input-container');
    var input = container && container.querySelector('.js-range-slider');
    var slider = input && $(input).data('ionRangeSlider');
    if (!slider) return;
    var o = slider.options, name = (container.querySelector('.control-label').firstChild || {}).textContent || '';
    function box(value, end) {
      var field = document.createElement('input');
      field.type = 'number'; field.inputMode = 'decimal'; field.className = 'metaui-range-box';
      field.min = o.min; field.max = o.max; field.step = o.step || 'any'; field.value = value;
      field.setAttribute('aria-label', name.trim() + ', ' + end);
      return field;
    }
    var a = box(slider.result.from, 'from'), b = box(slider.result.to, 'to');
    var boxes = document.createElement('span'); boxes.className = 'metaui-range metaui-range-boxes';
    boxes.append(a, ' to ', b);
    button.replaceWith(boxes);
    a.focus(); a.select();
    var done = false;
    function finish(apply) {
      if (done) return; done = true;
      var f = parseFloat(a.value), t = parseFloat(b.value);
      if (apply && isFinite(f) && isFinite(t)) {
        if (f > t) { var x = f; f = t; t = x; }
        f = Math.min(o.max, Math.max(o.min, f)); t = Math.min(o.max, Math.max(o.min, t));
        slider.update({ from: f, to: t });
        $(input).trigger('change');
      }
      boxes.replaceWith(button);
      schedule();
      button.focus();
    }
    boxes.addEventListener('keydown', function (e) {
      if (e.key === 'Enter') { e.preventDefault(); finish(true); }
      else if (e.key === 'Escape') { e.preventDefault(); finish(false); }
    });
    boxes.addEventListener('focusout', function (e) { if (!boxes.contains(e.relatedTarget)) finish(true); });
    // A press inside the label must not move focus to the slider.
    boxes.addEventListener('click', function (e) { e.preventDefault(); e.stopPropagation(); });
  });
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
    var moved = false, anyBars = false, mostInBar = 0;
    document.querySelectorAll('.metaui-moderators').forEach(function (table) {
      var tracks = Array.prototype.slice.call(table.querySelectorAll('tbody .metaui-track'));
      var dotCount = function (track) { return track.querySelectorAll('.metaui-track-dot').length; };
      // A level with many effects would run very tall, so it becomes a histogram: sooner on phones.
      var crowded = function (track) { return dotCount(track) >= (phone.matches ? 25 : 100); };
      var binWidth = function (width) { return Math.max(4, width / 60); };
      var binsOf = function (track, width) {
        var bins = {};
        track.querySelectorAll('.metaui-track-dot').forEach(function (dot) {
          var bin = Math.floor(parseFloat(dot.style.left) / 100 * width / binWidth(width));
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
      if (tracks.some(crowded)) { anyBars = true; mostInBar = Math.max(mostInBar, tallestBin); }
      // One shrink per table, so a dot's size means the same in every row.
      var largest = Math.max.apply(null, tracks.filter(function (track) { return !crowded(track); }).map(dotCount).concat(1));
      var crowd = Math.max(.75, Math.min(1, Math.sqrt(60 / Math.max(largest, 1))));
      tracks.forEach(function (track) {
        var width = track.clientWidth;
        if (!width || track.dataset.spreadWidth === String(width)) return;
        track.dataset.spreadWidth = width;
        if (!dotCount(track)) return;
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
            bar.title = bins[bin] + (bins[bin] === 1 ? ' effect' : ' effects');
            bar.style.left = (bin * binWidth(width)) + 'px';
            bar.style.width = (binWidth(width) - .5) + 'px';
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
    // The key names only what is drawn: dots, bars or both.
    document.querySelectorAll('.metaui-dots-note').forEach(function (note) {
      note.hidden = !Array.prototype.some.call(document.querySelectorAll('.metaui-moderators .metaui-track-dot'), function (dot) { return dot.style.display !== 'none'; });
    });
    document.querySelectorAll('.metaui-hist-note').forEach(function (note) {
      note.hidden = !anyBars;
      var most = note.querySelector('.metaui-hist-max');
      if (most) most.textContent = mostInBar;
    });
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
  // Where tick labels would touch, only every second (or third) is shown, always keeping zero.
  function thinAxes() {
    document.querySelectorAll('.metaui-axis').forEach(function (axis) {
      var ticks = Array.prototype.slice.call(axis.querySelectorAll('.metaui-axis-tick'));
      ticks.forEach(function (tick) { tick.style.visibility = ''; });
      if (!axis.clientWidth) return;
      var zero = Math.max(0, ticks.findIndex(function (tick) { return parseFloat(tick.textContent.replace('\u2212', '-')) === 0; }));
      var clash = function (shown) {
        return shown.some(function (tick, i) { return i > 0 && tick.getBoundingClientRect().left < shown[i - 1].getBoundingClientRect().right + 3; });
      };
      for (var step = 1; step <= 4; step++) {
        var shown = ticks.filter(function (tick, i) { return (i - zero) % step === 0; });
        if (!clash(shown)) break;
      }
      ticks.forEach(function (tick) { if (shown.indexOf(tick) < 0) tick.style.visibility = 'hidden'; });
    });
  }
  $(thinAxes);
  var spreadTimer;
  // A plot drawn in a hidden tab has no width to go by, and would overflow a phone.
  function fitPlots() {
    if (!window.Plotly) return;
    document.querySelectorAll('.tab-pane.active .js-plotly-plot').forEach(function (plot) {
      if (plot.clientWidth && plot.parentElement.clientWidth !== plot.querySelector('.main-svg').clientWidth) window.Plotly.Plots.resize(plot);
    });
  }
  $(document).on('shiny:value shown.bs.tab', function () { setTimeout(function () { markTables(); spreadDots(); fitPlots(); restoreForest(); restoreModerators(); thinAxes(); }, 0); });
  $(document).on('plotly_afterplot shiny:visualchange', function () { setTimeout(fitPlots, 0); });
  // Covers window resizes and a scrollbar arriving as later outputs render.
  new ResizeObserver(function () { clearTimeout(spreadTimer); spreadTimer = setTimeout(function () { spreadDots(); thinAxes(); }, 120); }).observe(document.documentElement);
  // Two-way view switches (metaUI_switch in helpers.R): pressing the other option changes the view.
  function press(group, value) {
    group.querySelectorAll('button').forEach(function (button) { button.setAttribute('aria-pressed', String(button.dataset.value === value)); });
  }
  var forestAll = false;
  $(document).on('click', '.metaui-switch button', function () {
    if (this.getAttribute('aria-pressed') === 'true') return;
    var value = this.dataset.value;
    press(this.closest('.metaui-switch'), value);
    if (this.classList.contains('metaui-forest-all')) {
      forestAll = value === 'effects';
      this.closest('.shiny-html-output').querySelectorAll('.metaui-forest-study').forEach(function (row) { setStudy(row, forestAll); });
    } else if (this.classList.contains('metaui-forest-sort')) {
      forestFocus = { selector: '.metaui-forest-sort[data-value="' + value + '"]', index: 0 };
      Shiny.setInputValue('forest_sort', value, { priority: 'event' });
    } else if (this.classList.contains('metaui-moderator-effects')) {
      moderatorFocus = true;
      Shiny.setInputValue('moderator_effects', value === 'show', { priority: 'event' });
    } else if (this.classList.contains('metaui-sample-view')) {
      Shiny.setInputValue('sample_view', value, { priority: 'event' });
    }
  });
  // The server sends one page at a time, so a new page takes on the reader's current view.
  var forestFocus = null;
  function restoreForest() {
    var table = document.querySelector('.metaui-forest:not([data-restored])');
    if (!table) return;
    table.dataset.restored = 'true';
    var box = table.closest('.shiny-html-output'), all = box.querySelector('.metaui-forest-all');
    if (forestAll && all) {
      press(all.closest('.metaui-switch'), 'effects');
      box.querySelectorAll('.metaui-forest-study').forEach(function (row) { setStudy(row, true); });
    }
    if (!forestFocus) return;
    var button = box.querySelectorAll(forestFocus.selector)[forestFocus.index];
    if (button && button.disabled) button = button.closest('p').querySelector('button:not([disabled])');
    // From the pager under the table, the new page starts back at the top, and focus goes there too.
    var fromBottom = forestFocus.index === 1;
    if (fromBottom) { button = box.querySelectorAll(forestFocus.selector)[0]; if (button && button.disabled) button = button.closest('p').querySelector('button:not([disabled])'); }
    if (button) button.focus({ preventScroll: true });
    if (fromBottom && button) button.closest('p').scrollIntoView();
    forestFocus = null;
  }
  $(document).on('click', '.metaui-forest-prev, .metaui-forest-next', function () {
    var selector = this.classList.contains('metaui-forest-next') ? '.metaui-forest-next' : '.metaui-forest-prev';
    forestFocus = { selector: selector, index: Array.prototype.indexOf.call(this.closest('.shiny-html-output').querySelectorAll(selector), this) };
    Shiny.setInputValue('forest_page', Number(this.dataset.page), { priority: 'event' });
  });
  $(document).on('change', '.metaui-forest-page', function () {
    forestFocus = { selector: '.metaui-forest-page', index: Array.prototype.indexOf.call(this.closest('.shiny-html-output').querySelectorAll('.metaui-forest-page'), this) };
    Shiny.setInputValue('forest_page', Number(this.value), { priority: 'event' });
  });
  var moderatorFocus = false;
  function restoreModerators() {
    if (!moderatorFocus) return;
    var button = document.querySelector('.metaui-moderator-effects[aria-pressed="true"]');
    if (button) button.focus({ preventScroll: true });
    moderatorFocus = false;
  }
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

/* Filters the reader has narrowed stand out from the untouched ones. */
(function ($) {
  var timer;
  function unchecked(box) {
    var check = box && box.querySelector('input[type=checkbox]');
    return !!check && !check.checked;
  }
  function mark() {
    document.querySelectorAll('form.well .shiny-input-container').forEach(function (box) {
      var select = box.querySelector('select[multiple]');
      var slider = box.querySelector('.js-range-slider');
      var narrowed = false;
      if (select) narrowed = select.selectedOptions.length < select.options.length;
      else if (slider) {
        var range = $(slider).data('ionRangeSlider');
        var missing = slider.id && document.getElementById(slider.id + '_include_NA');
        narrowed = (!!range && (range.result.from !== range.result.min || range.result.to !== range.result.max)) ||
          (!!missing && !missing.checked && !missing.closest('.metaui-no-missing'));
      } else if (box.classList.contains('shiny-input-checkboxgroup')) {
        narrowed = Array.prototype.some.call(box.querySelectorAll('input[type=checkbox]'), function (check) { return !check.checked; });
      } else if (/_include_NA$/.test((box.querySelector('input[type=checkbox]') || {}).id || '')) {
        narrowed = unchecked(box) && !box.classList.contains('metaui-no-missing');
      }
      box.classList.toggle('metaui-narrowed', narrowed);
    });
  }
  $(document).on('shiny:inputchanged', function () { clearTimeout(timer); timer = setTimeout(mark, 60); });
})(jQuery);

/* Shiny draws a tab only while it is visible. After each analysis, the other tabs are drawn
   one at a time off-screen, so that opening them is instant. A tab opened before it is ready
   shows one message, never empty headings or the previous selection's results. */
(function ($) {
  var ready = new WeakSet();
  var analysis = 0, queue = [], running = null;
  var TOO_SLOW = 20000;   // A background tab this slow stops the sequence: the reader's next click would wait for it.
  var GIVE_UP = 60000;    // An opened tab shows what it has after this long, without being counted as ready.
  var asked = 0, answers = {};
  // Outputs that have had a value or an error. One drawn inside another (a table inside a
  // panel) is bound only when its parent arrives, and asks for its own value after that.
  var received = new WeakSet();
  $(document).on('shiny:value shiny:error', function (event) { received.add(event.target); });
  function waiting(pane) {
    return !!pane && Array.prototype.some.call(pane.querySelectorAll('.shiny-bound-output'), function (output) {
      return output.offsetParent !== null && !received.has(output);
    });
  }

  function panes() {
    return Array.prototype.map.call(document.querySelectorAll('#metaui_results .nav-tabs > li > a'), function (link) {
      var pane = document.querySelector(link.getAttribute('href'));
      if (pane) {
        pane.setAttribute('data-metaui-title', link.textContent.trim());
        if (!pane.querySelector(':scope > .metaui-pane-status')) {
          var status = document.createElement('div');
          status.className = 'metaui-pane-status';
          status.setAttribute('role', 'status');
          pane.insertBefore(status, pane.firstChild);
        }
      }
      return pane;
    }).filter(Boolean);
  }
  // Bayesian and computation fits can take minutes, so they wait for the reader. The others
  // follow the order of the tabs, which is the order readers usually open them in.
  function backgroundOrder() {
    return panes().filter(function (pane) {
      return pane.querySelector('.shiny-bound-output') && !pane.querySelector('#bayesian_sensitivity, #computation_comparison');
    });
  }
  var listening = false;
  function listen() {
    if (listening || !window.Shiny || !Shiny.addCustomMessageHandler) return;
    listening = true;
    Shiny.addCustomMessageHandler('metaui_drawn', function (id) { if (answers[id]) answers[id](); });
  }
  listen();
  $(document).on('shiny:connected', listen);
  // Calls done once the server has answered for the outputs just shown and fallen quiet. The
  // server answers after it has started on them, so the wait never ends on elapsed time alone.
  // Returns a function that cancels the wait.
  function whenSettled(done, pane) {
    var id = ++asked, sent = Date.now(), quietFor = 0, quiet, cancelled = false;
    var check = function () {
      clearTimeout(quiet);
      if (quietFor && !document.documentElement.classList.contains('shiny-busy')) quiet = setTimeout(function () { if (!waiting(pane)) finish(); }, quietFor);
    };
    var stop = function () {
      cancelled = true;
      clearTimeout(quiet);
      delete answers[id];
      $(document).off('shiny:busy shiny:idle shiny:value shiny:error', check);
    };
    var finish = function () { stop(); done(); };
    // Outputs drawn from a new value can ask for more, one round trip later.
    // The answer comes after the server's work, so its delay overstates the network's; outputs
    // still to be bound are caught by waiting(), so a short quiet spell is enough.
    answers[id] = function () { delete answers[id]; quietFor = Math.max(150, Math.min(400, Date.now() - sent)); check(); };
    $(document).on('shiny:busy shiny:idle shiny:value shiny:error', check);
    // After Shiny has reported which outputs are now visible.
    setTimeout(function () { if (!cancelled) Shiny.setInputValue('metaui_drawn', id, { priority: 'event' }); }, 10);
    return stop;
  }
  function finishBackground(job, completed) {
    if (running !== job) return;
    running = null;
    clearTimeout(job.watchdog);
    job.cancel();
    job.pane.classList.remove('metaui-preloading');
    job.pane.style.width = '';
    if (completed && job.analysis === analysis) ready.add(job.pane);
    if (!job.pane.classList.contains('active')) $(job.pane).trigger('hidden');
    if (completed) setTimeout(next, 50);
    else { queue = []; document.documentElement.classList.remove('metaui-background'); }
  }
  function next() {
    if (running) return;
    var pane = queue.shift();
    if (!pane) { document.documentElement.classList.remove('metaui-background'); return; }
    if (ready.has(pane) || pane.classList.contains('active')) return next();
    var active = document.querySelector('#metaui_results .tab-pane.active');
    var job = { pane: pane, analysis: analysis };
    running = job;
    document.documentElement.classList.add('metaui-background');
    pane.style.width = active ? active.offsetWidth + 'px' : '';
    pane.classList.add('metaui-preloading');
    $(pane).trigger('shown');
    job.cancel = whenSettled(function () { finishBackground(job, true); }, pane);
    job.watchdog = setTimeout(function () { finishBackground(job, false); }, TOO_SLOW);
  }
  function newAnalysis() {
    analysis += 1;
    ready = new WeakSet();
    queue = [];
    // The reader's own analysis shows as busy, even while a background tab finishes.
    document.documentElement.classList.remove('metaui-background');
  }
  // Clicking Update puts every tab out of date at once; the answer restarts the sequence.
  $(document).on('click', '#go', newAnalysis);
  // Saved opening results stay until the server has real values: before the first analysis
  // starts, each output reports a silent error that would blank the page for a moment.
  $(document).on('shiny:error', function (event) {
    if (event.target.hasAttribute && event.target.hasAttribute('data-metaui-opening') && event.error.type && event.error.type.indexOf('shiny.silent.error') >= 0) event.preventDefault();
  });
  $(document).on('shiny:value', function (event) {
    if (event.target.removeAttribute) event.target.removeAttribute('data-metaui-opening');
    if (event.name !== 'selection_overview') return;
    newAnalysis();
    var forAnalysis = analysis;
    var active = document.querySelector('#metaui_results .tab-pane.active');
    if (active) ready.add(active);
    whenSettled(function () { if (forAnalysis === analysis) { queue = backgroundOrder(); next(); } });
  });
  // The message goes up before the tab appears, and the wait starts once Shiny has seen it shown.
  $(document).on('show.bs.tab', '#metaui_results .nav-tabs a', function () {
    var pane = document.querySelector(this.getAttribute('href'));
    if (!pane || ready.has(pane) || !pane.querySelector('.shiny-bound-output') || pane.classList.contains('metaui-pane-loading')) return;
    var status = pane.querySelector(':scope > .metaui-pane-status');
    pane.classList.add('metaui-pane-loading');
    if (status) status.textContent = 'Preparing ' + pane.getAttribute('data-metaui-title') + '\u2026';
  });
  $(document).on('shown.bs.tab', '#metaui_results .nav-tabs a', function () {
    var pane = document.querySelector(this.getAttribute('href'));
    if (!pane || !pane.classList.contains('metaui-pane-loading') || pane.metauiWaiting) return;
    var forAnalysis = analysis, status = pane.querySelector(':scope > .metaui-pane-status'), giveUp, cancel;
    var end = function (completed) {
      clearTimeout(giveUp);
      cancel();
      pane.metauiWaiting = false;
      pane.classList.remove('metaui-pane-loading');
      if (status) status.textContent = '';
      if (completed && forAnalysis === analysis) ready.add(pane);
    };
    pane.metauiWaiting = true;
    cancel = whenSettled(function () { end(true); }, pane);
    giveUp = setTimeout(function () { end(false); }, GIVE_UP);
  });
  document.addEventListener('DOMContentLoaded', panes);
})(jQuery);


/* Copy link: the address that reopens this selection goes to the clipboard. Plain http pages
   cannot use the clipboard API, so a hidden text field does the copying there; failing both,
   the link is shown selected for the reader to copy. */
(function ($) {
  function fallback(url) {
    var field = document.createElement('textarea');
    field.value = url; field.setAttribute('readonly', ''); field.style.position = 'fixed'; field.style.opacity = '0';
    document.body.appendChild(field); field.select();
    var ok = false;
    try { ok = document.execCommand('copy'); } catch (e) { ok = false; }
    document.body.removeChild(field);
    return ok;
  }
  // A static export runs the app in a frame; the link must open the page around it.
  function staticPage() {
    if (window.parent === window || !/\/app_[^/]+\/$/.test(location.pathname)) return null;
    try { return window.parent.location.href; } catch (e) { return null; }
  }
  $(document).on('click', '.metaui-copy-link', function () {
    var page = staticPage();
    var url = new URL(this.getAttribute('data-href'), page || window.location.href).href;
    var status = this.parentNode.querySelector('.metaui-copy-status');
    // Filters changed but not applied are not in the link, which holds the results shown.
    var pending = document.querySelector('#metaui_stale_banner.metaui-visible');
    var copied = 'Copied' + (pending ? ': the link is for the results shown, not your new filters, until you update.' :
      page ? '. The link opens once the app has loaded, which can take about a minute.' : '');
    var show = function (ok) {
      if (!status) return;
      if (ok) { status.textContent = copied; clearTimeout(status.metauiTimer); status.metauiTimer = setTimeout(function () { status.textContent = ''; }, pending || page ? 8000 : 3000); return; }
      status.textContent = 'Copy this link: ';
      var box = document.createElement('input'); box.type = 'text'; box.readOnly = true; box.value = url;
      box.setAttribute('aria-label', 'Link to this selection'); status.appendChild(box); box.select();
    };
    if (navigator.clipboard && window.isSecureContext) navigator.clipboard.writeText(url).then(function () { show(true); }, function () { show(fallback(url)); });
    else show(fallback(url));
  });
})(jQuery);

/* Update results: a new fit can take several seconds, so the button and the status line say
   that it is running until the new results have arrived; the server then rewrites the status
   line itself. Updates that finish quickly (cached fits) change nothing on screen, nor do clicks
   with nothing to apply (such as the automatic opening run). */
(function ($) {
  'use strict';
  var run = 0, label = null, showTimer;
  function parts() {
    var go = document.getElementById('go');
    return { go: go, text: go && (go.querySelector('.action-label') || go), box: document.getElementById('apply_state_box') };
  }
  function show() {
    var p = parts();
    if (!p.go) return;
    if (label === null) label = p.text.textContent;
    p.text.textContent = 'Updating…';
    p.go.classList.add('metaui-updating');
    if (!p.box) return;
    var note = p.box.querySelector('.metaui-updating-note');
    if (!note) { note = document.createElement('span'); note.className = 'metaui-updating-note'; p.box.appendChild(note); }
    note.textContent = 'Updating results…';
    p.box.classList.add('metaui-updating');
  }
  function restore() {
    clearTimeout(showTimer);
    var p = parts();
    if (p.go && label !== null) p.text.textContent = label;
    label = null;
    if (p.go) p.go.classList.remove('metaui-updating');
    if (p.box) {
      p.box.classList.remove('metaui-updating');
      var note = p.box.querySelector('.metaui-updating-note');
      if (note) note.remove();
    }
  }
  $(document).on('click', '#go', function () {
    if (this.disabled || !this.classList.contains('metaui-needs-run')) return;
    var mine = ++run, answered = false;
    clearTimeout(showTimer);
    showTimer = setTimeout(function () { if (mine === run) show(); }, 300);
    // Shiny goes idle several times during one update (outputs that do not need the fit come
    // first). The selection line is redrawn with the fitted results, so the update has finished
    // at the first idle after it.
    var value = function (event) { if (event.name === 'selection_overview') answered = true; };
    var idle = function () { if (answered) finish(); };
    var finish = function () {
      clearTimeout(giveUp);
      $(document).off('shiny:value', value).off('shiny:idle', idle);
      if (mine === run) restore();
    };
    var giveUp = setTimeout(finish, 300000);
    $(document).on('shiny:value', value).on('shiny:idle', idle);
  });
})(jQuery);

/* Reset brings back the results for the default selection. The reset filters reach the server
   over several round trips; once no filter has changed for a moment, the server is idle and it
   marks Update as needed, Update is pressed for the reader (the default fit is cached, so this
   is quick). Shiny handles messages in order, so the click always follows the reset values. */
(function ($) {
  'use strict';
  var poll;
  function stop() { clearInterval(poll); poll = null; $(document).off('.metauiReset'); }
  $(document).on('click', '#resetFilters', function () {
    stop();
    var started = Date.now(), lastChange = started, lastIdle = 0;
    // Tabs drawn in the background keep the server briefly busy, so only filter changes count.
    $(document).on('shiny:inputchanged.metauiReset', function (event) {
      if (event.el && event.el.closest && event.el.closest('form.well')) lastChange = Date.now();
    }).on('shiny:idle.metauiReset', function () { lastIdle = Date.now(); });
    poll = setInterval(function () {
      var go = document.getElementById('go');
      if (Date.now() - started > 15000) return stop();
      // The server has answered since the last filter change, which has had time to settle.
      if (!go || Date.now() - lastChange < 600 || lastIdle < lastChange + 50 ||
        !go.classList.contains('metaui-needs-run') || go.disabled) return;
      stop();
      // Marked as automatic: the reader stays in the filters they just reset.
      $(go).trigger('click', [{ auto: true }]);
    }, 200);
  });
})(jQuery);

/* A disclosure opened near the foot of the filters scrolls into view above the sticky update bar. */
document.addEventListener('toggle', function (event) {
  var details = event.target;
  if (!details.open || !details.matches || !details.matches('form.well details')) return;
  var bar = document.querySelector('form.well .metaui-actions');
  if (!bar || getComputedStyle(bar).position !== 'sticky') return;
  details.style.scrollMarginBottom = (bar.offsetHeight + 12) + 'px';
  details.scrollIntoView({ block: 'nearest' });
}, true); // toggle does not bubble

/* Wide DataTables scroll sideways. While columns are out of view, a shade at the right edge
   (metaui.css) and a short note above the table say so. */
(function ($) {
  'use strict';
  function mark(scroller) {
    var table = scroller.querySelector(':scope > table.dataTable');
    if (!table) return;
    // Tables stacked into cards on phones have no columns to scroll to.
    var over = getComputedStyle(table).display !== 'block' && scroller.scrollWidth > scroller.clientWidth + 2;
    scroller.classList.toggle('metaui-overflow', over);
    var note = scroller.previousElementSibling && scroller.previousElementSibling.classList.contains('metaui-scroll-note') ? scroller.previousElementSibling : null;
    if (over && !note) {
      note = document.createElement('p');
      note.className = 'metaui-scroll-note';
      note.textContent = 'Scroll sideways for more columns →';
      scroller.parentNode.insertBefore(note, scroller);
    }
    if (note) note.hidden = !over;
  }
  var timer;
  function markAll() {
    clearTimeout(timer);
    timer = setTimeout(function () { document.querySelectorAll('.tab-content .metaui-dt-scroll').forEach(mark); }, 50);
  }
  $(document).on('draw.dt shown.bs.tab', markAll);
  window.addEventListener('resize', markAll);
})(jQuery);

/* The out-of-date banner above the results updates them, as the panel's button does. */
(function ($) {
  'use strict';
  $(document).on('click', '.metaui-banner-go', function () {
    var go = document.getElementById('go');
    if (go && !go.disabled) go.click();
  });
})(jQuery);

/* Range sliders for keyboard and screen readers. ion.rangeSlider gives its two handles one
   keyboard stop (the track) and no names, so each handle becomes its own named slider. The arrow
   keys, Page Up/Down, Home and End move it through the slider's own API, which fires the change
   that Shiny listens to. An update redraws the whole slider, so the roles are reapplied after
   every redraw. */
(function ($) {
  'use strict';
  function instance(input) { return input && $(input).data('ionRangeSlider'); }
  // The filter's name, without the chosen range and help mark that metaui adds to its label.
  function filterName(input) {
    var box = input.closest('.shiny-input-container');
    var label = document.getElementById(input.id + '-label') || (box && box.querySelector('.control-label'));
    if (!label) return input.id;
    var copy = label.cloneNode(true);
    copy.querySelectorAll('.metaui-range, .metaui-filter-help').forEach(function (el) { el.remove(); });
    return copy.textContent.replace(/\s+/g, ' ').trim();
  }
  function shown(slider, value) {
    try { return String(slider.decorate(slider._prettify(value), value)); } catch (e) { return String(value); }
  }
  function enhance(input) {
    var slider = instance(input), box = input.previousElementSibling;
    if (!slider || !box || !box.classList.contains('irs')) return;
    var r = slider.result, double = slider.options.type === 'double', name = filterName(input);
    // The readonly field repeats the handles' values, and the track and scale only draw them.
    input.setAttribute('aria-hidden', 'true');
    box.querySelectorAll('.irs-line, .irs-grid').forEach(function (el) { el.setAttribute('aria-hidden', 'true'); el.setAttribute('tabindex', '-1'); });
    [['from', '.irs-handle.from'], ['to', '.irs-handle.to'], ['from', '.irs-handle.single']].forEach(function (pair) {
      var handle = box.querySelector(pair[1]), end = pair[0];
      if (!handle) return;
      handle.dataset.metauiEnd = end;
      handle.setAttribute('role', 'slider');
      handle.setAttribute('tabindex', slider.options.disable ? '-1' : '0');
      handle.setAttribute('aria-label', double ? name + (end === 'from' ? ', lower end' : ', upper end') : name);
      handle.setAttribute('aria-orientation', 'horizontal');
      // Each end can move only up to the other.
      handle.setAttribute('aria-valuemin', end === 'to' ? r.from : r.min);
      handle.setAttribute('aria-valuemax', end === 'from' && double ? r.to : r.max);
      handle.setAttribute('aria-valuenow', r[end]);
      handle.setAttribute('aria-valuetext', shown(slider, r[end]));
      handle.toggleAttribute('aria-disabled', !!slider.options.disable);
    });
  }
  function enhanceAll() { document.querySelectorAll('input.js-range-slider').forEach(enhance); }
  $(document).on('keydown', '.irs-handle[role="slider"]', function (event) {
    var box = this.closest('[class*="js-irs-"]'), input = box && box.nextElementSibling, slider = instance(input);
    if (!slider || slider.options.disable) return;
    var r = slider.result, end = this.dataset.metauiEnd, step = slider.options.step || 1;
    var page = Math.max(step, Math.round((r.max - r.min) / 10 / step) * step);
    var moves = { ArrowLeft: -step, ArrowDown: -step, ArrowRight: step, ArrowUp: step, PageDown: -page, PageUp: page };
    var value;
    if (event.key in moves) value = r[end] + moves[event.key];
    else if (event.key === 'Home') value = r.min;
    else if (event.key === 'End') value = r.max;
    else return;
    event.preventDefault();
    var low = end === 'to' ? r.from : r.min, high = end === 'from' && slider.options.type === 'double' ? r.to : r.max;
    var decimals = (String(step).split('.')[1] || '').length;
    value = Number(Math.min(high, Math.max(low, r.min + Math.round((value - r.min) / step) * step)).toFixed(decimals));
    if (value === r[end]) return;
    var change = {};
    change[end] = value;
    slider.update(change);
    // The redrawn slider has new handles: name them and keep the focus on this end.
    enhance(input);
    var handle = input.previousElementSibling.querySelector('[data-metaui-end="' + end + '"]');
    if (handle) handle.focus();
  });
  var timer;
  document.addEventListener('DOMContentLoaded', function () {
    new MutationObserver(function (mutations) {
      for (var i = 0; i < mutations.length; i++) {
        var target = mutations[i].target;
        if (target.closest && target.closest('.shiny-input-container')) { clearTimeout(timer); timer = setTimeout(enhanceAll, 0); return; }
      }
    }).observe(document.body, { childList: true, subtree: true });
  });
  $(document).on('shiny:bound shiny:inputchanged', function () { clearTimeout(timer); timer = setTimeout(enhanceAll, 0); });
})(jQuery);

/* Find a study (forest plot): once its page is drawn, the marked study scrolls into view. */
(function ($) {
  'use strict';
  var last = null;
  $(document).on('shiny:value', function (event) {
    if (event.target.id !== 'foreststudies') return;
    setTimeout(function () {
      var found = document.querySelector('#foreststudies .metaui-found');
      var name = found ? found.textContent : null;
      if (found && name !== last) found.scrollIntoView({ block: 'center' });
      last = name;
    }, 50);
  });
})(jQuery);

/* Plots are drawn on the server, so it is told whether the page is dark: the same rule as the
   dark colours in metaui.css (system setting or data-theme, never for an author design). */
(function ($) {
  'use strict';
  var query = window.matchMedia ? window.matchMedia('(prefers-color-scheme: dark)') : null;
  function dark() {
    if (document.querySelector('link[href$="app-design.css"], link[href^="metaui-design/"]')) return false;
    var theme = document.documentElement.getAttribute('data-theme');
    if (theme === 'dark' || theme === 'light') return theme === 'dark';
    return !!(query && query.matches);
  }
  function send() { if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('metaui_dark', dark()); }
  $(document).on('shiny:connected', send);
  if (query && query.addEventListener) query.addEventListener('change', send);
  new MutationObserver(send).observe(document.documentElement, { attributes: true, attributeFilter: ['data-theme'] });
})(jQuery);

/* A shared link: until the server has applied it, the banner says so, rather than leaving the
   dimmed default results unexplained. The server replaces the text once it takes over. */
document.addEventListener('DOMContentLoaded', function () {
  var query = new URLSearchParams(location.search);
  var banner = document.getElementById('metaui_stale_banner');
  if (!banner || !query.get('metaui')) return;
  var text = document.getElementById('stale_banner');
  if (text && !text.textContent.trim()) text.textContent = 'Applying the linked selection…';
  banner.classList.add('metaui-visible', 'metaui-restoring');
});

/* Pickers: the button names its filter, so a screen reader says "Country: 3 of 34 selected"
   rather than the count alone. */
(function ($) {
  function name(select) {
    var label = select.id && document.getElementById(select.id + '-label');
    var button = $(select).parent().find('button.dropdown-toggle')[0];
    if (!label || !button) return;
    var text = button.querySelector('.filter-option-inner-inner');
    button.setAttribute('aria-label', label.textContent.trim() + ': ' + (text ? text.textContent.trim() : ''));
  }
  $(document).on('loaded.bs.select rendered.bs.select changed.bs.select refreshed.bs.select', 'select', function () {
    var select = this; setTimeout(function () { name(select); }, 0);
  });
})(jQuery);

/* Links in notes that open another tab. */
(function ($) {
  'use strict';
  $(document).on('click', '.metaui-tab-link', function (event) {
    event.preventDefault();
    var tab = document.querySelector('.nav-tabs a[data-value="' + this.dataset.tab + '"]');
    if (tab) { $(tab).tab('show'); tab.focus(); window.scrollTo(0, 0); }
  });
})(jQuery);
