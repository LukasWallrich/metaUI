// metaUI app behaviour. Copied into each generated app's www/ folder; edit freely.
// Slider grid labels can still collide in narrow sidebars or after uploads change a
// slider's range. Hide any label that would overlap a neighbour, keeping both ends.
(function () {
  function declutter() {
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
   Analyze data collapses them again and moves to the results. metaui.css hides the
   toggle and keeps the filters open on wider screens. */
(function ($) {
  'use strict';
  var phone = window.matchMedia('(max-width: 767px)');
  function setOpen(open) {
    var toggle = document.querySelector('.metaui-filters-toggle');
    if (!toggle) return;
    toggle.setAttribute('aria-expanded', String(open));
    toggle.textContent = open ? 'Hide filters' : 'Show filters';
    document.getElementById(toggle.getAttribute('aria-controls')).classList.toggle('metaui-open', open);
  }
  $(document).on('click', '.metaui-filters-toggle', function () {
    setOpen(this.getAttribute('aria-expanded') !== 'true');
  });
  $(document).on('click', '#go', function () {
    if (!phone.matches) return;
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
  function stackAll() { document.querySelectorAll('.tab-content table').forEach(stack); }
  var timer;
  function schedule() { clearTimeout(timer); timer = setTimeout(stackAll, 30); }
  $(document).on('draw.dt', function (event, settings) { if (settings && settings.nTable) stack(settings.nTable); });
  document.addEventListener('DOMContentLoaded', function () {
    var results = document.querySelector('.tab-content') || document.body;
    new MutationObserver(schedule).observe(results, { childList: true, subtree: true });
    schedule();
  });
})(jQuery);
