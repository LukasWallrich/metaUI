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
  document.addEventListener('mousedown', function (event) {
    if (event.target.closest && event.target.closest('.metaui-study-toggle')) event.stopPropagation();
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
})(jQuery);
