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
