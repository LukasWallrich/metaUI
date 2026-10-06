/* Expandable author-written filter help; independent of Bootstrap version. */
(function ($) {
  'use strict';
  function closeHelp(button) {
    button.setAttribute('aria-expanded', 'false');
    document.getElementById(button.getAttribute('aria-controls')).hidden = true;
  }
  $(document).on('click', '.metaui-filter-help', function (event) {
    event.preventDefault();
    var expanded = this.getAttribute('aria-expanded') === 'true';
    this.setAttribute('aria-expanded', String(!expanded));
    document.getElementById(this.getAttribute('aria-controls')).hidden = expanded;
  });
  $(document).on('keydown', function (event) {
    if (event.key === 'Escape') {
      document.querySelectorAll('.metaui-filter-help[aria-expanded="true"]').forEach(closeHelp);
    }
  });
})(jQuery);
