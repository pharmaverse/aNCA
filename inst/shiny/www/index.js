/**
 * Disables all buttons with class `.btn-page`.
 * Enables them after 100ms.
 * This is to prevent the user changing the pages
 * too fast, which will cause glitches.
 */
const togglePageButtons = function() {
  const page_buttons = $('.btn-page');
  page_buttons.prop('disabled', true);
  setTimeout(function() {
    page_buttons.prop('disabled', false);
  }, 100);
};

$(document).on('click', '.btn-page', togglePageButtons);