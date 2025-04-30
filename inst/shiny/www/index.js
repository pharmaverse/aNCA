/**
 * Enables bootstrap tooltips for the whole application.
 */
$(document).ready(() => {
  $('[data-toggle="tooltip"]').tooltip();
});

/**
 * Disables button for a given period of time. If function is re-called
 * If the function is re-called during the timeout period, it will
 * be extended appropriately.
 * 
 * @param {String} selector    Selector for the button to disable.
 * @param {Number} debounce    Time in miliseconds that the button should be disabled for.
 * @param {String} placeholder HTML string to show when button is disabled.
 * @param {String} ready       HTML string to show when button is ready.
 */
let disable_button_timeouts = {};
const buttonTimeout = function(selector, debounce, placeholder, ready) {
  $(selector).html(placeholder).prop("disabled", true);
  clearTimeout(disable_button_timeouts[selector]);
  disable_button_timeouts[selector] = setTimeout(() => {
    $(selector).html(ready).prop("disabled", false);
  }, debounce)
}