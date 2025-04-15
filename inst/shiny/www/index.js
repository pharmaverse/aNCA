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
 * @param {String} button_id   ID of the button to disable.
 * @param {Number} debounce    Time in miliseconds that the button should be disabled for.
 * @param {String} placeholder HTML string to show when button is disabled.
 * @param {String} ready       HTML string to show when button is ready.
 */
let disable_button_timeouts = {};
const buttonTimeout = function(button_id, debounce, placeholder, ready) {
  $(`#${button_id}`).html(placeholder).prop("disabled", true);
  clearTimeout(disable_button_timeouts[button_id]);
  disable_button_timeouts[button_id] = setTimeout(() => {
    $(`#${button_id}`).html(ready).prop("disabled", false);
  }, debounce)
}