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

/**
 * Enable drag-and-drop file upload on a custom container (e.g., a div wrapping a file input).
 * 
 * This script listens for drag-and-drop events on a container element and
 * redirects any dropped files to the corresponding file input element, so
 * Shiny can process the files as if they were uploaded normally.
 * 
 * Requirements:
 * - The container must have a known ID (e.g., "data-raw_data-upload_container'").
 * - Inside the container, there must be an <input type="file"> element (Shiny fileInput).
 * - CSS class 'dragover' can be used to style the container during drag.
 */
const enableDragAndDropUpload = function(element_id) {
  const container = document.getElementById(element_id);
  if (!container) return;
  const fileInput = container.querySelector('input[type="file"]');
  if (!fileInput) return;
  container.addEventListener('dragover', function (e) {
    e.preventDefault();
    container.classList.add('dragover');
  });
  container.addEventListener('dragleave', function (e) {
    e.preventDefault();
    container.classList.remove('dragover');
  });
  container.addEventListener('drop', function (e) {
    e.preventDefault();
    container.classList.remove('dragover');
    if (e.dataTransfer.files.length > 0) {
      fileInput.files = e.dataTransfer.files;
      fileInput.dispatchEvent(new Event('change'));
    }
  });
}
document.addEventListener('DOMContentLoaded', function () {
  enableDragAndDropUpload('data-raw_data-upload_container');
});
