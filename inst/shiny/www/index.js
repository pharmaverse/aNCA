/**
 * Enables bootstrap tooltips for the whole application.
 */
$(document).ready(() => {
  $('[data-toggle="tooltip"]').tooltip();

  const results_table = document.getElementById('nca-nca_results-myresults-table');
  observe_visible(results_table, "nca-results_visible");
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
};

/**
 * Shows overlay div by ID
 */
Shiny.addCustomMessageHandler("showOverlay", function(message) {
  const el = document.getElementById(message.id);
  if (el) {
    el.style.display = 'block';
  }
});

/**
 * Hides overlay div by ID
 */
Shiny.addCustomMessageHandler("hideOverlay", function(message) {
  const el = document.getElementById(message.id);
  if (el) {
    el.style.display = 'none';
  }
});

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
  initSidebarResize();
});

/**
 * Attaches a drag handle to a single sidebar element for resizing.
 * Updates the bslib grid's --_sidebar-width CSS variable on drag.
 */
function attachResizeHandle(sidebar) {
  if (sidebar.querySelector('.sidebar-resize-handle')) return;

  var handle = document.createElement('div');
  handle.className = 'sidebar-resize-handle';
  sidebar.prepend(handle);

  var layout = sidebar.closest('.sidebar-right');

  handle.addEventListener('mousedown', function(e) {
    e.preventDefault();
    handle.classList.add('dragging');
    var startX = e.clientX;
    var startWidth = sidebar.getBoundingClientRect().width;

    function onMouseMove(e) {
      var newWidth = startWidth + (startX - e.clientX);
      if (newWidth >= 150 && newWidth <= 600) {
        layout.style.setProperty('--_sidebar-width', newWidth + 'px', 'important');
      }
    }

    function onMouseUp() {
      handle.classList.remove('dragging');
      document.removeEventListener('mousemove', onMouseMove);
      document.removeEventListener('mouseup', onMouseUp);
    }

    document.addEventListener('mousemove', onMouseMove);
    document.addEventListener('mouseup', onMouseUp);
  });
}

/**
 * Initializes drag-to-resize on all right-side sidebars, including
 * those rendered dynamically by Shiny (e.g., TLG tab content).
 */
function initSidebarResize() {
  document.querySelectorAll('.sidebar-right > .sidebar').forEach(attachResizeHandle);

  new MutationObserver(function(mutations) {
    mutations.forEach(function(m) {
      m.addedNodes.forEach(function(node) {
        if (node.nodeType !== 1) return;
        if (node.matches && node.matches('.sidebar-right > .sidebar')) {
          attachResizeHandle(node);
        }
        if (node.querySelectorAll) {
          node.querySelectorAll('.sidebar-right > .sidebar').forEach(attachResizeHandle);
        }
      });
    });
  }).observe(document.body, { childList: true, subtree: true });
}

/**
 * Creates a custom observer that checks if particular element is visible in the viewport.
 * If it is, it sets a Shiny input value to a random number (to trigger reactivity).
 * @param {Array} elements List of observed elements.
 * @param {String} inputid Name of the Shiny input to set when element is visible.
 */
observe_visible = function(element, input_id) {
  const observer = new IntersectionObserver(function(els) {
    els.forEach(function(el) {
      if (el.isIntersecting) {
        Shiny.setInputValue(input_id, Math.random(), {priority: "event"});
      }
    });
  }, {
    threshold: 0.1
  });

  observer.observe(element);
}
