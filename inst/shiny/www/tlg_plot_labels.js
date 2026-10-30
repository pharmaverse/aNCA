function(el, x, labels) {
  // Reflow annotations at the chart's actual width, including after a sidebar resize.
  var canvas = document.createElement('canvas');
  var context = canvas.getContext('2d');
  var previousWidth;
  var previousHeight;
  var captionOffset = labels.captionOffset;
  var busy = false;
  var escape = function(text) {
    return text.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  };
  function wrap(text, size, width) {
    context.font = size + 'px ' + el._fullLayout.font.family;
    var lines = [];
    text.split('\n').forEach(function(line) {
      var current = '';
      line.split(/\s+/).forEach(function(word) {
        if (current && context.measureText(current + ' ' + word).width > width) {
          lines.push(current);
          current = '';
        }
        // Also keep an unbroken identifier or unit inside the chart boundary.
        Array.from(word).forEach(function(letter) {
          if (context.measureText(current + letter).width > width) {
            lines.push(current);
            current = '';
          }
          current += letter;
        });
        current += ' ';
      });
      lines.push(current.trimEnd());
    });
    return lines.map(escape);
  }
  function reflow() {
    if (busy || !el._fullLayout) return;
    var chartWidth = el.getBoundingClientRect().width;
    var width = Math.floor(chartWidth - el._fullLayout._size.l - el._fullLayout._size.r) - 8;
    if (width < 40 || (width === previousWidth &&
        Math.abs(el.getBoundingClientRect().height - previousHeight) < 1 &&
        Math.abs(el._fullLayout.height - previousHeight) < 1 &&
        Math.abs(el._fullLayout.width - chartWidth) < 1)) return;
    previousWidth = width;
    var sizes = [18, 12, 11];
    var heights = [24, 17, 16];
    var text = labels.text.map(function(value, i) {
      return value ? wrap(value, sizes[i], width) : [];
    });
    var top = text[0].length * heights[0] + text[1].length * heights[1];
    var bottom = text[2].length * heights[2];
    if (labels.measureBottom && bottom) {
      // Existing widgets already know their tick and legend geometry. Measure it
      // in the browser instead of rebuilding the data through ggplot in R.
      var chartTop = el.getBoundingClientRect().top;
      var panelBottom = chartTop + el._fullLayout._size.t + el._fullLayout._size.h;
      el.querySelectorAll('.legend, .xtitle, .xtick').forEach(function(node) {
        captionOffset = Math.max(captionOffset, node.getBoundingClientRect().bottom - panelBottom);
      });
    }
    var extraTop = top ? top + 12 : 0;
    var extraBottom = bottom ? bottom + 12 + captionOffset - labels.bottom : 0;
    var height = 500 + extraTop + extraBottom;
    previousHeight = height;
    var update = {
      'margin.t': labels.top + extraTop,
      'margin.b': labels.bottom + extraBottom,
      width: chartWidth,
      height: height
    };
    var shifts = [labels.top + text[1].length * heights[1] + 6,
                  labels.top, -captionOffset - 6];
    el._fullLayout.annotations.forEach(function(annotation, index) {
      var i = ['tlg-title', 'tlg-subtitle', 'tlg-caption'].indexOf(annotation.name);
      if (i < 0) return;
      update['annotations[' + index + '].text'] = text[i].join('<br>');
      update['annotations[' + index + '].yshift'] = shifts[i];
    });
    busy = true;
    el.style.height = height + 'px';
    Plotly.relayout(el, update).then(function() { busy = false; });
  }
  if (el._tlgLabelReflow) el.removeListener('plotly_afterplot', el._tlgLabelReflow);
  if (el._tlgLabelObserver) el._tlgLabelObserver.disconnect();
  el._tlgLabelReflow = reflow;
  el.on('plotly_afterplot', reflow);
  el._tlgLabelObserver = new ResizeObserver(function() {
    requestAnimationFrame(reflow);
  });
  el._tlgLabelObserver.observe(el);
  reflow();
}
