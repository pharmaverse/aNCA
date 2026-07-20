// Client-side behaviour for the "Add TLGs to order" modal (see
// inst/shiny/functions/tlg_add_picker.R).  The modal shows dataset tabs
// (PK Concentrations / PK Parameters) over one column per output Type; this
// object drives search, tab switching, and the select-all helpers.  The
// per-open initialisation (active tab + first render) is injected inline by the
// R helper because it depends on the datasets present in the current modal.
window.tlgAdd = {
  q: "",
  tab: null,

  // visibleOnly: only toggle rows shown under the active tab / search, so
  // Select-all respects the current dataset.  Clear-all passes false to wipe
  // every dataset.
  _setChecked: function(scope, checked, visibleOnly) {
    var groups = {};
    scope.querySelectorAll(".checkbox").forEach(function(row) {
      if (visibleOnly && row.style.display === "none") return;
      var cb = row.querySelector("input[type=checkbox]");
      if (!cb) return;
      cb.checked = checked;
      var g = cb.closest(".shiny-input-checkboxgroup");
      if (g) groups[g.id] = g;
    });
    Object.keys(groups).forEach(function(id) {
      var inp = groups[id].querySelector("input[type=checkbox]");
      if (inp) inp.dispatchEvent(new Event("change", { bubbles: true }));
    });
  },

  selectAll: function() {
    var r = document.querySelector(".tlg-add-checklist");
    if (r) this._setChecked(r, true, true);
  },

  clearAll: function() {
    var r = document.querySelector(".tlg-add-checklist");
    if (r) this._setChecked(r, false, false);
  },

  colSelect: function(btn) {
    var c = btn.closest(".tlg-col");
    if (c) this._setChecked(c, true, true);
  },

  setQuery: function(v) {
    this.q = v || "";
    this.render();
  },

  setTab: function(v, btn) {
    this.tab = v;
    document.querySelectorAll(".tlg-tabs .tlg-tab").forEach(function(b) {
      b.classList.remove("active");
    });
    if (btn) btn.classList.add("active");
    this.render();
  },

  render: function() {
    var root = document.querySelector(".tlg-add-checklist");
    if (!root) return;
    var q = this.q.trim().toLowerCase();
    var tab = this.tab;
    // Per-dataset text-match counts are computed independently of the active
    // tab (they drive the tab badges), but only the active tab's matches are
    // shown.  This way searching in a term that only matches another dataset
    // surfaces the count on that tab's badge -- clicking it reveals the match
    // -- instead of silently showing another dataset's rows under this tab.
    var dsMatch = {};
    var totalMatches = 0;
    root.querySelectorAll(".tlg-ds").forEach(function(ds) {
      var name = ds.getAttribute("data-dataset");
      var isActive = name === tab;
      var anyVisible = false;
      ds.querySelectorAll(".checkbox").forEach(function(it) {
        var textMatch = q === "" || it.textContent.toLowerCase().indexOf(q) > -1;
        if (textMatch) {
          dsMatch[name] = (dsMatch[name] || 0) + 1;
          totalMatches++;
        }
        var show = isActive && textMatch;
        it.style.display = show ? "" : "none";
        if (show) anyVisible = true;
      });
      ds.style.display = anyVisible ? "" : "none";
    });
    // Per-column: visible count, empty state, hide select-all when empty.
    root.querySelectorAll(".tlg-col").forEach(function(col) {
      var vis = 0;
      col.querySelectorAll(".checkbox").forEach(function(it) {
        if (it.style.display !== "none") vis++;
      });
      var cnt = col.querySelector(".tlg-col-count");
      if (cnt) cnt.textContent = vis;
      var empty = col.querySelector(".tlg-col-empty");
      if (empty) empty.style.display = vis ? "none" : "";
      var sa = col.querySelector(".tlg-col-selall");
      if (sa) sa.style.display = vis ? "" : "none";
    });
    // Tab badges: per-dataset match count while searching, dataset total otherwise.
    document.querySelectorAll(".tlg-tabs .tlg-tab").forEach(function(t) {
      var badge = t.querySelector(".tlg-tab-count");
      if (!badge) return;
      badge.textContent = q === ""
        ? t.getAttribute("data-total")
        : (dsMatch[t.getAttribute("data-dataset")] || 0);
    });
    // Global "no matches" message only when nothing matches in any dataset.
    var nm = document.querySelector(".tlg-no-matches");
    if (nm) nm.style.display = totalMatches === 0 ? "" : "none";
  }
};
