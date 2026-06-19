// Split expression into tokens
// ==============================================================================
function tokenize(text) {
  const multi = ["<=", ">=", "==", "!="];
  const single = "+-*/()^<>,|~:";
  const tokens = [];
  let i = 0;
  while (i < text.length) {
    const ch = text[i];
    if (/\s/.test(ch)) { i++; continue; }
    if (multi.includes(text.substr(i, 2))) {
      tokens.push(text.substr(i, 2));
      i += 2;
      continue;
    }
    if (single.includes(ch)) {
      tokens.push(ch);
      i++;
      continue;
    }
    let j = i;
    while (j < text.length && /[A-Za-z0-9._]/.test(text[j])) j++;
    if (j > i) {
      tokens.push(text.slice(i, j));
      i = j;
    } else {
      tokens.push(ch);
      i++;
    }
  }
  return tokens;
}

// Expression state definition
// ==============================================================================
class ExprState {
  constructor() {
    this.tokens = [];
    this.variables = [];
    this.checkVariables = true;
    this.docs = {};
    this.columnTypes = [];
    this.operatorSet = null; // supplied by R via receiveMessage
    this.specs = {};         // function arity, supplied by R
    this.error = null;
    this.status = "";
  }
  text() {
    return this.tokens.join(" ");
  }
  add(token) {
    this.tokens.push(token);
  }
  removeAt(index) {
    this.tokens.splice(index, 1);
  }
  insertAt(index, token) {
    this.tokens.splice(index, 0, token);
  }
  setText(str) {
    this.tokens = tokenize(str);
  }
}

// Methods which are added to the shiny::actionButtons
// ==============================================================================
function dragStart(event) {
  event.dataTransfer.setData(
    "text/plain",
    event.target.dataset.token
  );
}
function addToken(event) {
  const btn = event.currentTarget;
  const el = document.getElementById(btn.dataset.target);
  if (!el) return;
  const state = exprState.get(el);
  const token = btn.dataset.token;
  state.add(token);
  if (isFunction(state, token)) state.add("(");
  expressionBinding.validateAndRender(el);
}
function renderDoc(el, name, doc) {
  const box = document.getElementById(el.id + "-docs-content");
  if (!box) return;
  if (!name) return; // no token to describe: keep the last doc on screen
  box.innerHTML = "";
  const title = document.createElement("div");
  title.className = "expr-docs-title";
  title.textContent = name;
  const body = document.createElement("div");
  body.className = "expr-docs-body";
  body.textContent = doc ? doc : "No description available.";
  box.appendChild(title);
  box.appendChild(body);
}
function toggleDocs(event) {
  const panel = event.currentTarget.closest(".expr-docs");
  if (!panel) return;
  const minimized = panel.classList.toggle("minimized");
  event.currentTarget.textContent = minimized ? "+" : "–";
}
function docFor(state, name) {
  const d = state.docs ? state.docs[name] : null;
  return d ? d : null;
}
function isFunction(state, token) {
  return state.operatorSet ? within(token, state.operatorSet.function_ops) : false;
}
function isVariable(state, name) {
  return within(name, state.variables);
}
function docForVariable(state, name) {
  let counter = 0;
  for (let i = 0; i < state.variables.length; i++) {
    if (name === state.variables[i]) {
      counter = i;
      break;
    }
  }
  return "Type: " + state.columnTypes[counter];
}
function showDoc(event) {
  const btn = event.currentTarget;
  const el = document.getElementById(btn.dataset.target);
  if (!el) return;
  const state = exprState.get(el);
  if (isVariable(state, btn.dataset.token)) {
    renderDoc(el, btn.dataset.token, docForVariable(state, btn.dataset.token));
  } else {
    renderDoc(el, btn.dataset.token, docFor(state, btn.dataset.token));
  }
}
function hideDoc(event) {
  // Intentionally a no-op: the docs panel keeps showing the last hovered token.
}

// Methods defined for the dropping area
// ==============================================================================
// Index of the gap (between tokens) nearest the pointer's x position
function tokenDropIndex(el, x) {
  const spans = Array.prototype.slice.call(el.querySelectorAll(".expr-token"));
  for (let i = 0; i < spans.length; i++) {
    const rect = spans[i].getBoundingClientRect();
    if (x < rect.left + rect.width / 2) return i;
  }
  return spans.length;
}
function removeDropIndicator() {
  const ind = document.getElementById("expr-drop-indicator");
  if (ind && ind.parentNode) ind.parentNode.removeChild(ind);
}
function showDropIndicator(el, x) {
  let ind = document.getElementById("expr-drop-indicator");
  if (!ind) {
    ind = document.createElement("span");
    ind.id = "expr-drop-indicator";
    ind.className = "expr-drop-indicator";
  }
  const spans = Array.prototype.slice.call(el.querySelectorAll(".expr-token"));
  const idx = tokenDropIndex(el, x);
  if (idx >= spans.length) {
    el.appendChild(ind);
  } else {
    el.insertBefore(ind, spans[idx]);
  }
}
function allowDrop(event) {
  event.preventDefault();
  const el = event.currentTarget;
  el.classList.add("drag-over");
  showDropIndicator(el, event.clientX);
}
function dragLeave(event) {
  const el = event.currentTarget;
  if (el.contains(event.relatedTarget)) return; // still inside the zone
  el.classList.remove("drag-over");
  removeDropIndicator();
}
function dropToken(event) {
  event.preventDefault();
  const el = event.currentTarget;
  el.classList.remove("drag-over");
  const state = exprState.get(el);
  const token = event.dataTransfer.getData("text/plain");
  const sourceIndex = event.dataTransfer.getData("source-index");
  removeDropIndicator();
  let index = tokenDropIndex(el, event.clientX);
  if (sourceIndex !== "") {
    const from = Number(sourceIndex);
    if (from < index) index--; // removing the source shifts later indices left
    state.removeAt(from);
    state.insertAt(index, token);
  } else {
    state.insertAt(index, token);
    if (isFunction(state, token)) state.insertAt(index + 1, "(");
  }
  expressionBinding.validateAndRender(el);
}

// docu for the text box
// ==============================================================================
function enclosingFunction(input, fnNames) {
  const toks = tokenize(input.value.slice(0, input.selectionStart));
  const stack = [];
  for (let i = 0; i < toks.length; i++) {
    if (toks[i] === "(") {
      stack.push(within(toks[i - 1], fnNames) ? toks[i - 1] : null);
    } else if (toks[i] === ")") {
      stack.pop();
    }
  }
  for (let i = stack.length - 1; i >= 0; i--) {
    if (stack[i]) return stack[i];
  }
  return null;
}
function updateCaretDoc(el) {
  const state = exprState.get(el);
  const input = document.getElementById(el.id + "-text");
  if (!state || !input) return;
  const name = enclosingFunction(input, Object.keys(state.docs || {}));
  renderDoc(el, name, name ? docFor(state, name) : null);
}


// Autocomplete suggestions in the text box (expr-suggest)
// ==============================================================================
// The word the caret currently sits in
function currentWord(input) {
  const value = input.value;
  const caret = input.selectionStart;
  let start = caret;
  while (start > 0 && /[A-Za-z0-9._]/.test(value[start - 1])) start--;
  let end = caret;
  while (end < value.length && /[A-Za-z0-9._]/.test(value[end])) end++;
  return { start: start, end: end, text: value.slice(start, caret) };
}
// Typeahead dropdown for the text box
function setupAutocomplete(el, input, state, binding) {
  const menu = document.getElementById(el.id + "-suggest");
  if (!menu) return;
  let items = [];
  let active = -1;
  let word = null;

  function close() {
    menu.classList.remove("open");
    menu.innerHTML = "";
    items = [];
    active = -1;
  }

  function candidates() {
    const fns = state.operatorSet ? state.operatorSet.function_ops : [];
    return state.variables.concat(fns);
  }

  function renderMenu() {
    menu.innerHTML = "";
    items.forEach(function(c, i) {
      const opt = document.createElement("div");
      opt.className = "expr-suggest-item" + (i === active ? " active" : "");
      opt.textContent = c;
      opt.addEventListener("mousedown", function(e) {
        e.preventDefault(); // keep focus in the input
        accept(i);
      });
      menu.appendChild(opt);
    });
    menu.classList.add("open");
  }

  function refresh() {
    word = currentWord(input);
    const prefix = word.text;
    if (!prefix) { close(); return; }
    const lower = prefix.toLowerCase();
    const seen = {};
    items = candidates().filter(function(c) {
      if (typeof c !== "string") return false;
      if (c.toLowerCase().indexOf(lower) !== 0) return false; // prefix match
      if (c.toLowerCase() === lower) return false;            // already complete
      if (seen[c]) return false;
      seen[c] = true;
      return true;
    }).slice(0, 8);
    if (!items.length) { close(); return; }
    active = 0;
    renderMenu();
  }

  function accept(i) {
    if (i < 0 || i >= items.length) return;
    const candidate = isFunction(state, items[i]) ? items[i] + "(" : items[i];
    const before = input.value.slice(0, word.start);
    const after = input.value.slice(word.end);
    input.value = before + candidate + after;
    const pos = (before + candidate).length;
    input.setSelectionRange(pos, pos);
    close();
    state.setText(input.value);
    binding.validateAndRender(el, false);
    updateCaretDoc(el);
  }

  input.addEventListener("input", function() { refresh(); updateCaretDoc(el); });
  input.addEventListener("click", function() { updateCaretDoc(el); });
  input.addEventListener("keyup", function(e) {
    if (e.key === "ArrowLeft" || e.key === "ArrowRight" ||
        e.key === "Home" || e.key === "End") {
      updateCaretDoc(el);
    }
  });
  input.addEventListener("keydown", function(e) {
    if (!menu.classList.contains("open")) return;
    if (e.key === "ArrowDown") {
      e.preventDefault();
      active = (active + 1) % items.length;
      renderMenu();
    } else if (e.key === "ArrowUp") {
      e.preventDefault();
      active = (active - 1 + items.length) % items.length;
      renderMenu();
    } else if (e.key === "Enter" || e.key === "Tab") {
      e.preventDefault();
      accept(active);
    } else if (e.key === "Escape") {
      e.preventDefault();
      close();
    }
  });
  input.addEventListener("blur", function() { setTimeout(close, 120); });
}

// Define the binding to expr-builder so that input[["TEST-expr"]] can be used
// ==============================================================================
const exprState = new WeakMap();

const expressionBinding = new Shiny.InputBinding();
$.extend(expressionBinding, {
  find: function(scope) {
    return $(scope).find(".expr-builder");
  },
  initialize: function(el) {
    const state = new ExprState();
    exprState.set(el, state);

    const textInput = document.getElementById(el.id + "-text");
    if (textInput) {
      const self = this;
      textInput.addEventListener("input", function() {
        state.setText(textInput.value);
        self.validateAndRender(el, false);
      });
      setupAutocomplete(el, textInput, state, this);
    }

    this.render(el);
  },
  getId: function(el) {
    return el.id;
  },
  getValue: function(el) {
    const state = exprState.get(el);
    return {
      text: state.text(),
      variables: state.variables,
      tokens: state.tokens,
      error: state.error
    };
  },
  subscribe: function(el, callback) {
    $(el).on("expr-change.expressionBinding", function() {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".expressionBinding");
  },
  receiveMessage: function(el, data) {
    const state = exprState.get(el);
    if (data.variables !== undefined) {
      state.variables = data.variables;
    }
    if (data.checkVariables !== undefined) {
      state.checkVariables = data.checkVariables;
    }
    if (data.docs !== undefined) {
      state.docs = data.docs;
    }
    if (data.columnTypes !== undefined) {
      state.columnTypes = data.columnTypes;
    }
    if (data.operatorSet !== undefined) {
      state.operatorSet = data.operatorSet;
    }
    if (data.specs !== undefined) {
      state.specs = data.specs;
    }
    // Re-validate so the status reflects the new variables/flag, not just the next token edit.
    if (state.tokens.length) {
      this.validateAndRender(el);
    } else {
      this.render(el);
      $(el).trigger("expr-change");
    }
  },
  render: function(el) {
    const state = exprState.get(el);
    el.innerHTML = "";
    state.tokens.forEach((token, index) => {
      const span = document.createElement("span");
      span.textContent = token;
      span.classList.add("expr-token");
      span.draggable = true;
      span.dataset.index = index;
      span.addEventListener("dragstart", function(event) {
        event.dataTransfer.setData("text/plain", token);
        event.dataTransfer.setData("source-index", index);
        span.classList.add("dragging");
      });
      span.addEventListener("dragend", function() {
        span.classList.remove("dragging");
      });
      el.appendChild(span);
    });
  },
  validateAndRender: function(el, updateText) {
    if (updateText === undefined) updateText = true;
    const state = exprState.get(el);
    if (!state.operatorSet) { this.render(el); return; } // wait for R to supply the operator set
    try {
      const res = parse(state.tokens, state.variables, state.operatorSet, state.specs, state.checkVariables);
      const flat = res.to_flat_string();
      state.status = (typeof flat === "string") ? flat : flat.to_flat_string();
      state.error = null;
    } catch (err) {
      state.error = err.message;
      state.status = "ParseError(" + err.message + ")";
    }

    const statusEl = document.getElementById(el.id + "-status");
    if (statusEl) {
      statusEl.textContent = "Status: " + state.status;
      statusEl.classList.remove("status-ok", "status-error", "status-empty");
      if (!state.tokens.length || state.status === "") {
        statusEl.classList.add("status-empty");
      } else if (state.error !== null || state.status.includes("ParseError")) {
        statusEl.classList.add("status-error");
      } else {
        statusEl.textContent = "Status: OK";
        statusEl.classList.add("status-ok");
      }
    }

    this.render(el);
    const textEl = document.getElementById(el.id + "-text");
    if (textEl && updateText) {
      textEl.value = state.text();
    }
    $(el).trigger("expr-change");
  }
});

Shiny.inputBindings.register(expressionBinding);
