// Highlights a click path of the teach mode step by step.
// A CSS rule instead of a class on the element: many elements are created by
// renderUI and would lose the class, or do not exist yet when the path starts
(function() {
  let steps = [];
  let styleEl = null;

  function show() {
    while (steps.length && steps[0].done && document.querySelector(steps[0].done)) {
      steps.shift();
    }
    if (!styleEl) {
      styleEl = document.createElement("style");
      document.head.appendChild(styleEl);
    }
    styleEl.textContent = steps.length ?
      "@keyframes openstats-highlight { 50% { outline-color: transparent; } }\n" +
      steps[0].selector + " { outline: 3px solid #f0ad4e; outline-offset: 3px; " +
      "animation: openstats-highlight 1.2s infinite; }" :
      "";
  }

  document.addEventListener("keydown", function(e) {
    if (e.key === "Escape" && steps.length) {
      steps = [];
      show();
    }
  });

  document.addEventListener("click", function(e) {
    if (!steps.length) return;
    if (e.target.closest(steps[0].selector)) steps.shift();
    // Also after other clicks: a step can be done in another way (e.g. modal closed via the backdrop).
    // The next element (e.g. inside the modal) needs a moment to appear
    setTimeout(show, 400);
  }, true);

  $(document).on("shiny:connected", function() {
    Shiny.addCustomMessageHandler("highlight_path", function(msg) {
      steps = msg.steps || [];
      show();
    });
  });
})();
