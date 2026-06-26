Shiny.addCustomMessageHandler("renderPlot", function (msg) {
  const spec = buildSpec(msg, msg.data);
  vegaEmbed("#vega-plot", spec, { actions: false })
    .then(function(result) { console.log("vegaEmbed ok", result); })
    .catch(function(err)   { console.error("vegaEmbed error", err); });
});

function buildSpec(msg, data) {
  const encoding = buildEncoding(msg);
  const params   = msg.zoom
    ? [{ name: "grid", select: "interval", bind: "scales" }]
    : [];

  const innerSpec = buildInner(msg, encoding, params);

  if (msg.facetVar) {
    return {
      $schema: "https://vega.github.io/schema/vega-lite/v5.json",
      data: { values: data },
      facet: { field: msg.facetVar, type: "nominal", columns: 2 },
      spec: { width: 250, height: 300, ...innerSpec },
    };
  }

  return {
    $schema: "https://vega.github.io/schema/vega-lite/v5.json",
    width: 600,
    height: 600,
    data: { values: data },
    ...innerSpec,
  };
}

function buildEncoding(msg) {
  const enc = {
    x: { field: msg.x, type: msg.xType },
  };
  if (msg.y)        enc.y     = { field: msg.y,        type: msg.yType };
  if (msg.colorVar) enc.color = { field: msg.colorVar, type: msg.colorType };
  return enc;
}

function buildInner(msg, encoding, params) {
  switch (msg.type) {
    case "scatter":
      return {
        mark: { type: "point", tooltip: true },
        encoding,
        params,
      };

    case "boxplot":
      if (msg.overlay) {
        // params omitted: boxplot is a composite mark internally and causes
        // "Duplicate signal name" when combined with a selection in a layer
        return {
          encoding,
          layer: [
            { mark: { type: "boxplot", extent: "min-max" } },
            { mark: { type: "point",   opacity: 0.6, color: "black" } },
          ],
        };
      }
      return {
        mark: { type: "boxplot", extent: "min-max", tooltip: true },
        encoding,
        params,
      };

    case "line":
      return {
        mark: { type: "line", tooltip: true },
        encoding,
        params,
      };

    case "histogram":
      return {
        mark: { type: "bar", tooltip: true },
        encoding: {
          x: { field: msg.x, bin: true, type: "quantitative" },
          y: { aggregate: "count", type: "quantitative" },
        },
        params,
      };
  }
}
