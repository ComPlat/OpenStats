Shiny.addCustomMessageHandler("drawGraph", function(message) {

  const nodes = new vis.DataSet(message.nodes);
  const edges = new vis.DataSet(message.edges);

  const container = document.getElementById('graph');

  const data = {
    nodes: nodes,
    edges: edges
  };

  const options = {
    interaction: { hover: true },
    layout: {
      hierarchical: {
        direction: "UD",
        sortMethod: "directed"
      }
    },
    nodes: {
      shape: 'box',
      margin: 10,
      font: { size: 14 }
    },
    edges: {
      arrows: 'to'
    },
    physics: false
  };

  const network = new vis.Network(container, data, options);

  network.on("click", function(params) {
    if (params.nodes.length > 0) {
      const clickedNodeId = params.nodes[0];
      Shiny.setInputValue("node_clicked", clickedNodeId, {priority: "event"});
    }
  });
});

