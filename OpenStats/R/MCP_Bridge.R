# MCP spike: an external MCP client (Claude Desktop/Code) runs the LLM tools
# in a running app.
#   MCP client <-stdio-> MCP server process <-nanonext req/rep-> app session
# The server process only knows the tool definitions; every call is executed
# in the app so that results show up there.
# ----------------------------------------------------------
mcp_address_V1_2 <- function() {
  getOption("OpenStats.mcp_address", "ipc:///tmp/openstats-mcp")
}

# App side
# ----------------------------------------------------------
mcp_handle_call_V1_2 <- function(msg, DataModelState, ResultsState, reply) {
  fail <- function(e) reply(list(ok = FALSE, value = conditionMessage(e)))
  started <- tryCatch(
    list(ok = TRUE, value = llm_run_tool_V1_2(msg$name, msg$args, DataModelState, ResultsState)),
    error = function(e) list(ok = FALSE, value = conditionMessage(e))
  )
  if (!started$ok) {
    reply(started)
    return(promises::promise_resolve(NULL))
  }
  # Background analyses return a promise, the others plain text
  promises::then(
    promises::promise_resolve(started$value),
    onFulfilled = function(x) reply(list(ok = TRUE, value = x)),
    onRejected = fail
  )
}

McpBridgeServer <- function(DataModelState, ResultsState) {
  if (!isTRUE(getOption("OpenStats.mcp"))) {
    return()
  }
  session <- shiny::getDefaultReactiveDomain()
  # One rep socket per address: a second browser tab cannot listen
  sock <- tryCatch(
    nanonext::socket("rep", listen = mcp_address_V1_2()),
    error = function(e) NULL
  )
  if (is.null(sock)) {
    print_warn("MCP: another session is already connected")
    return()
  }
  active <- TRUE
  session$onSessionEnded(function() {
    active <<- FALSE
    close(sock)
  })
  reply <- function(x) {
    nanonext::send(sock, x, mode = "serial", block = 1000)
  }
  listen <- function() {
    if (!active) {
      return()
    }
    aio <- nanonext::recv_aio(sock, mode = "serial")
    # Polling via later instead of blocking: the Shiny loop has to keep running
    poll <- function() {
      if (!active) {
        return()
      }
      if (nanonext::unresolved(aio)) {
        return(later::later(poll, 0.1))
      }
      msg <- aio$data
      if (nanonext::is_error_value(msg)) {
        return(listen())
      }
      # later callbacks have no default reactive domain; shinyjs and
      # session$sendCustomMessage in the handlers need it
      shiny::withReactiveDomain(session, {
        promises::finally(
          mcp_handle_call_V1_2(msg, DataModelState, ResultsState, reply),
          listen
        )
      })
    }
    poll()
  }
  listen()
}

# MCP server side (separate process, started by the MCP client)
# ----------------------------------------------------------
mcp_server_tools_V1_2 <- function(timeout = 600000) {
  sock <- nanonext::socket("req", dial = mcp_address_V1_2())
  run <- function(name, args) {
    if (nanonext::stat(sock, "pipes") == 0) {
      stop("OpenStats is not running. Start the app with options(OpenStats.mcp = TRUE).")
    }
    nanonext::send(sock, list(name = name, args = args), mode = "serial", block = 1000)
    res <- nanonext::recv(sock, mode = "serial", block = timeout)
    if (nanonext::is_error_value(res)) {
      stop("No answer from OpenStats: ", res)
    }
    if (!res$ok) {
      stop(res$value)
    }
    res$value
  }
  tools <- Map(
    function(n, cls) llm_make_tool_V1_2(n, cls, run),
    names(llm_tool_classes_V1_2),
    llm_tool_classes_V1_2
  )
  unname(tools)
}

run_mcp_server_V1_2 <- function() {
  mcptools::mcp_server(tools = mcp_server_tools_V1_2(), session_tools = FALSE)
}
