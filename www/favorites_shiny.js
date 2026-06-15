// Bridge localStorage favorites ↔ Shiny

(function() {
  if (typeof Shiny === "undefined") return;

  Shiny.addCustomMessageHandler("favorites_add", function(msg) {
    favAdd(msg);
    Shiny.setInputValue("favorites_json_from_browser", JSON.stringify(favReadAll()), {priority: "event"});
  });

  Shiny.addCustomMessageHandler("favorites_remove", function(msg) {
    if (msg && msg.id) favRemove(msg.id);
    Shiny.setInputValue("favorites_json_from_browser", JSON.stringify(favReadAll()), {priority: "event"});
  });

  Shiny.addCustomMessageHandler("favorites_request_list", function(msg) {
    Shiny.setInputValue("favorites_json_from_browser", JSON.stringify(favReadAll()), {priority: "event"});
  });

  Shiny.addCustomMessageHandler("favorites_import", function(msg) {
    if (msg && msg.json) {
      favImportJson(msg.json);
      Shiny.setInputValue("favorites_json_from_browser", JSON.stringify(favReadAll()), {priority: "event"});
    }
  });

  $(document).on("shiny:connected", function() {
    Shiny.setInputValue("favorites_json_from_browser", JSON.stringify(favReadAll()), {priority: "event"});
  });
})();
