(function() {
   $(document).on("click", ".conrapp-popover", function() {
     $(this).popover({sanitize: false, html: true, trigger: "focus"});
     $(this).popover("show");
   });
   LeafletWidget.methods.addZoom = function(options) {
    (function(){
      L.control.zoom({ position: options.position }).addTo(this);
    }).call(this);
  };
})();
