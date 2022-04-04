(function() {
   $(document).on("click", ".conrapp-popover", function() {
     $(this).popover({sanitize: false, html: true, trigger: "focus"});
     $(this).popover("show");
   });
})();
