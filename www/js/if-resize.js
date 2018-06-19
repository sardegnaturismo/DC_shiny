(function () {
  "use strict";

  /**
   * Questo file non viene usato nel tema, ma sulle dashboard RAS incluse in osservatorio
   * per mezzo di iframe. Viene usato per la comunicazione tra osservatorio e la dashboard
   * per il ridimenzionamento del iframe secondo le misure della dashboard stessa. 
   */

  window.addEventListener("load", function(){
    function getDocHeight(doc) {
        doc = doc || document;
        // from http://stackoverflow.com/questions/1145850/get-height-of-entire-document-with-javascript
        var body = doc.body, html = doc.documentElement;
        var height = Math.max( body.scrollHeight, body.offsetHeight,
            html.clientHeight, html.scrollHeight, html.offsetHeight );
        return height;
      }

      function sendDocHeightMsg(e) {
        var ht = getDocHeight();
        parent.postMessage( JSON.stringify( {"docHeight": ht} ), "*" );
        e.stopPropagation();
      }

      if ( window.addEventListener ) {
        window.addEventListener("message", sendDocHeightMsg, false);
        window.addEventListener("resize", sendDocHeightMsg, true);
      } else if ( window.attachEvent ) { // ie8
        window.attachEvent("message", sendDocHeightMsg);
        window.attachEvent("resize", sendDocHeightMsg);
      }
  }, false);

})();