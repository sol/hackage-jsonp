$(document).ready(function() {

  /* fetch latest packages versions, the result will be passed to
   * hackagePackageVersionsCallback(..) */
  $.getScript("http://www.typeful.net/~tbot/hackage/latest-package-versions.jsonp");
});

function hackagePackageVersionsCallback(response) {

  /* output latest version of hspec */
  var version = $(response).attr("hspec");
  $("#result").text(version);
}
