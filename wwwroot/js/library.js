function parseQueryString() {
  var re1 = /^([^?]+)\?(.+)/
  var urlHalves;
  urlHalves = re1.exec(String(document.location));
  if(!urlHalves) {
    return {};
  }
  urlHalves = urlHalves[2];
  var params = {};
  var i;
  
  var re = /^([^=]+)=(.*)$/;
  
  var urlVars = urlHalves.split('&');
  for(i=0; i< urlVars.length; i++){
    var kv = re.exec(urlVars[i]);
    params[kv[1]] = kv[2];
  }
  return params;
}

