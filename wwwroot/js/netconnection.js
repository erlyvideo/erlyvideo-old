netconnection = {};
netconnection.initialized = function(server) {
	if (console) {
		console.log("Connecting to rtmp server "+server);
	}
  
}

netconnection.connected = function() {
	if (console) {
		console.log("Connected");
	}
}

netconnection.failed = function() {
	if (console) {
		console.log("Disconnected");
	}
}

netconnection.message = function(message) {
  $("#chat").append("<div>"+message+"</div>");
}
